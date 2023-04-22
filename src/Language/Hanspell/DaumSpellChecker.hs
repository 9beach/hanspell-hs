{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Defines the interfaces for DAUM spell check service.
module Language.Hanspell.DaumSpellChecker
    ( DaumSpellChecker
    , spellCheckByDaum
    , daumSpellCheckerMaxChars
    ) where

import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU

import Data.List
import Data.List.Split
import Network.HTTP.Types.Status
import Text.Regex
import Debug.Trace
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Language.Hanspell.Typo
import Language.Hanspell.Decoder

-- | Defines a class for 'spellCheckByDaum' function overloading.
class Monad m => DaumSpellChecker m where

    -- | Requests spell check to DAUM server, parses the responses,
    -- and returns @m [Typo]@. @spellCheckByDaum@ has two return types.
    -- One is @MaybeT IO [Typo]@, and the other is @IO [Typo]@. 
    --
    -- @
    -- import Language.Hanspell 
    --
    -- example = do
    --     let sentence = "위에계신분, 잘들리세요?"
    --     typos <- spellCheckByDaum sentence
    --     mapM_ (putStrLn . typoToStringWithStyle False) typos
    -- @
    --
    -- The expected output is:
    --
    -- @
    -- 위에계신분, -> 위에 계신 분,
    -- 뒤에 오는 명사를 수식하는 관형격 어미 ‘-ㄴ’, ‘-는’, ‘-던’, ‘-ㄹ’ 등과 의존명사는 띄어 쓰는 것이 옳습니다.
    -- (예)
    -- 노력한 만큼 대가를 얻다.
    -- 소문으로만 들었을 뿐이네.
    -- 합격했다는 소리를 들으니 그저 기쁠 따름이다.
    -- 
    -- 잘들리세요? -> 잘 들리세요?
    -- '익숙하고 능란하게', '좋고 훌륭하게'라는 의미의 부사 '잘'은 띄어 쓰세요.
    -- (예)
    -- 바둑을 잘 두다.
    -- 옷을 잘 차려입고 나서니 딴사람 같구나.
    -- 다음 대화를 잘 듣고 물음에 답하세요.
    -- @ 
    spellCheckByDaum :: String -> m [Typo]

-- | Obsessive version returning @MaybeT IO [Typo]@.
instance DaumSpellChecker (MaybeT IO) where
    spellCheckByDaum text = htmlToTypos <$> requestToDaum text

-- | Bold version returning @IO [Typo]@.
instance DaumSpellChecker IO where
    spellCheckByDaum text = do
        maybe <- runMaybeT $ htmlToTypos <$> requestToDaum text
        case maybe of 
            Nothing -> return []
            Just typos -> return typos

-- | Official maximum character length for a 'spellCheckByDaum' request.
-- Notice that DAUM server can handle more than maximum characters.
daumSpellCheckerMaxChars :: Int
daumSpellCheckerMaxChars = 1000
 
-- 'spellCheckByDaum' prints the message below when the HTTP status code is 
-- not 200. Mainly due to timeout.
daumConnectError :: String
daumConnectError = 
     "-- 한스펠 오류: 다음 서버의 접속 오류로 일부 문장 교정에 실패했습니다."

-- 'spellCheckByDaum' prints the message below when the response is not of 
-- spell checker. Mainly due to the changes of service URL.
invalidResponseFromDaum :: String
invalidResponseFromDaum = 
     "-- 한스펠 오류: 다음 서비스가 유효하지 않은 양식을 반환했습니다. (" 
     ++ daumSpellCheckUrl ++ ")" 

-- Daum spell checker URL.
--
-- Try `curl -H "Content-Type: application/x-www-form-urlencoded" \
-- -X POST https://dic.daum.net/grammar_checker.do -d \
-- "sentence=안녕 하세요"`.
daumSpellCheckUrl :: String
daumSpellCheckUrl = "https://dic.daum.net/grammar_checker.do"

-- Requests spell check to the server, check the responses, and returns it. 
-- When the status code is not 200, or the response is not of spell checker, 
-- traces error message, and returns Nothing.
requestToDaum :: String -> MaybeT IO String
requestToDaum text = do
    manager <- lift $ newManager tlsManagerSettings
    let pair = [("sentence",BU.fromString text)]
    initialRequest <- lift $ parseRequest daumSpellCheckUrl
    let request = (urlEncodedBody pair initialRequest) { method = "POST" }
    response <- lift $ httpLbs request manager
    let errCode = statusCode (responseStatus response)
    let daumResponseInfix = "=\"screen_out\">맞춤법 검사기 본문</h2>"
    if errCode == 200
       then let body = BLU.toString (responseBody response)
             in if daumResponseInfix `isInfixOf` body
                   then return body
                   else trace invalidResponseFromDaum (MaybeT $ return Nothing)
       else trace (daumConnectError ++ " ("++ show errCode ++ ")")
            (MaybeT $ return Nothing)

-- Parses the response HTML to [Typo]
htmlToTypos :: String -> [Typo]
htmlToTypos body =
        -- Removes unuseds from body
    let stripped = head . splitOn "<span class=\"info_byte\">" $ body
        -- Splits the body for each typo
        splitted = tail . splitOn "data-error-type" $ stripped
     in map htmlToTypo splitted

-- Parse a unit of response to Typo
htmlToTypo :: String -> Typo
htmlToTypo body = Typo { errorType    =  decodeEntity (splitted!!1)
                       , token        =  decodeEntity (splitted!!3)
                       , suggestions  = [decodeEntity (splitted!!5)]
                       , context      =  decodeEntity (splitted!!7)
                       , info         =  info''''
                       } where
    gsub from to text = subRegex (mkRegex from) text to
    splitted = splitOn "\"" $ head (lines body)
    info' = splitOn "<div>" body!!1
    info'' = head (splitOn "<span class=\"info_byte\">" info')
    info''' = gsub "^[ \n][ \n]*" ""
            . gsub "<[^>]*>" ""
            . gsub "<br[^>]*>" "\n"
            . gsub "</span><span class.*\n" ""
            . gsub "<a href=\"#none\".*\n" ""
            . gsub "^<span>.*\n" ""
            . gsub "<strong class.*\n" ""
            . gsub ".*strong class=.tit_help.>예문</strong.*\n" "(예)"
            . gsub "\t" ""
            $ info''
    info'''' = if decodeEntity info''' == "도움말이 없습니다.\n"
                  then ""
                  else info'''
