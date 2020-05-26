{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

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
    -- main = do
    --     let sentence = "위에계신분, 잘들리세요?"
    --     let correctSentence = "위에 계신 분, 잘 들리세요?"
    --     typos <- spellCheckByDaum sentence
    --     let fixedSentence = fixTyposWithStyle False sentence typos
    --     putStrLn . show $ fixedSentence == correctSentence
    -- @
    spellCheckByDaum :: String -> m [Typo]

-- | Obssesive version returning @MaybeT IO [Typo]@.
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
                       , info         =  info'''
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
