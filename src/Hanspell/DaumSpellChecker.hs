{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Hanspell.DaumSpellChecker
    ( spellCheckByDaum
    , daumSpellCheckerMaxChars
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Text.Regex
import Debug.Trace
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import Hanspell.Typo
import Hanspell.Decoder

-- | 'spellCheckByDaum' has two return types. One is 'MaybeT IO [Typo]',
-- and the other is 'IO [Typo]'. I'd prefer the latter.
class Monad m => DaumSpellChecker m where
    -- | Requests spell check to the server, parses the responses, 
    -- and returns m [Typo]
    spellCheckByDaum :: T.Text -> m [Typo]

-- | Obssesive version.
instance DaumSpellChecker (MaybeT IO) where
    -- spellCheckByDaum :: T.Text -> MaybeT IO [Typo]
    spellCheckByDaum text = requestToDaum text >>= return . htmlToTypos

-- | Bold version.
instance DaumSpellChecker IO where
    -- spellCheckByDaum :: T.Text -> IO [Typo]
    spellCheckByDaum text = do
        maybe <- runMaybeT $ requestToDaum text >>= return . htmlToTypos
        case maybe of 
            Nothing -> return []
            Just typos -> return typos

-- | Maximum character length for one spell check request. Notice that DAUM
-- server can handle more than maximum characters.
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
        "-- 한스펠 오류: 접속한 다음 서비스는 맞춤법 검사 서비스가 아닙니다."

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
requestToDaum :: T.Text -> MaybeT IO T.Text
requestToDaum text = do
    manager <- lift $ newManager tlsManagerSettings
    let pair = [("sentence",TE.encodeUtf8 text)]
    initialRequest <- lift $ parseRequest daumSpellCheckUrl
    let request = (urlEncodedBody pair initialRequest) { method = "POST" }
    response <- lift $ httpLbs request manager
    let errCode = statusCode (responseStatus response)
        daumResponseInfix = 
                T.pack "<h2 class=\"screen_out\">맞춤법 검사기 본문</h2>"
     in if (errCode == 200)
           then let body = TE.decodeUtf8 . BL.toStrict . responseBody $ response
                 in if T.isInfixOf daumResponseInfix body
                       then return body
                       else trace invalidResponseFromDaum 
                           (MaybeT $ return Nothing)
           else trace (daumConnectError ++ " ("++ show errCode ++ ")")
               (MaybeT $ return Nothing)

-- Parses the response HTML to [Typo]
htmlToTypos :: T.Text -> [Typo]
htmlToTypos body =
        -- removes unused from body
    let stripped = head . T.splitOn (T.pack "<span class=\"info_byte\">") $ body
        -- splits the body to each typo
        splitted = tail . T.splitOn (T.pack "data-error-type") $ stripped
     in map htmlToTypo splitted

-- Parse a unit of response to Typo
htmlToTypo :: T.Text -> Typo
htmlToTypo body =
    let gsub from to text = subRegex (mkRegex from) text to
        splitted = T.split (=='"') $ head (T.lines body)
        info' = (T.splitOn (T.pack "<div>") body)!!1
        info'' = (T.splitOn (T.pack
            "<span class=\"info_byte\">") info')!!0
        info''' = T.pack
                . gsub "^[ \n][ \n]*" ""
                . gsub "<[^>]*>" ""
                . gsub "<br[^>]*>" "\n"
                . gsub "</span><span class.*\n" ""
                . gsub "<a href=\"#none\".*\n" ""
                . gsub "^<span>.*\n" ""
                . gsub "<strong class.*\n" ""
                . gsub ".*strong class=.tit_help.>예문</strong.*\n" "(예)"
                . gsub "\t" ""
                $ T.unpack info''
     in Typo { errorType    =  decodeEntity (splitted!!1)
             , token        =  decodeEntity (splitted!!3)
             , suggestions  = [decodeEntity (splitted!!5)]
             , context      =  decodeEntity (splitted!!7)
             , info         =  info'''
             }
