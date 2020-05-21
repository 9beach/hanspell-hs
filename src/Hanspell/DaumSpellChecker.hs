{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Hanspell.DaumSpellChecker
    ( spellCheckByDaum
    , daumSpellCheckerMaxChars
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Text.Regex
import Debug.Trace
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Hanspell.Typo
import Hanspell.Decoder

-- | Maximum character length for one spell check request.
daumSpellCheckerMaxChars :: Int
daumSpellCheckerMaxChars = 1000
 
-- | 'spellCheckByDaum' has two return types. One is 'MaybeT IO [Typo]',
-- and the other is 'IO [Typo]'. I'd prefer the latter.
class Monad m => DaumSpellChecker m where
    -- | Requests spell check to the server, parses the responses, 
    -- and returns m [Typo]
    spellCheckByDaum :: T.Text -> m [Typo]

-- | Obssesive version.
instance DaumSpellChecker (MaybeT IO) where
    -- spellCheckByDaum :: T.Text -> MaybeT IO [Typo]
    spellCheckByDaum text = requestToDaum text >>= return . responseToTypos

-- | Bold version.
instance DaumSpellChecker IO where
    -- spellCheckByDaum :: T.Text -> IO [Typo]
    spellCheckByDaum text = do
        maybe <- runMaybeT $ requestToDaum text >>= return . responseToTypos
        case maybe of 
            Nothing -> return []
            Just typos -> return typos

-- 'spellCheckByDaum' prints the message below when the HTTP status code is 
-- not 200. It is mainly due to timeout.
daumConnectError :: String
daumConnectError = "-- 한스펠 오류: 다음 서버 접속 오류로 일부 문장 교정 실패"

-- 'spellCheckByDaum' prints the message below when the response is not of 
-- spell checker. Sometimes the service URL changes.
invalidResponseFromDaum :: String
invalidResponseFromDaum = "-- 한스펠 오류: 다음 서버가 유효하지 않은 결과 반환"

-- Daum spell checker URL.
--
-- Try `curl -d "sentence=안녕 하세요" -H \
-- "Content-Type: application/x-www-form-urlencoded" -X POST \
-- https://dic.daum.net/grammar_checker.do`
daumSpellCheckUrl :: String
daumSpellCheckUrl = "https://dic.daum.net/grammar_checker.do"

-- | Requests spell check to the server, check the responses, and returns it. 
-- When the status code is not 200 or the response is not of spell checker, 
-- traces error message, and returns Nothing.
requestToDaum :: T.Text -> MaybeT IO B.ByteString
requestToDaum text = do
    manager <- lift $ newManager tlsManagerSettings
    let pair = [("sentence",TE.encodeUtf8 text)]
    initialRequest <- lift $ parseRequest daumSpellCheckUrl
    let request = (urlEncodedBody pair initialRequest) { method = "POST" }
    response <- lift $ httpLbs request manager
    let errCode = statusCode (responseStatus response)
        daumResponseInfix = BU.fromString 
                  "<h2 class=\"screen_out\">맞춤법 검사기 본문</h2>"
     in if (errCode == 200)
           then let body = BL.toStrict $ responseBody response
                 in if B.isInfixOf daumResponseInfix body
                       then return body
                       else trace invalidResponseFromDaum 
                           (MaybeT $ return Nothing)
           else trace (daumConnectError ++ " ("++ show errCode ++ ")")
               (MaybeT $ return Nothing)

-- | Parses the response to [Typo]
responseToTypos :: B.ByteString -> [Typo]
responseToTypos body =
    let text = TE.decodeUtf8 body
        -- removes unused from body
        stripped = head . T.splitOn (T.pack "<span class=\"info_byte\">") $ text
        -- splits the body to each typo
        splitted = tail . T.splitOn (T.pack "data-error-type") $ stripped
     in map textToTypo splitted

-- | Parse a unit of response to Typo
textToTypo :: T.Text -> Typo
textToTypo body =
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
