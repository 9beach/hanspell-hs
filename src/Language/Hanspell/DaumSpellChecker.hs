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
import System.IO (hPutStrLn, stderr)
import Network.HTTP.Types.Status
import Text.Regex.TDFA ((=~))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Language.Hanspell.Typo
import Language.Hanspell.Decoder

-- | Defines a class for 'spellCheckByDaum' function overloading.
class Monad m => DaumSpellChecker m where
    -- | Requests spell check to DAUM server, parses the responses,
    -- and returns @m [Typo]@.
    spellCheckByDaum :: String -> m [Typo]

-- | Obsessive version returning @MaybeT IO [Typo]@.
instance DaumSpellChecker (MaybeT IO) where
    spellCheckByDaum text = htmlToTypos <$> requestToDaum text

-- | Bold version returning @IO [Typo]@.
instance DaumSpellChecker IO where
    spellCheckByDaum text = do
        result <- runMaybeT $ htmlToTypos <$> requestToDaum text
        case result of
            Nothing    -> return []
            Just typos -> return typos

-- | Official maximum character length for a 'spellCheckByDaum' request.
daumSpellCheckerMaxChars :: Int
daumSpellCheckerMaxChars = 1000

daumConnectError :: String
daumConnectError =
    "-- 한스펠 오류: 다음 서버의 접속 오류로 일부 문장 교정에 실패했습니다."

invalidResponseFromDaum :: String
invalidResponseFromDaum =
    "-- 한스펠 오류: 다음 서비스가 유효하지 않은 양식을 반환했습니다. ("
    ++ daumSpellCheckUrl ++ ")"

daumSpellCheckUrl :: String
daumSpellCheckUrl = "https://dic.daum.net/grammar_checker.do"

httpTimeoutMicros :: Int
httpTimeoutMicros = 20 * 1000 * 1000

-- POSIX-style regex substitution built on regex-tdfa.
gsub :: String -> String -> String -> String
gsub pat replacement = go
  where
    go s = case s =~ pat :: (String, String, String) of
        (_,      "", _)    -> s
        (before, _,  rest) -> before ++ replacement ++ go rest

requestToDaum :: String -> MaybeT IO String
requestToDaum text = do
    manager <- lift $ newManager $ tlsManagerSettings
        { managerResponseTimeout = responseTimeoutMicro httpTimeoutMicros }
    let pair = [("sentence", BU.fromString text)]
    initialRequest <- lift $ parseRequest daumSpellCheckUrl
    let request = (urlEncodedBody pair initialRequest) { method = "POST" }
    response <- lift $ httpLbs request manager
    let errCode = statusCode (responseStatus response)
    let daumResponseInfix = "=\"screen_out\">맞춤법 검사기 본문</h2>"
    if errCode == 200
       then let body = BLU.toString (responseBody response)
             in if daumResponseInfix `isInfixOf` body
                   then return body
                   else do
                       lift $ hPutStrLn stderr invalidResponseFromDaum
                       MaybeT $ return Nothing
       else do
           lift $ hPutStrLn stderr
               (daumConnectError ++ " (HTTP " ++ show errCode ++ ")")
           MaybeT $ return Nothing

-- Parses the response HTML to [Typo]
htmlToTypos :: String -> [Typo]
htmlToTypos body =
    let stripped = head . splitOn "<span class=\"info_byte\">" $ body
        splitted = tail . splitOn "data-error-type" $ stripped
     in map htmlToTypo splitted

-- Parses a unit of response to Typo
htmlToTypo :: String -> Typo
htmlToTypo body = Typo
    { errorType   =  decodeEntity (splitted !! 1)
    , token       =  decodeEntity (splitted !! 3)
    , suggestions = [decodeEntity (splitted !! 5)]
    , context     =  decodeEntity (splitted !! 7)
    , info        =  infoFinal
    } where
    splitted = splitOn "\"" $ head (lines body)
    raw      = splitOn "<div>" body !! 1
    body'    = head (splitOn "<span class=\"info_byte\">" raw)
    cleaned  = gsub "^[ \n][ \n]*"                  ""
             . gsub "<[^>]*>"                       ""
             . gsub "<br[^>]*>"                     "\n"
             . gsub "</span><span class.*\n"       ""
             . gsub "<a href=\"#none\".*\n"        ""
             . gsub "^<span>.*\n"                  ""
             . gsub "<strong class.*\n"            ""
             . gsub ".*strong class=.tit_help.>예문</strong.*\n" "(예)"
             . gsub "\t"                            ""
             $ body'
    infoFinal | decodeEntity cleaned == "도움말이 없습니다.\n" = ""
              | otherwise                                       = cleaned
