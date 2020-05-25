{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Requests spell check to PNU server, parses the responses, and 
-- returns m [Typo]. Two versions of return types are supported. One is 
-- 'MaybeT IO [Typo]', and the other is 'IO [Typo]'. I'd prefer the latter.
module Language.Hanspell.PnuSpellChecker
    ( spellCheckByPnu
    , pnuSpellCheckerMaxWords
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
import Data.Aeson
import GHC.Generics

import Language.Hanspell.Typo
import Language.Hanspell.Decoder

-- | 'spellCheckByPnu' has two return types. One is 'MaybeT IO [Typo]',
-- and the other is 'IO [Typo]'. I'd prefer the latter.
class Monad m => PnuSpellChecker m where
    -- | Requests spell check to the server, parses the responses, 
    -- and returns m [Typo]. 'spellCheckByPnu' has two return types.
    -- One is 'MaybeT IO [Typo]', and the other is 'IO [Typo]'. 
    -- I'd prefer the latter.
    spellCheckByPnu :: String -> m [Typo]

-- | Obssesive version.
instance PnuSpellChecker (MaybeT IO) where
    -- spellCheckByPnu :: String.Text -> MaybeT IO [Typo]
    spellCheckByPnu text = htmlToTypos <$> requestToPnu text

-- | Bold version.
instance PnuSpellChecker IO where
    -- spellCheckByPnu :: String.Text -> IO [Typo]
    spellCheckByPnu text = do
        maybe <- runMaybeT $ htmlToTypos <$> requestToPnu text
        case maybe of 
            Nothing -> return []
            Just typos -> return typos

-- | Maximum words count for one spell check request.
pnuSpellCheckerMaxWords :: Int
pnuSpellCheckerMaxWords = 295
 
-- 'spellCheckByPnu' prints the message below when the HTTP status code is 
-- not 200. Mainly due to timeout.
pnuConnectError :: String
pnuConnectError = 
        "-- 한스펠 오류: 부산대 서버의 접속 오류로 일부 문장 교정에 실패했습니다."

-- 'spellCheckByPnu' prints the message below when the response is not of 
-- spell checker. Mainly due to the changes of service URL.
invalidResponseFromPnu :: String
invalidResponseFromPnu = 
        "-- 한스펠 오류: 부산대 서비스가 유효하지 않은 양식을 반환했습니다. ("
        ++ pnuSpellCheckUrl ++ ")"

-- Pnu spell checker URL.
--
-- Try `curl -H "Content-Type: application/x-www-form-urlencoded" \
-- -X POST http://speller.cs.pusan.ac.kr/results -d \
-- --data-urlencode "text1=안녕 하세요"`.
pnuSpellCheckUrl :: String
pnuSpellCheckUrl = "http://speller.cs.pusan.ac.kr/results"

-- For convenience
gsub :: String -> String -> String -> String
gsub from to text = subRegex (mkRegex from) text to

-- Requests spell check to the server, check the responses, and returns it. 
-- When the status code is not 200, or the response is not of spell checker, 
-- traces error message, and returns Nothing.
requestToPnu :: String -> MaybeT IO String
requestToPnu text = if null (words text) then return "" else do
    -- Walkaround for PNU server's weired logic
    let text' = intercalate "\n " . splitOn "\n" $ text
    manager <- lift $ newManager tlsManagerSettings
    let pair = [("text1",BU.fromString text')]
    initialRequest <- lift $ parseRequest pnuSpellCheckUrl
    let request = (urlEncodedBody pair initialRequest) { method = "POST" }
    response <- lift $ httpLbs request manager
    let errCode = statusCode (responseStatus response)

    let pnuResponseInfix = "<title>한국어 맞춤법/문법 검사기</title>"
    if errCode == 200
       then let body = BLU.toString (responseBody response)
             in if pnuResponseInfix `isInfixOf` body
                   then return body
                   else trace invalidResponseFromPnu (MaybeT $ return Nothing)
       else trace (pnuConnectError ++ " ("++ show errCode ++ ")")
            (MaybeT $ return Nothing)

-- PNU response format for a Typo.
data PnuTypo = PnuTypo
    { help :: String
    , errorIdx :: Int
    , correctMethod :: Int
    , start :: Int
    , end :: Int
    , orgStr :: String
    , candWord :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

-- PNU response format for Typos.
data PnuTypos = PnuTypos
    { str :: String
    , errInfo :: [PnuTypo]
    , idx :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Parses the response HTML to [Typo].
htmlToTypos :: String -> [Typo]
htmlToTypos body =
    case matchRegex (mkRegex "^\tdata = (.*);$") body of 
        Nothing -> []
        Just [jsonText] -> map pnuTypoToTypo pnuTypos
          where
            Just pnuTyopsList = decode . BLU.fromString $ jsonText
            pnuTypos = mconcat . map errInfo $ pnuTyopsList

-- Converts PnuTypo (response JSON) to Typo.
pnuTypoToTypo :: PnuTypo -> Typo
pnuTypoToTypo pnuTypo = 
    Typo { errorType = ""
         , token = decodeEntity . orgStr $ pnuTypo
         , suggestions = splitOn "|" . decodeEntity $ suggestions'
         , context = ""
         , info = decodeEntity . gsub "\n\n" "\n" 
                . gsub " *<br/> *" "\n" . (++ "\n") . help $ pnuTypo
         } where
    suggestions' = if null . candWord $ pnuTypo
                        then orgStr pnuTypo
                        else candWord pnuTypo
