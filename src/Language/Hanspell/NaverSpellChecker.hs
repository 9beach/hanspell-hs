{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Defines the interfaces for Naver spell check service.
module Language.Hanspell.NaverSpellChecker
    ( NaverSpellChecker
    , spellCheckByNaver
    , naverSpellCheckerMaxWords
    , naverSpellCheckerMinIntervalMicros
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM

import Data.Aeson (decode, Value(..))
import Data.IORef
import Data.List (isInfixOf)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)
import Text.Regex.TDFA ((=~))

import Language.Hanspell.Typo
import Language.Hanspell.Decoder

-- | Defines a class for 'spellCheckByNaver' function overloading.
class Monad m => NaverSpellChecker m where
    -- | Requests spell check to Naver server, parses the response and
    -- returns @m [Typo]@. Has two return types: @MaybeT IO [Typo]@ and
    -- @IO [Typo]@.
    --
    -- @
    -- import Language.Hanspell
    --
    -- example = do
    --     let sentence = "안뇽하세요. 정교수측이 추천한 영화."
    --     typos <- spellCheckByNaver sentence
    --     mapM_ (putStrLn . typoToStringWithStyle False) typos
    -- @
    spellCheckByNaver :: String -> m [Typo]

-- | Obsessive version returning @MaybeT IO [Typo]@.
instance NaverSpellChecker (MaybeT IO) where
    spellCheckByNaver text = responseToTypos <$> requestToNaver text

-- | Bold version returning @IO [Typo]@.
instance NaverSpellChecker IO where
    spellCheckByNaver text = do
        result <- runMaybeT $ responseToTypos <$> requestToNaver text
        case result of
            Nothing    -> return []
            Just typos -> return typos

-- | Maximum words per request. Naver의 SpellerProxy는 GET 쿼리스트링
-- 길이가 약 3300자(대략 100단어)를 넘으면 HTTP 413을 반환합니다.
-- 안전 여유를 두고 80단어로 청크를 자르세요.
naverSpellCheckerMaxWords :: Int
naverSpellCheckerMaxWords = 80

-- | Recommended minimum interval (μs) between consecutive Naver requests
-- to avoid rate limiting.
naverSpellCheckerMinIntervalMicros :: Int
naverSpellCheckerMinIntervalMicros = 400 * 1000

naverConnectError :: String
naverConnectError =
    "-- 한스펠 오류: 네이버 서버의 접속 오류로 일부 문장 교정에 실패했습니다."

naverInvalidResponse :: String
naverInvalidResponse =
    "-- 한스펠 오류: 네이버 서비스가 유효하지 않은 양식을 반환했습니다."

naverPassportPage :: String
naverPassportPage =
    "https://search.naver.com/search.naver?query=%EB%A7%9E%EC%B6%A4%EB%B2%95+%EA%B2%80%EC%82%AC%EA%B8%B0"

naverProxyUrl :: String
naverProxyUrl =
    "https://m.search.naver.com/p/csearch/ocontent/util/SpellerProxy"

naverUA :: BS.ByteString
naverUA = BU.fromString
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 \
    \(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

httpTimeoutMicros :: Int
httpTimeoutMicros = 20 * 1000 * 1000

-- Module-level cache for passportKey. Naver issues per-session keys and we
-- re-fetch on "유효한 키" errors.
{-# NOINLINE passportKeyRef #-}
passportKeyRef :: IORef (Maybe String)
passportKeyRef = unsafePerformIO (newIORef Nothing)

makeManager :: IO Manager
makeManager = newManager $ tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro httpTimeoutMicros }

fetchPassportKey :: Manager -> IO (Maybe String)
fetchPassportKey manager = do
    req' <- parseRequest naverPassportPage
    let req = req' { requestHeaders = [("User-Agent", naverUA)] }
    res <- httpLbs req manager
    let body = BLU.toString (responseBody res) :: String
    return $ case body =~ ("passportKey=([a-f0-9]+)" :: String)
                  :: (String, String, String, [String]) of
        (_, _, _, [key]) -> Just key
        _                -> Nothing

getPassportKey :: Manager -> Bool -> MaybeT IO String
getPassportKey manager forceRefresh = do
    cached <- lift $ readIORef passportKeyRef
    case (cached, forceRefresh) of
        (Just k, False) -> return k
        _ -> do
            mkey <- lift $ fetchPassportKey manager
            case mkey of
                Nothing -> do
                    lift $ hPutStrLn stderr
                        (naverInvalidResponse ++ " (passportKey 추출 실패)")
                    MaybeT $ return Nothing
                Just k -> do
                    lift $ writeIORef passportKeyRef (Just k)
                    return k

makeQueryUrl :: String -> String -> String
makeQueryUrl key text =
    naverProxyUrl
    ++ "?_callback=jQuery"
    ++ "&q=" ++ encoded
    ++ "&where=nexearch&color_blindness=0"
    ++ "&passportKey=" ++ key
  where
    encoded = BU.toString (urlEncode True (BU.fromString text))

callOnce :: Manager -> String -> MaybeT IO String
callOnce manager url = do
    req' <- lift $ parseRequest url
    let req = req' { requestHeaders =
                       [ ("User-Agent", naverUA)
                       , ("Referer", "https://search.naver.com/")
                       ] }
    res <- lift $ httpLbs req manager
    let code = statusCode (responseStatus res)
        body = BLU.toString (responseBody res)
    if code == 200
        then return body
        else do
            lift $ hPutStrLn stderr
                (naverConnectError ++ " (HTTP " ++ show code ++ ")")
            MaybeT $ return Nothing

-- 한 번 호출 → invalid key면 forceRefresh로 한 번 더 시도.
requestToNaver :: String -> MaybeT IO String
requestToNaver text =
    if null (words text)
    then return ""
    else do
        manager <- lift makeManager
        body <- attempt manager False
        if "유효한 키가 아닙니다" `isInfixOf` body
            then do
                lift $ writeIORef passportKeyRef Nothing
                attempt manager True
            else return body
  where
    attempt manager force = do
        key <- getPassportKey manager force
        callOnce manager (makeQueryUrl key text)

-- JSONP wrapper(`jQuery(...)`)를 벗긴다.
unwrapJsonp :: String -> Maybe String
unwrapJsonp body =
    case break (== '(') body of
        (_, '(':rest) ->
            let stripped = dropWhileEnd isSpaceOrSemi rest
            in case reverse stripped of
                ')':xs -> Just (reverse xs)
                _      -> Nothing
        _ -> Nothing
  where
    isSpaceOrSemi c = c == ' ' || c == '\t' || c == '\n' || c == ';'
    dropWhileEnd p = reverse . dropWhile p . reverse

responseToTypos :: String -> [Typo]
responseToTypos "" = []
responseToTypos body =
    case unwrapJsonp body >>= decode . BLU.fromString of
        Just (Object obj) -> fromMessage obj
        _                 -> []
  where
    fromMessage obj = case KM.lookup "message" obj of
        Just (Object msg) -> case KM.lookup "result" msg of
            Just (Object res) -> typosFromResult res
            _                 -> []
        _ -> []

typosFromResult :: KM.KeyMap Value -> [Typo]
typosFromResult res =
    let origins = extractOrigins originHtml
        fixes   = extractFixes htmlBody
        n       = min (length origins) (length fixes)
    in zipWith makeTypo (take n origins) (take n fixes)
  where
    originHtml = lookupStr "origin_html" res
    htmlBody   = lookupStr "html"        res
    lookupStr k m = case KM.lookup k m of
        Just (String t) -> T.unpack t
        _               -> ""
    makeTypo tok (color, fixed) = Typo
        { errorType   = ""
        , token       = decodeEntity tok
        , suggestions = [decodeEntity fixed]
        , context     = ""
        , info        = colorInfo color
        }

extractOrigins :: String -> [String]
extractOrigins src =
    [ m !! 1
    | m <- src =~ ("<span class='result_underline'>([^<]+)</span>" :: String)
        :: [[String]]
    , length m >= 2
    ]

extractFixes :: String -> [(String, String)]
extractFixes src =
    [ (m !! 1, m !! 2)
    | m <- src =~ ("<em class='([a-z]+)_text'>([^<]+)</em>" :: String)
        :: [[String]]
    , length m >= 3
    ]

colorInfo :: String -> String
colorInfo "red"    = "맞춤법 오류입니다.\n"
colorInfo "green"  = "띄어쓰기 오류입니다.\n"
colorInfo "blue"   = "표준어 의심이거나 대치어 추천입니다.\n"
colorInfo "violet" = "통계적 교정입니다.\n"
colorInfo _        = ""
