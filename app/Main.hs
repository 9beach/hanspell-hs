module Main where

import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Terminal
import Control.Concurrent (threadDelay)

import Language.Hanspell.Typo
import Language.Hanspell.DaumSpellChecker
import Language.Hanspell.NaverSpellChecker
import Language.Hanspell.Glob
import Language.Hanspell.TextLines

data SpellChecker = DAUM | NAVER | All

main :: IO ()
main = do
    args <- getArgs
    checker <- case args of
        []           -> return DAUM
        ["-d"]       -> return DAUM
        ["--daum"]   -> return DAUM
        ["-n"]       -> return NAVER
        ["--naver"]  -> return NAVER
        ["-a"]       -> return All
        ["--all"]    -> return All
        _            -> putStrLn help >> exitFailure

    -- Reads input and splits it to proper size.
    sentences <- getContents
    let daumChunks  = linesByLength    daumSpellCheckerMaxChars sentences
        naverChunks = linesByWordCount naverSpellCheckerMaxWords sentences

    -- Spell checks (gets typos) from the servers.
    typos <- case checker of
        DAUM  -> concat <$> mapM spellCheckByDaum daumChunks
        NAVER -> naverChecks naverChunks
        All   -> (++) <$> naverChecks naverChunks
                     <*> (concat <$> mapM spellCheckByDaum daumChunks)

    -- Removes duplicated typos.
    let typos' = rmdupTypos typos

    -- Reads ignoring typos.
    homeDir <- getEnv "HOME"
    let ignorePath = homeDir ++ "/.hanspell-ignore"
    exists <- doesFileExist ignorePath
    ignoreds <- if exists
                   then lines <$> readFile ignorePath
                   else return []

    -- Removes ignoring typos.
    let typos'' = if not (null ignoreds)
                     then filter (not . matchGlobs ignoreds . token) typos'
                     else typos'

    -- Prints typos and fixed sentences.
    isTTY <- queryTerminal stdOutput
    mapM_ (hPutStr stderr . typoToStringWithStyle isTTY) typos''
    putStr $ fixTyposWithStyle isTTY sentences typos''

    -- Writes history.
    let logPath = homeDir ++ "/.hanspell-history"
    let logs = concatMap (\t ->
               token t ++ " -> " ++ head (suggestions t) ++ "\n") typos''
    appendFile logPath logs

-- Naver는 청크 사이에 짧은 인터벌이 필요하므로 직렬 발사.
naverChecks :: [String] -> IO [Typo]
naverChecks parts = concat <$> sequenceWithDelay
    naverSpellCheckerMinIntervalMicros
    (map spellCheckByNaver parts)

sequenceWithDelay :: Int -> [IO a] -> IO [a]
sequenceWithDelay _     []     = return []
sequenceWithDelay _     [x]    = (:[]) <$> x
sequenceWithDelay delay (x:xs) = do
    r <- x
    threadDelay delay
    rs <- sequenceWithDelay delay xs
    return (r : rs)

help :: String
help = "\
\사용법: hanspell [-d | -n | -a | -h]\n\
\\n\
\옵션:\n\
\  -d, --daum [default]    다음 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -n, --naver             네이버 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -a, --all               두 서비스의 모든 결과를 반영해서 맞춤법을 교정합니다\n\
\  -h, --info              도움말을 출력합니다\n\
\\n\
\버그 리포트와 제안: <https://github.com/9beach/hanspell-hs/issues>\n\
\한스펠 홈 페이지: <https://github.com/9beach/hanspell-hs/>"
