module Main where

import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Terminal
import Data.List
import Control.Concurrent.Async

import Language.Hanspell.Typo
import Language.Hanspell.DaumSpellChecker
import Language.Hanspell.PnuSpellChecker
import Language.Hanspell.Glob
import Language.Hanspell.TextLines

data SpellChecker = DAUM | PNU | All

main :: IO ()
main = do
    args <- getArgs
    checker <- case args of
        []          -> return DAUM
        ["-d"]      -> return DAUM
        ["--daum"]  -> return DAUM
        ["-p"]      -> return PNU
        ["--pnu"]   -> return PNU
        ["-a"]      -> return All
        ["--all"]   -> return All
        _           -> putStrLn help >> exitFailure

    -- Reads input and splits it to proper size.
    sentences <- getContents
    let splitted = case checker of
            DAUM -> linesByLength daumSpellCheckerMaxChars sentences
            _    -> linesByWordCount pnuSpellCheckerMaxWords sentences

    -- Spell checks (gets typos) from the servers.
    typos <- case checker of
        DAUM -> concat <$> mapConcurrently spellCheckByDaum splitted
        PNU  -> concat <$> mapConcurrently spellCheckByPnu splitted
        All  -> (++) <$> (concat <$> mapConcurrently spellCheckByPnu splitted)
                     <*> (concat <$> mapConcurrently spellCheckByDaum splitted)

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

help :: String
help = "\
\사용법: hanspell [-d | -p | -a | -h]\n\
\\n\
\옵션:\n\
\  -d, --daum              다음 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -p, --pnu               부산대학교 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -a, --all [default]     두 서비스의 모든 결과를 반영해서 맞춤법을 교정합니다\n\
\  -h, --info              도움말을 출력합니다\n\
\\n\
\버그 리포트와 제안: <https://github.com/9beach/hanspell-hs/issues>\n\
\한스펠 홈 페이지: <https://github.com/9beach/hanspell-hs/>"
