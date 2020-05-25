module Main where

import System.Directory
import System.Environment
import System.Exit
import System.IO
import Data.List
import Control.Concurrent.Async

import Language.Hanspell

data SpellChecker = DAUM | PNU | All deriving Eq

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

    -- Reads input and splits it to proper size :: [Text].
    sentences <- getContents
    let splitted = case checker of
            DAUM -> linesByLength daumSpellCheckerMaxChars sentences
            _    -> linesByWordCount pnuSpellCheckerMaxWords sentences

    -- Get typos :: [Typo] from spell check service.
    typos <- case checker of
        DAUM -> concat <$> mapConcurrently spellCheckByDaum splitted
        PNU  -> concat <$> mapConcurrently spellCheckByPnu splitted
        All  -> (++) <$> (concat <$> mapConcurrently spellCheckByDaum splitted)
                     <*> (concat <$> mapConcurrently spellCheckByPnu splitted)

    -- Removes duplicated typos.
    let typos' = rmdupTypos typos

    -- Removes ignoring typos.
    homeDir <- getEnv "HOME"

    let ignorePath = homeDir ++ "/.hanspell-ignore"
    exists <- doesFileExist ignorePath
    ignoreds <- if exists
                   then do contents <- readFile ignorePath
                           return (lines contents)
                   else return []
    mapM_ putStrLn ignoreds
    let typos'' = if not (null ignoreds)
                     then filter (not . matchGlobs ignoreds . token) typos'
                     else typos'

    -- Prints typos and fixed sentences
    mapM_ (hPutStr stderr . typoToTextWithStyle) typos''
    putStr $ fixTyposWithStyle sentences typos''

    -- Writes history
    let logPath = homeDir ++ "/.hanspell-history"
    let logs = intercalate "\n" . map (\t ->
               concat [token t," -> ",head (suggestions t)]) $ typos''
    appendFile logPath logs

help :: String
help = "\
\사용법: hanspell [-d | -p | -a | -h]\n\
\\n\
\옵션:\n\
\  -d, --daum              다음 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -p, --pnu               부산대학교 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -a, --all [default]     두 서비스의 모든 결과를 반영해서 맞춤법을 교정합니다\n\
\  -h, --info              도움말을 출력합니다"
