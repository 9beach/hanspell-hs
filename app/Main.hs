module Main where

import qualified Data.Text.IO as T

import System.Environment   
import System.Exit
import Control.Concurrent.Async

import Hanspell

data SpellChecker = DAUM | PNU | All deriving Eq

main :: IO ()
main = do 
    args <- getArgs
    checker <- case args of
        [] -> return DAUM
        ["-d"] -> return DAUM
        ["--daum"] -> return DAUM
        ["-p"] -> return PNU
        ["--pnu"] -> return PNU
        ["-a"] -> return All
        ["--all"] -> return All
        _ -> putStrLn help >> exitFailure

    contents <- T.getContents
    let texts = if checker == DAUM
                   then linesByLength daumSpellCheckerMaxChars contents
                   else linesByWordCount pnuSpellCheckerMaxWords contents

    typos <- case checker of
        DAUM -> concat <$> mapConcurrently spellCheckByDaum texts
        PNU -> concat <$> mapConcurrently spellCheckByPnu texts
        All -> (++) <$> (concat <$> mapConcurrently spellCheckByDaum texts)
                    <*> (concat <$> mapConcurrently spellCheckByPnu texts)

    let typos' = rmdupTypos typos
    mapM_ (T.putStr . typoToTextWithStyle) typos'
    T.putStr $ fixTyposWithStyle contents typos'

help :: String
help = "\
\사용법: hanspell [-d | -p | -a | -h]\n\
\\n\
\옵션:\n\
\  -d, --daum [default]    다음 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -p, --pnu               부산대학교 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -a, --all               두 서비스의 모든 결과를 반영해서 맞춤법을 교정합니다\n\
\  -h, --info              도움말을 출력합니다"
