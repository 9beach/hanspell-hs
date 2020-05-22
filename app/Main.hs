module Main where

import Data.Text.IO as T

import System.Environment   
import System.Exit
import Control.Concurrent.Async

import Hanspell

main = do 
    args <- getArgs
    case args of
        [] -> return ()
        ["-d"] -> return ()
        ["-daum"] -> return ()
        ["-p"] -> return ()
        ["-pnu"] -> return ()
        ["-a"] -> return ()
        ["--all"] -> return ()
        _ -> exitWithHelp

    contents <- T.getContents
    let texts = linesByLength daumSpellCheckerMaxChars contents
    typos <- concat <$> mapConcurrently spellCheckByDaum texts
    let typos' = rmdupTypo typos
    mapM (T.putStr . typoToTextWithStyle) typos
    T.putStr $ fixTyposWithStyle contents typos'

exitWithHelp = Prelude.putStrLn help >> exitFailure where help = "\
\사용법: hanspell-cli [-d | -p | -j | -a | -h]\n\
\\n\
\옵션:\n\
\  -d, --daum [default]    다음 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -p, --pnu               부산대학교 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -j, --joint             두 서비스의 공통 결과만 반영해서 맞춤법을 교정합니다\n\
\  -a, --all               두 서비스의 모든 결과를 반영해서 맞춤법을 교정합니다\n\
\  -h, --info              도움말을 출력합니다"
