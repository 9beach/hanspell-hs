module Main where

import qualified Data.Text.IO as I
import qualified Data.Text as T

import System.Environment   
import System.Exit
import System.IO
import Control.Concurrent.Async

import Hanspell

data SpellChecker = DAUM | PNU | All deriving Eq

main :: IO ()
main = do 
    args <- getArgs
    checker <- case args of
        []          -> return DAUM
        ["-d"]      -> return DAUM
        ["--daum"]  -> return DAUM
        ["-a"]      -> return All
        ["--all"]   -> return All
        ["-p"]      -> return PNU
        ["--pnu"]   -> return PNU
        _           -> putStrLn help >> exitFailure

    -- Reads input and splits it to proper size :: [Text].
    sentences <- I.getContents
    let splitted = if checker == DAUM
                   then linesByLength daumSpellCheckerMaxChars sentences
                   else linesByWordCount pnuSpellCheckerMaxWords sentences

    -- Get typos :: [Typo] from spell check service.
    typos <- case checker of
        DAUM -> concat <$> mapConcurrently spellCheckByDaum splitted
        PNU  -> concat <$> mapConcurrently spellCheckByPnu splitted
        All  ->   (++) <$> (concat <$> mapConcurrently spellCheckByDaum splitted)
                       <*> (concat <$> mapConcurrently spellCheckByPnu splitted)

    -- Prints typos and fixed sentences
    let typos' = rmdupTypos typos
    mapM_ (I.putStr . typoToTextWithStyle) typos'
    I.putStr $ fixTyposWithStyle sentences typos'

    -- Writes history
    logPath <- (++ "/.hanspell-history") <$> getEnv "HOME"
    let logs = T.intercalate (T.singleton '\n') . map (\t -> T.concat
                                                [ token t
                                                , T.pack " -> "
                                                , head (suggestions t)
                                                ]) $ typos'
    I.appendFile logPath logs

help :: String
help = "\
\사용법: hanspell [-d | -p | -a | -h]\n\
\\n\
\옵션:\n\
\  -d, --daum              다음 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -p, --pnu               부산대학교 서비스를 이용해서 맞춤법을 교정합니다\n\
\  -a, --all [default]     두 서비스의 모든 결과를 반영해서 맞춤법을 교정합니다\n\
\  -h, --info              도움말을 출력합니다"
