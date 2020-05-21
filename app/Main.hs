module Main where

import Data.Text.IO as T
import Control.Concurrent.Async

import Hanspell

main = do 
    contents <- T.getContents
    let texts = linesByLength daumSpellCheckerMaxChars contents
    typos <- concat <$> mapConcurrently spellCheckByDaum texts
    let typos' = rmdupTypo typos
    mapM_ (T.putStr . typoToTextWithStyle) typos
    T.putStr $ fixTyposWithStyle contents typos'
