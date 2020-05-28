module HanspellExample where

import Language.Hanspell

example = do
    let sentence = "위에계신분, 잘들리세요?"
    typos <- spellCheckByDaum sentence
    mapM_ (putStrLn . typoToStringWithStyle True) typos
