{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO as I
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Concurrent.Async

import Hanspell

main = do 
    content <- I.getContents
    let texts = linesByLength daumSpellCheckerMaxChars content
    typos <- concat <$> mapConcurrently spellCheckByDaum texts
    let typos' = rmdup typos
    mapM_ (I.putStr . typoToTextWithColors) typos
    I.putStr $ fixTyposWithColors content typos'
