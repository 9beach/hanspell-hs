{-# OPTIONS_HADDOCK hide #-}

-- | Breaks a text up into a list of texts at newline chars. The maximum 
-- length or word count of a element of the list is given by parameters.
-- So the element may contain multiple newlines.
module Language.Hanspell.TextLines
    ( linesByLength
    , linesByWordCount
    ) where

import Data.List

-- | Line-wrapped break up to character count.
linesByLength :: Int -> String -> [String]
linesByLength maxChars =
    map unlines . reverse . map reverse . merge . 
        foldl' mergeLine (maxChars,0,[],[]) . lines
  where
    mergeLine :: (Int,Int,[String],[[String]])
              -> String
              -> (Int,Int,[String],[[String]])
    mergeLine (maxChars,headsLength,heads,merged) aLine = 
        if lineLength + headsLength > maxChars && not (null heads) 
           then (maxChars,lineLength,[aLine],heads:merged)
           else (maxChars,lineLength + headsLength,aLine:heads,merged)
        where lineLength = length aLine + 1

-- | Line-wrapped break up to word count.
linesByWordCount :: Int -> String -> [String]
linesByWordCount maxWords =
    map unlines . reverse . map reverse . merge . 
        foldl' mergeLine (maxWords,0,[],[]) . lines
  where
    mergeLine :: (Int,Int,[String],[[String]])
              -> String
              -> (Int,Int,[String],[[String]])
    mergeLine (maxWords,headsWords,heads,merged) aLine = 
        if xWords + headsWords > maxWords && not (null heads) 
           then (maxWords,xWords,[aLine],heads:merged)
           else (maxWords,xWords + headsWords,aLine:heads,merged)
        where xWords = length (words aLine)

-- Add the heads left to the merged heads list
merge (_,_,heads,merged) = if null heads then merged else heads:merged
