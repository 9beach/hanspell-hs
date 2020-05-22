-- | Breaks a text up into a list of texts at newline chars. The maximum 
-- length or word count of a element of the list is given by parameters.
-- So the element may contain multiple newlines.
module Hanspell.TextLines
    ( linesByLength
    , linesByWordCount
    ) where

import qualified Data.Text as T

-- | Line-wrapped break up to character count.
linesByLength :: Int -> T.Text -> [T.Text]
linesByLength maxChars =
    map T.unlines . reverse . map reverse . merge . 
        foldl mergeLine (maxChars,0,[],[]) . T.lines
  where
    mergeLine :: (Int,Int,[T.Text],[[T.Text]])
              -> T.Text
              -> (Int,Int,[T.Text],[[T.Text]])
    mergeLine (maxChars,headsLength,heads,merged) aLine = 
        if lineLength + headsLength > maxChars && not (null heads) 
           then (maxChars,lineLength,[aLine],heads:merged)
           else (maxChars,lineLength + headsLength,aLine:heads,merged)
        where lineLength = T.length aLine + 1

-- | Line-wrapped break up to word count.
linesByWordCount :: Int -> T.Text -> [T.Text]
linesByWordCount maxWords =
    map T.unlines . reverse . map reverse . merge . 
        foldl mergeLine (maxWords,0,[],[]) . T.lines
  where
    mergeLine :: (Int,Int,[T.Text],[[T.Text]])
              -> T.Text
              -> (Int,Int,[T.Text],[[T.Text]])
    mergeLine (maxWords,headsWords,heads,merged) aLine = 
        if xWords + headsWords > maxWords && not (null heads) 
           then (maxWords,xWords,[aLine],heads:merged)
           else (maxWords,xWords + headsWords,aLine:heads,merged)
        where xWords = length (T.words aLine)

-- Add the heads left to the merged heads list
merge (_,_,heads,merged) = if null heads then merged else heads:merged
