module Hanspell.Typo where

import qualified Data.Text as T

import Data.Ord
import Data.List

-- | Carries the information of a typo.
data Typo = Typo { errorType    ::  T.Text
                 , token        ::  T.Text
                 , suggestions  :: [T.Text]
                 , context      ::  T.Text
                 , info         ::  T.Text
                 } deriving (Show, Eq, Ord)

reverseText = T.pack "\x1b[7m"
greyText    = T.pack "\x1b[90m"
resetText   = T.pack "\x1b[0m"

lnText      = T.pack "\n"
commaText   = T.pack ", "
arrowText   = T.pack "\x1b[37m → \x1b[0m"
betweenText = T.pack "\x1b[37m ↔ \x1b[0m"

-- | Fix typos of the text. The colors of fixed words are inverted.
fixTyposWithStyle :: T.Text -> [Typo] -> T.Text
fixTyposWithStyle text typos = foldl fixTypo text typos
  where
    fixTypo :: T.Text -> Typo -> T.Text
    fixTypo text aTypo = if token aTypo /= suggestions aTypo!!0
                            then T.replace (token aTypo) (T.concat
                                 [ reverseText
                                 , suggestions aTypo!!0
                                 , resetText
                                 ]) text
                            else text

-- | Convert a typo to colored text
typoToTextWithStyle :: Typo -> T.Text
typoToTextWithStyle typo = T.concat [token typo,arrowText,match,lnText,
                                     greyText,info typo,resetText]
  where
    match :: T.Text
    match = if length (suggestions typo) > 1
               then T.concat
                    [ suggestions typo!!0
                    , betweenText
                    , T.intercalate commaText (tail (suggestions typo))
                    ]
               else (suggestions typo!!0)

-- | Removes the typos whose tokens are duplicated.
rmdupTypo :: [Typo] -> [Typo]
rmdupTypo typos = ts
  where
    ord = zip typos [1..] :: [(Typo,Int)]
    sorted = sort ord
    removed = rmdupTypo' sorted
    (ts,ns) = unzip $ sortBy cmp removed :: ([Typo],[Int])
    cmp (a,n) (b,m) = compare n m
    rmdupTypo' (a:b:ts) = if token (fst a) == token (fst b)
                         then rmdupTypo' (a:ts)
                         else a:rmdupTypo' (b:ts)
    rmdupTypo' ts = ts
