module Hanspell.Typo 
    ( Typo(..)
    , fixTyposWithStyle
    , typoToTextWithStyle
    , rmdupTypo
    ) where

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

-- Changes console text style
reverseText = T.pack "\x1b[7m"
greyText    = T.pack "\x1b[90m"
resetText   = T.pack "\x1b[0m"


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

-- | Convert a typo to text. The the typo info is greyed out.
typoToTextWithStyle :: Typo -> T.Text
typoToTextWithStyle typo = T.concat
                         [ token typo
                         , arrowText
                         , T.intercalate commaText (suggestions typo)
                         , lnText
                         , greyText
                         , info typo
                         , resetText
                         ]
  where
    lnText      = T.pack "\n"
    commaText   = T.pack ", "
    arrowText   = T.pack "\x1b[90m → \x1b[0m"

-- | Removes the typos whose tokens are duplicated. O(log n) and original ord
-- preserved.
rmdupTypo :: [Typo] -> [Typo]
rmdupTypo typos = typos'
  where
    ord = zip typos [1..] :: [(Typo,Int)]
    sorted = sort ord
    removed = rmdup sorted
    (typos',ns) = unzip $ sortBy cmp removed :: ([Typo],[Int])
    cmp (a,n) (b,m) = compare n m
    rmdup (a:b:typos') = if token (fst a) == token (fst b)
                         then rmdup (a:typos')
                         else a:rmdup (b:typos')
    rmdup typos' = typos'
