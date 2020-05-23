-- | Defines Typo data structure and related utilities. Typo data carries the 
-- information of a typo.
module Hanspell.Typo 
    ( Typo(..)
    , fixTyposWithStyle
    , typoToTextWithStyle
    , rmdupTypos
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
fixTyposWithStyle = foldl fixTypo
  where
    fixTypo :: T.Text -> Typo -> T.Text
    fixTypo text aTypo = let aSuggestion = head (suggestions aTypo)
                          in if aSuggestion == token aTypo
                                then text
                                else T.replace (token aTypo) (T.concat
                                        [ reverseText
                                        , aSuggestion
                                        , resetText
                                        ]) text

-- | Convert a typo to text. 'info' of the typo is greyed out.
typoToTextWithStyle :: Typo -> T.Text
typoToTextWithStyle typo = T.concat
                         [ token typo
                         , arrow
                         , T.intercalate comma (suggestions typo)
                         , ln
                         , greyText
                         , info typo
                         , resetText
                         ]
  where
    ln      = T.pack "\n"
    comma   = T.pack "\x1b[90m, \x1b[0m"
    arrow   = T.pack "\x1b[90m → \x1b[0m"

-- | Removes the typos whose tokens are duplicated. Order preserving and 
-- O(nlogn).
rmdupTypos :: [Typo] -> [Typo]
rmdupTypos typos =
    map fst . sortBy compare' . rmdup . sort $ zip typos [1..]
  where
    compare' (n,t) (n',t') = compare t t'
    rmdup (a:b:typos) = if token (fst a) == token (fst b)
                           then rmdup (a:typos)
                           else a:rmdup (b:typos)
    rmdup typos = typos
