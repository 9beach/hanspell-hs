-- | Defines Typo data structure and related utilities. Typo data carries the 
-- information of a typo.
module Language.Hanspell.Typo 
    ( Typo(..)
    , fixTyposWithStyle
    , typoToTextWithStyle
    , rmdupTypos
    ) where

import Data.Ord
import Data.List
import Data.List.Split

-- | Carries the information of a typo.
data Typo = Typo { errorType    ::  String
                 , token        ::  String
                 , suggestions  :: [String]
                 , context      ::  String
                 , info         ::  String
                 } deriving (Show, Eq, Ord)

-- Changes console text style
reverseText = "\x1b[7m"
greyText    = "\x1b[90m"
resetText   = "\x1b[0m"

-- | Fix typos of the text. The colors of fixed words are inverted.
fixTyposWithStyle :: String -> [Typo] -> String
fixTyposWithStyle = foldl fixTypo
  where
    replace from to = intercalate to . splitOn from
    fixTypo :: String -> Typo -> String
    fixTypo text aTypo = let aSuggestion = head (suggestions aTypo)
                          in if aSuggestion == token aTypo
                                then text
                                else replace (token aTypo) (concat
                                     [ reverseText
                                     , aSuggestion
                                     , resetText
                                     ]) text

-- | Convert a typo to text. 'info' of the typo is greyed out.
typoToTextWithStyle :: Typo -> String
typoToTextWithStyle typo = concat
                         [ token typo
                         , arrow
                         , intercalate comma (suggestions typo)
                         , ln
                         , greyText
                         , info typo
                         , resetText
                         ]
  where
    ln      = "\n"
    comma   = "\x1b[90m, \x1b[0m"
    arrow   = "\x1b[90m -> \x1b[0m"

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
