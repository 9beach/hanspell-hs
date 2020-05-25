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
reversed :: Bool -> String -> String
reversed isTTY text = if isTTY 
                         then "\x1b[7m" ++ text ++ "\x1b[0m" 
                         else text

grey :: Bool -> String -> String
grey isTTY text = if isTTY 
                     then "\x1b[90m" ++ text ++ "\x1b[0m" 
                     else text

-- | Fix typos of the text. The colors of fixed words are inverted.
fixTyposWithStyle :: Bool -> String -> [Typo] -> String
fixTyposWithStyle isTTY = foldl (fixTypo isTTY)
  where
    replace from to = intercalate to . splitOn from
    fixTypo :: Bool -> String -> Typo -> String
    fixTypo isTTY text aTypo = 
        let aSuggestion = head (suggestions aTypo)
         in if aSuggestion == token aTypo
               then text
               else replace (token aTypo) (reversed isTTY aSuggestion) text

-- | Convert a typo to text. 'info' of the typo is greyed out.
typoToTextWithStyle :: Bool -> Typo -> String
typoToTextWithStyle isTTY typo = 
    concat [ token typo
           , grey isTTY " -> "
           , intercalate (grey isTTY ", ") (suggestions typo)
           , "\n"
           , grey isTTY (info typo)
           ]

-- | Removes the typos whose tokens are duplicated. Order preserving and 
-- O(nlogn).
rmdupTypos :: [Typo] -> [Typo]
rmdupTypos typos =
    map fst . sortBy compareOrder . rmdup . sortBy compareToken 
        $ zip typos [1..]
  where
    compareToken (t, n) (t', n') = compare (token t) (token t')
    compareOrder (t, n) (t', n') = compare n n'
    rmdup (a:b:typos) = if compareToken a b == EQ
                           then rmdup (a:typos)
                           else a:rmdup (b:typos)
    rmdup typos = typos
