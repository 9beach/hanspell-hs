{-# OPTIONS_HADDOCK hide #-}

-- | Defines Typo data structure and related utilities. Typo data carries the 
-- information of a typo.
module Language.Hanspell.Typo 
    ( Typo(..)
    , fixTyposWithStyle
    , typoToStringWithStyle
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

-- If given @True@, makes console text style reversed.
reversed :: Bool -> String -> String
reversed isTTY text = if isTTY 
                         then "\x1b[7m" ++ text ++ "\x1b[0m" 
                         else text

-- If given @True@, makes console text style grey.
grey :: Bool -> String -> String
grey isTTY text = if isTTY 
                     then "\x1b[90m" ++ text ++ "\x1b[0m" 
                     else text

-- | Fixes typos of given sentences. If given @True@, the colors of fixed 
-- words are inverted.
fixTyposWithStyle :: Bool -> String -> [Typo] -> String
fixTyposWithStyle isTTY = foldl' (fixTypo isTTY)
  where
    replace from to = intercalate to . splitOn from
    fixTypo :: Bool -> String -> Typo -> String
    fixTypo isTTY text aTypo = 
        let aSuggestion = head (suggestions aTypo)
         in if aSuggestion == token aTypo
               then text
               else replace (token aTypo) (reversed isTTY aSuggestion) text

-- | Converts a 'Typo' to string. If given @True@, 'info' of the 'Typo' is 
-- greyed out.
typoToStringWithStyle :: Bool -> Typo -> String
typoToStringWithStyle isTTY typo = token typo
     ++ grey isTTY " -> "
     ++ intercalate (grey isTTY ", ") (suggestions typo)
     ++ "\n"
     ++ grey isTTY (info typo)

-- | Removes the 'Typo's whose 'token's are duplicated. Order preserving and 
-- O(nlogn).
rmdupTypos :: [Typo] -> [Typo]
rmdupTypos =
    map snd . sortBy compareOrder . rmdup . sortBy compareToken . zip [1..]
  where
    compareToken (n, t) (n', t') = compare (token t) (token t')
    compareOrder (n, t) (n', t') = compare n n'
    rmdup (a:b:typos) = if compareToken a b == EQ
                           then rmdup (a:typos)
                           else a:rmdup (b:typos)
    rmdup typos = typos
