-- | Match Glob patterns by convertng them to Regular Expressions.
-- From the chapter 8 of Real World Haskell.
module Hanspell.Glob (matchGlob, matchGlobs) where

import Text.Regex

-- Converts a Glob Expression into a Regular Expression, anchor it to the
-- beginning and end of the line
globToRegex :: String -> String
globToRegex globex = '^' : globToRegex' globex ++ "$"

-- | Checks if a name matches a glob pattern by converting that glob 
-- pattern to a regular expression and matching using that.
matchGlob :: String -> String -> Bool
matchGlob pattern name = 
    case matchRegex (mkRegex (globToRegex pattern)) name of 
        Nothing -> False
        _ -> True

-- | Checks if a name matches glob patterns by converting that glob 
-- pattern to a regular expression and matching using that.
matchGlobs :: [String] -> String -> Bool
matchGlobs patterns name = 
    or . map (flip matchGlob name) $ patterns

-- | Finds glob specific characters, and convert them to regex specific
-- characters, escapes regex specific characters and verify that character
-- classes are properly terminated
globToRegex' :: String -> String
globToRegex' ""             = ""
globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
globToRegex' ('?':cs)       = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '[' : c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"
globToRegex' (c:cs)         = escape c ++ globToRegex' cs

-- | Escapes regex characters.
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
    where regexChars = "\\+()^$.{}]"

-- | Verifies character classes are terminated.
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"
