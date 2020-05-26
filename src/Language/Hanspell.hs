-- | 'Language.Hanspell' defines the interfaces of Pusan National University 
-- spell check service and DAUM spell check service.
--
-- The code below explains simple usecase of 'spellCheckByDaum' or 
-- 'spellCheckByPnu'.
--
-- @
-- import Language.Hanspell
-- 
-- main = do
--     let sentence = "위에계신분, 잘들리세요?"
--     let correctSentence = "위에 계신 분, 잘 들리세요?"
--     typos <- spellCheckByDaum sentence
--     let fixedSentence = fixTyposWithStyle False sentence typos
--     putStrLn . show $ fixedSentence == correctSentence
-- @
--
-- @hanspell@, a simple command line program and this library are packaged 
-- together. Please visit <https://github.com/9beach/hanspell-hs>.
module Language.Hanspell (module Hanspell) where

import Language.Hanspell.Typo as Hanspell
import Language.Hanspell.DaumSpellChecker as Hanspell
import Language.Hanspell.PnuSpellChecker as Hanspell
import Language.Hanspell.TextLines as Hanspell
import Language.Hanspell.Glob as Hanspell
