-- | This package includes @hanspell@, a simple command line spell check 
-- program and a library which defines the interfaces for Pusan National 
-- University spell check service and DAUM spell check service.
module Language.Hanspell (module Hanspell) where

import Language.Hanspell.PnuSpellChecker as Hanspell
import Language.Hanspell.DaumSpellChecker as Hanspell
import Language.Hanspell.Typo as Hanspell
