import Test.Hspec

import TypoSpec
import TextLinesSpec
import DaumSpellCheckerSpec
import NaverSpellCheckerSpec

main :: IO ()
main = hspec $ do
    describe "TextLines"         TextLinesSpec.spec
    describe "DaumSpellChecker"  DaumSpellCheckerSpec.spec
    describe "Typo"              TypoSpec.spec
    describe "NaverSpellChecker" NaverSpellCheckerSpec.spec
