import Test.Hspec

import TypoSpec
import TextLinesSpec
import DaumSpellCheckerSpec

main :: IO ()
main = hspec $ do
    describe "TextLines"        TextLinesSpec.spec
    describe "DaumSpellChecker" DaumSpellCheckerSpec.spec
    describe "Typo"             TypoSpec.spec
