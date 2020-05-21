module TypoSpec where

import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck

import Hanspell

t1 = T.pack "t1-한글"
typo1 = Typo T.empty t1 [] T.empty T.empty

t2 = T.pack "t2-한글"
typo2 = Typo T.empty t2 [] T.empty T.empty

t3 = T.pack "t3-한글"
typo3 = Typo T.empty t3 [] T.empty T.empty

typos1 = [typo3,typo1,typo3,typo3,typo2]
typos2 = [typo3,typo1,typo3,typo3,typo2,typo1]
typos3 = [typo3,typo1,typo2]

spec :: Spec
spec = do
    describe "rmdupTypo tests" $ do
        it "returns 3" $ do
            let typos1' = rmdupTypo typos1
                typos2' = rmdupTypo typos2
                typos3' = rmdupTypo typos3

            length typos1' `shouldBe` 3
            length typos2' `shouldBe` 3
            typos1' `shouldBe` typos3
            typos2' `shouldBe` typos3
            typos3' `shouldBe` typos3
