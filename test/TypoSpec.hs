module TypoSpec where

import qualified Data.Text as T
import qualified Data.Text.IO as I

import Test.Hspec
import Test.QuickCheck
import Control.Monad.IO.Class

import Language.Hanspell

info0 = "동작성이 있는 일부 명사나 어근 뒤에 붙어, 그 동작성을 ...\n"
text0 = "사랑 합시다.\n여러분, 반갑 습니다.\n"

token1 = "사랑 합시다"
suggestion1 = "사랑합시다"
typo1 = Typo "space" token1 [suggestion1] "" info0

token2 = "안녕 하세요"
suggestion2 = "안녕하세요"
typo2 = Typo "cont_gramma" token2 [suggestion2] "" info0

token3 = "반갑 습니다"
suggestion3 = "반갑습니다"
typo3 = Typo "space" token3 [suggestion3,suggestion1] "" info0
typo3' = Typo "sp  ace" token3 [suggestion1] "" info0

typos1 = [typo3,typo1,typo3',typo3,typo2]
typos2 = [typo3,typo1,typo3',typo3,typo2,typo1]
typos3 = [typo3,typo1,typo2]

spec :: Spec
spec = do
    describe "rmdupTypos tests" $
        it "returns 3" $ do
            let typos1' = rmdupTypos typos1
                typos2' = rmdupTypos typos2
                typos3' = rmdupTypos typos3

            length typos1' `shouldBe` 3
            length typos2' `shouldBe` 3
            typos1' `shouldBe` typos3
            typos2' `shouldBe` typos3
            typos3' `shouldBe` typos3

    describe "typoToStringWithStyle tests" $
        it "just prints results" $ do
            liftIO . putStr . typoToStringWithStyle True $ typo1
            liftIO . putStr . typoToStringWithStyle True $ typo3
            shouldBe 1 1

    describe "fixTyposWithStyle tests" $
        it "just prints results" $ do
            liftIO . putStr . fixTyposWithStyle True text0 $ [typo1,typo3]
            shouldBe 1 1
