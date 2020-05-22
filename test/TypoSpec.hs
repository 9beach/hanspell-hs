module TypoSpec where

import qualified Data.Text as T
import qualified Data.Text.IO as I

import Test.Hspec
import Test.QuickCheck
import Control.Monad.IO.Class

import Hanspell

info0 = T.pack ("동작성이 있는 일부 명사나 어근 뒤에 붙어, 그 동작성을 실현하는 "
             ++ "동사를 만들거나 또는 상태성이 있는 일부 명사나 어근 뒤에 ...\n")
text0 = T.pack "사랑 합시다.\n여러분, 반갑 습니다.\n"

token1 = T.pack "사랑 합시다"
suggestion1 = T.pack "사랑합시다"
typo1 = Typo (T.pack "space") token1 [suggestion1] T.empty info0

token2 = T.pack "안녕 하세요"
suggestion2 = T.pack "안녕하세요"
typo2 = Typo (T.pack "cont_gramma") token2 [suggestion2] T.empty info0

token3 = T.pack "반갑 습니다"
suggestion3 = T.pack "반갑습니다"
typo3 = Typo (T.pack "space") token3 [suggestion3,suggestion1] T.empty info0

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

    describe "typoToTextWithStyle tests" $ do
        it "just to check print output" $ do
            liftIO . I.putStr . typoToTextWithStyle $ typo1
            liftIO . I.putStr . typoToTextWithStyle $ typo3
            shouldBe 1 1

    describe "fixTyposWithStyle tests" $ do
        it "just to check print output" $ do
            liftIO . I.putStr . fixTyposWithStyle text0 $ [typo1,typo3]
            shouldBe 1 1
