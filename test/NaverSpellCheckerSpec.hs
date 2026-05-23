module NaverSpellCheckerSpec where

import Test.Hspec

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)

import Language.Hanspell.Typo
import Language.Hanspell.TextLines
import Language.Hanspell.NaverSpellChecker

sampleFile        :: String
sampleFile        = "test/long-paragraph-sample.txt"

emptyText, lnText, correctText, typoText :: String
emptyText   = ""
lnText      = "\n"
correctText = "안녕하세요."
typoText    = "안뇽하세요. 정교수측이 추천한 영화. 곤색이다."

-- Naver는 호출 사이에 짧은 인터벌을 두는 게 안전.
delay :: IO ()
delay = threadDelay (naverSpellCheckerMinIntervalMicros)

spec :: Spec
spec = do
    describe "spellCheckByNaver empty/correct" $
        it "returns 0 typos" $ do
            typos <- liftIO (spellCheckByNaver emptyText)
            length typos `shouldBe` 0
            delay

            typos' <- liftIO (spellCheckByNaver correctText)
            length typos' `shouldBe` 0
            delay

            typos'' <- liftIO (spellCheckByNaver lnText)
            length typos'' `shouldBe` 0

    describe "spellCheckByNaver detects typos" $
        it "returns at least 1 typo with expected fields" $ do
            delay
            typos <- liftIO (spellCheckByNaver typoText)
            length typos `shouldSatisfy` (>= 1)
            mapM_ (\t -> do
                       token t       `shouldSatisfy` (not . null)
                       suggestions t `shouldSatisfy` (\ss -> length ss == 1)
                ) typos

    describe "spellCheckByNaver MaybeT and IO instances agree" $
        it "returns same typos" $ do
            delay
            typos <- liftIO (spellCheckByNaver typoText)
            delay
            Just typos' <- runMaybeT $ spellCheckByNaver typoText
            typos `shouldBe` typos'
