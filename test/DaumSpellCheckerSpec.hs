module DaumSpellCheckerSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Text as T
import qualified Data.Text.IO as I

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Concurrent.Async

import Hanspell

fixTypos :: T.Text -> [Typo] -> T.Text
fixTypos text typos = foldl fixTypo text typos where
    fixTypo :: T.Text -> Typo -> T.Text
    fixTypo text aTypo = T.replace (token aTypo) (suggestions aTypo!!0) text

sampleFile          = "test/sample.utf-8.txt"

sampleText          = T.pack "안녕  하세요.자줏 빛 합니다.호호  하세요. 삐리릿!"
sampleTextFixed     = T.pack "안녕하세요. 자주 빛 합니다. 호호하세요. 삐리릿!"
sampleTextCorrect   = T.pack "안녕하세요."

sampleTextLn        = T.pack "\n"

spec :: Spec
spec = do
    describe "spellCheckByDaum correct sample text test" $ do
        it "returns 0 typos" $ do
            typos <- liftIO (spellCheckByDaum sampleTextCorrect)
            length typos `shouldBe` 0

            Just typos' <- runMaybeT $ spellCheckByDaum sampleTextCorrect
            typos `shouldBe` typos'

            typos <- liftIO (spellCheckByDaum sampleTextLn)
            length typos `shouldBe` 0

            Just typos' <- runMaybeT $ spellCheckByDaum sampleTextLn
            typos `shouldBe` typos'

    describe "spellCheckByDaum sample text test" $ do
        it "returns 3 typos" $ do
            typos <- liftIO (spellCheckByDaum sampleText)
            length typos `shouldBe` 3

            Just typos' <- runMaybeT $ spellCheckByDaum sampleText
            typos `shouldBe` typos'

            let fixed = fixTypos sampleText typos
            fixed `shouldBe` sampleTextFixed

    describe "spellCheckByDaum sample file test" $ do
        it "returns more than 20 typos" $ do
            content <- liftIO $ I.readFile sampleFile
            let texts = linesByLength daumSpellCheckerMaxChars content
            length texts `shouldBe` 12

            -- typos :: [Typo]
            typos <- liftIO $ concat <$> mapConcurrently spellCheckByDaum texts
            length typos `shouldSatisfy` (>20)

            -- typos' :: Maybe [[Typo]]
            typos' <- liftIO $ mapConcurrently (runMaybeT . spellCheckByDaum) texts
            let Just typos'' = sequenceA typos'
            typos `shouldBe` (concat typos'')
