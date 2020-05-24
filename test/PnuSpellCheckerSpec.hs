module PnuSpellCheckerSpec where

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
fixTypos = foldl fixTypo where
    fixTypo :: T.Text -> Typo -> T.Text
    fixTypo text aTypo = T.replace (token aTypo) (head (suggestions aTypo)) text

sampleFile          = "test/sample.utf-8.txt"
sampleText          = T.pack "안녕  하세요.자줏 빛 합니다.호호  하세요. 삐리릿!"
sampleTextFixed     = T.pack "안녕  하세요. 자줏 빛 합니다.호호  하세요. 삐리릭!"
sampleTextCorrect   = T.pack "안녕하세요."
sampleTextLn        = T.pack "\n"

spec :: Spec
spec = do
    describe "spellCheckByPnu correct sample text test" $
        it "returns 0 typos" $ do
            typos <- liftIO (spellCheckByPnu sampleTextCorrect)
            length typos `shouldBe` 0

            Just typos' <- runMaybeT $ spellCheckByPnu sampleTextCorrect
            typos `shouldBe` typos'

            typos <- liftIO (spellCheckByPnu sampleTextLn)
            length typos `shouldBe` 0

            Just typos' <- runMaybeT $ spellCheckByPnu sampleTextLn
            [] `shouldBe` typos'

    describe "spellCheckByPnu sample text test" $
        it "returns 4 typos" $ do
            typos <- liftIO (spellCheckByPnu sampleText)
            length typos `shouldBe` 4

            Just typos' <- runMaybeT $ spellCheckByPnu sampleText
            typos `shouldBe` typos'

            let fixed = fixTypos sampleText typos
            fixed `shouldBe` sampleTextFixed

    describe "spellCheckByPnu sample file test" $
        it "returns more than 30 typos" $ do
            content <- liftIO $ I.readFile sampleFile
            -- PNU server is too slow. So we take only 8.
            let texts = take 8 $ linesByLength pnuSpellCheckerMaxWords content

            -- typos :: [Typo]
            typos <- liftIO $ concat <$> mapConcurrently spellCheckByPnu texts
            length typos `shouldSatisfy` (>30)

            -- typos' :: Maybe [[Typo]]
            typos' <- liftIO $ mapConcurrently (runMaybeT . spellCheckByPnu) texts
            let Just typos'' = sequenceA typos'
            typos `shouldBe` concat typos''