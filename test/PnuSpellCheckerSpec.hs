module PnuSpellCheckerSpec where

import Test.Hspec
import Test.QuickCheck

import Data.List
import Data.List.Split
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Concurrent.Async

import Language.Hanspell.Typo
import Language.Hanspell.TextLines
import Language.Hanspell.PnuSpellChecker

fixTypos :: String -> [Typo] -> String
fixTypos = foldl fixTypo where
    replace from to = intercalate to . splitOn from
    fixTypo :: String -> Typo -> String
    fixTypo text aTypo = replace (token aTypo) (head (suggestions aTypo)) text

sampleFile          = "test/long-paragraph-sample.txt"
sampleText          = "안녕  하세요.자줏 빛 합니다.호호  하세요. 삐리릿!"
sampleTextFixed     = "안녕  하세요. 자줏 빛 합니다.호호  하세요. 삐리릭!"
sampleTextCorrect   = "안녕하세요."
sampleTextEmpty     = ""
sampleTextLn        = "\n"

spec :: Spec
spec = do
    describe "spellCheckByPnu correct sample text test" $
        it "returns 0 typos" $ do
            typos <- liftIO (spellCheckByPnu sampleTextEmpty)
            length typos `shouldBe` 0

            Just typos' <- runMaybeT $ spellCheckByPnu sampleTextEmpty
            typos `shouldBe` typos'

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
        it "returns more than 20 typos" $ do
            content <- liftIO $ readFile sampleFile
            let texts = linesByLength pnuSpellCheckerMaxWords content
            length texts `shouldBe` 3

            -- typos :: [Typo]
            typos <- liftIO $ concat <$> mapConcurrently spellCheckByPnu texts
            length typos `shouldSatisfy` (>20)

            -- typos' :: Maybe [[Typo]]
            typos' <- liftIO $ mapConcurrently (runMaybeT . spellCheckByPnu) texts
            let Just typos'' = sequenceA typos'
            typos `shouldBe` concat typos''
