module DaumSpellCheckerSpec where

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
import Language.Hanspell.DaumSpellChecker

fixTypos :: String -> [Typo] -> String
fixTypos = foldl fixTypo where
    replace from to = intercalate to . splitOn from
    fixTypo :: String -> Typo -> String
    fixTypo text aTypo = replace (token aTypo) (head (suggestions aTypo)) text

sampleFile          = "test/long-paragraph-sample.txt"
sampleText          = "안녕  하세요.자줏 빛 합니다.호호  하세요. 삐리릿!"
sampleTextFixed     = "안녕하세요. 자주 빛 합니다. 호호하세요. 삐리릿!"
sampleTextCorrect   = "안녕하세요."
sampleTextLn        = "\n"

spec :: Spec
spec = do
    describe "spellCheckByDaum correct sample text test" $
        it "returns 0 typos" $ do
            typos <- liftIO (spellCheckByDaum sampleTextCorrect)
            length typos `shouldBe` 0

            Just typos' <- runMaybeT $ spellCheckByDaum sampleTextCorrect
            typos `shouldBe` typos'

            typos <- liftIO (spellCheckByDaum sampleTextLn)
            length typos `shouldBe` 0

            Just typos' <- runMaybeT $ spellCheckByDaum sampleTextLn
            typos `shouldBe` typos'

    describe "spellCheckByDaum sample text test" $
        it "returns 3 typos" $ do
            typos <- liftIO (spellCheckByDaum sampleText)
            length typos `shouldBe` 3

            Just typos' <- runMaybeT $ spellCheckByDaum sampleText
            typos `shouldBe` typos'

            let fixed = fixTypos sampleText typos
            fixed `shouldBe` sampleTextFixed

    describe "spellCheckByDaum sample file test" $
        it "returns more than 10 typos" $ do
            content <- liftIO $ readFile sampleFile
            let texts = linesByLength daumSpellCheckerMaxChars content
            length texts `shouldBe` 3

            -- typos :: [Typo]
            typos <- liftIO $ concat <$> mapConcurrently spellCheckByDaum texts
            length typos `shouldSatisfy` (>10)

            -- typos' :: Maybe [[Typo]]
            typos' <- liftIO $ mapConcurrently (runMaybeT . spellCheckByDaum) texts
            let Just typos'' = sequenceA typos'
            typos `shouldBe` concat typos''
