module DaumSpellCheckerSpec where

import Test.Hspec

import Data.List
import Data.List.Split
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Concurrent.Async

import Language.Hanspell.Typo
import Language.Hanspell.TextLines
import Language.Hanspell.DaumSpellChecker

sampleFile        :: String
sampleFile        = "test/long-paragraph-sample.txt"

sampleText, sampleTextCorrect, sampleTextEmpty, sampleTextLn :: String
sampleText        = "안녕  하세요.자줏 빛 합니다.호호  하세요. 삐리릿!"
sampleTextCorrect = "안녕하세요."
sampleTextEmpty   = ""
sampleTextLn      = "\n"

-- runMaybeT 결과가 Nothing일 수도 있으므로 안전하게 풀어준다.
runOrEmpty :: MaybeT IO [Typo] -> IO [Typo]
runOrEmpty m = do
    r <- runMaybeT m
    return $ maybe [] id r

spec :: Spec
spec = do
    describe "spellCheckByDaum 무오류 입력" $
        it "returns 0 typos" $ do
            typos <- liftIO (spellCheckByDaum sampleTextEmpty)
            length typos `shouldBe` 0

            typos' <- liftIO . runOrEmpty $ spellCheckByDaum sampleTextEmpty
            typos `shouldBe` typos'

            typos2 <- liftIO (spellCheckByDaum sampleTextCorrect)
            length typos2 `shouldBe` 0

            typos2' <- liftIO . runOrEmpty $ spellCheckByDaum sampleTextCorrect
            typos2 `shouldBe` typos2'

            typos3 <- liftIO (spellCheckByDaum sampleTextLn)
            length typos3 `shouldBe` 0

    describe "spellCheckByDaum 오타 포함 입력" $
        it "최소 1건 이상의 오타를 잡아낸다" $ do
            liftIO $ threadDelay 500000  -- 다음 서버 rate limit 회피
            typos <- liftIO (spellCheckByDaum sampleText)
            length typos `shouldSatisfy` (>= 1)
            mapM_ (\t -> do
                       token t       `shouldSatisfy` (not . null)
                       suggestions t `shouldSatisfy` (not . null)
                ) typos

    describe "spellCheckByDaum 긴 문서 분할 검사" $
        it "분할된 청크 각각에서 오타를 모은다" $ do
            content <- liftIO $ readFile sampleFile
            let texts = linesByLength daumSpellCheckerMaxChars content
            length texts `shouldSatisfy` (>= 1)
            typos <- liftIO $ concat <$> mapConcurrently spellCheckByDaum texts
            length typos `shouldSatisfy` (>= 1)
