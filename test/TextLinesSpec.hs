module TextLinesSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Text as T

import Hanspell

sampleText          = T.pack "12\t45\n12  56\n1234567"
sampleTextLn        = T.pack "12\t45\n12  56\n1234567\n"
sampleTextUtf8      = T.pack "안녕하세요.\n반갑습니다."
sampleTextUtf8Ln    = T.pack "안녕하세요.\n반갑습니다.\n"

spec :: Spec
spec = do
    describe "linesByLength properties" $ do
        it "almost always returns 1" $
            property $ \text ->
                let inText = T.pack text
                    inLength = T.length inText + 1
                    outTexts = linesByLength inLength inText
                 in length outTexts == 1 - if null text then 1 else 0
        it "is almost always same" $
            property $ \text len ->
                let outText = concatMap T.unpack . linesByLength len $
                              T.pack text
                 in outText == text || outText == (text ++ "\n")

    describe "linesByLength basic tests" $ do
        it "returns []" $
            let inText = T.pack ""
                outTexts = linesByLength 0 inText
             in outTexts `shouldBe` []
        it "returns []" $
            let inText = T.pack ""
                outTexts = linesByLength (-100) inText
             in outTexts `shouldBe` []
        it "returns 1 element" $
            let inText = T.pack "\n"
                outTexts = linesByLength (-1) inText
             in map T.unpack outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = T.pack "\n"
                outTexts = linesByLength 0 inText
             in map T.unpack outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = T.pack "\n"
                outTexts = linesByLength 1 inText
             in map T.unpack outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = T.pack "\n\n"
                outTexts = linesByLength 2 inText
             in map T.unpack outTexts `shouldBe` ["\n\n"]
        it "returns 2 elements" $
            let inText = T.pack "\n\n"
                outTexts = linesByLength 1 inText
             in map T.unpack outTexts `shouldBe` ["\n","\n"]
        it "returns 2 elements" $
            let inText = T.pack "\n\n"
                outTexts = linesByLength (-1) inText
             in map T.unpack outTexts `shouldBe` ["\n","\n"]

    describe "linesByLength sample tests" $ do
        it "returns 3 elements" $
            map T.unpack (linesByWordCount 0 sampleText)
                `shouldBe` [ "12\t45\n"
                           , "12  56\n"
                           , "1234567\n"
                           ]
        it "returns 2 elements" $
            map T.unpack (linesByLength 13 sampleText)
                `shouldBe` [ "12\t45\n12  56\n"
                           , "1234567\n"
                           ]
        it "returns 1 element" $
            linesByLength 30 sampleText `shouldBe` [ sampleTextLn ]
        it "returns 2 elements" $
            map T.unpack (linesByLength (T.length sampleText) sampleText)
                `shouldBe` [ "12\t45\n12  56\n"
                           , "1234567\n"
                           ]
        it "returns 1 element" $
            linesByLength (T.length sampleTextLn) sampleTextLn
                `shouldBe` [ sampleTextLn ]
        it "returns 1 element" $
            linesByLength (T.length sampleText + 1) sampleText
                `shouldBe` [ sampleTextLn ]

    describe "linesByLength utf-8 sample tests" $ do
        it "returns 2 elements" $
            map T.unpack (linesByWordCount 0 sampleTextUtf8)
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 2 elements" $
            map T.unpack (linesByLength 7 sampleTextUtf8)
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 2 elements" $
            map T.unpack (linesByLength 13 sampleTextUtf8)
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 1 element" $
            linesByLength (T.length sampleTextUtf8 + 1) sampleTextUtf8
                `shouldBe` [ sampleTextUtf8Ln ]
        it "returns 1 element" $
            linesByLength 14 sampleTextUtf8 `shouldBe` [ sampleTextUtf8Ln ]
        it "returns 1 element" $
            linesByLength 30 sampleTextUtf8 `shouldBe` [ sampleTextUtf8Ln ]

    describe "linesByWordCount properties" $ do
        it "almost always returns 1" $
            property $ \text ->
                let inText = T.pack text
                    wordCount = length (words text)
                    outTexts = linesByWordCount wordCount inText
                 in length outTexts == 1 - if null text then 1 else 0
        it "is almost always same" $
            property $ \text len ->
                let outText = concatMap T.unpack . linesByWordCount len $
                              T.pack text
                 in outText == text || outText == (text ++ "\n")

    describe "linesByWordCount basic tests" $ do
        it "returns []" $
            let inText = T.pack ""
                outTexts = linesByWordCount 0 inText
             in outTexts `shouldBe` []
        it "returns []" $
            let inText = T.pack ""
                outTexts = linesByWordCount (-100) inText
             in outTexts `shouldBe` []
        it "returns 1 element" $
            let inText = T.pack "\n"
                outTexts = linesByWordCount (-1) inText
             in map T.unpack outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = T.pack "\n"
                outTexts = linesByWordCount 0 inText
             in map T.unpack outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = T.pack "\n"
                outTexts = linesByWordCount 1 inText
             in map T.unpack outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = T.pack "\n\n"
                outTexts = linesByWordCount 0 inText
             in map T.unpack outTexts `shouldBe` ["\n\n"]
        it "returns 1 element" $
            let inText = T.pack "\n\n"
                outTexts = linesByWordCount 1 inText
             in map T.unpack outTexts `shouldBe` ["\n\n"]
        it "returns 1 element" $
            let inText = T.pack "\n\n"
                outTexts = linesByWordCount 2 inText
             in map T.unpack outTexts `shouldBe` ["\n\n"]
        it "returns 2 elements" $
            let inText = T.pack "\n\n"
                outTexts = linesByWordCount (-1) inText
             in map T.unpack outTexts `shouldBe` ["\n","\n"]

    describe "linesByWordCount sample tests" $ do
        it "returns 3 elements" $
            map T.unpack (linesByWordCount 0 sampleText)
                `shouldBe` [ "12\t45\n"
                           , "12  56\n"
                           , "1234567\n"
                           ]
        it "returns 2 elements" $
            map T.unpack (linesByWordCount 3 sampleText)
                `shouldBe` [ "12\t45\n"
                           , "12  56\n1234567\n"
                           ]
        it "returns 2 elements" $
            map T.unpack (linesByWordCount 4 sampleText)
                `shouldBe` [ "12\t45\n12  56\n"
                           , "1234567\n"
                           ]
        it "returns 1 element" $
            linesByWordCount (length (T.words sampleText)) sampleText
                `shouldBe` [ sampleTextLn ]

    describe "linesByWordCount utf-8 sample tests" $ do
        it "returns 2 elements" $
            map T.unpack (linesByWordCount 0 sampleTextUtf8)
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 2 elements" $
            map T.unpack (linesByWordCount 1 sampleTextUtf8)
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 1 element" $
            linesByWordCount 2 sampleTextUtf8
                `shouldBe` [ sampleTextUtf8Ln ]
