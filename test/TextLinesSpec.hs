module TextLinesSpec where

import Test.Hspec
import Test.QuickCheck

import Language.Hanspell

sampleText          = "12\t45\n12  56\n1234567"
sampleTextLn        = "12\t45\n12  56\n1234567\n"
sampleTextUtf8      = "안녕하세요.\n반갑습니다."
sampleTextUtf8Ln    = "안녕하세요.\n반갑습니다.\n"

spec :: Spec
spec = do
    describe "linesByLength properties" $ do
        it "almost always returns 1" $
            property $ \text ->
                let inText = text
                    inLength = length inText + 1
                    outTexts = linesByLength inLength inText
                 in length outTexts == 1 - if null text then 1 else 0
        it "is almost always same" $
            property $ \text len ->
                let outText = concat $ linesByLength len text
                 in outText == text || outText == (text ++ "\n")

    describe "linesByLength basic tests" $ do
        it "returns []" $
            let inText = ""
                outTexts = linesByLength 0 inText
             in outTexts `shouldBe` []
        it "returns []" $
            let inText = ""
                outTexts = linesByLength (-100) inText
             in outTexts `shouldBe` []
        it "returns 1 element" $
            let inText = "\n"
                outTexts = linesByLength (-1) inText
             in outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = "\n"
                outTexts = linesByLength 0 inText
             in outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = "\n"
                outTexts = linesByLength 1 inText
             in outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = "\n\n"
                outTexts = linesByLength 2 inText
             in outTexts `shouldBe` ["\n\n"]
        it "returns 2 elements" $
            let inText = "\n\n"
                outTexts = linesByLength 1 inText
             in outTexts `shouldBe` ["\n","\n"]
        it "returns 2 elements" $
            let inText = "\n\n"
                outTexts = linesByLength (-1) inText
             in outTexts `shouldBe` ["\n","\n"]

    describe "linesByLength sample tests" $ do
        it "returns 3 elements" $
            linesByWordCount 0 sampleText
                `shouldBe` [ "12\t45\n"
                           , "12  56\n"
                           , "1234567\n"
                           ]
        it "returns 2 elements" $
            linesByLength 13 sampleText
                `shouldBe` [ "12\t45\n12  56\n"
                           , "1234567\n"
                           ]
        it "returns 1 element" $
            linesByLength 30 sampleText `shouldBe` [ sampleTextLn ]
        it "returns 2 elements" $
            linesByLength (length sampleText) sampleText
                `shouldBe` [ "12\t45\n12  56\n"
                           , "1234567\n"
                           ]
        it "returns 1 element" $
            linesByLength (length sampleTextLn) sampleTextLn
                `shouldBe` [ sampleTextLn ]
        it "returns 1 element" $
            linesByLength (length sampleText + 1) sampleText
                `shouldBe` [ sampleTextLn ]

    describe "linesByLength utf-8 sample tests" $ do
        it "returns 2 elements" $
            linesByWordCount 0 sampleTextUtf8
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 2 elements" $
            linesByLength 7 sampleTextUtf8
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 2 elements" $
            linesByLength 13 sampleTextUtf8
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 1 element" $
            linesByLength (length sampleTextUtf8 + 1) sampleTextUtf8
                `shouldBe` [ sampleTextUtf8Ln ]
        it "returns 1 element" $
            linesByLength 14 sampleTextUtf8 `shouldBe` [ sampleTextUtf8Ln ]
        it "returns 1 element" $
            linesByLength 30 sampleTextUtf8 `shouldBe` [ sampleTextUtf8Ln ]

    describe "linesByWordCount properties" $ do
        it "almost always returns 1" $
            property $ \text ->
                let inText = text
                    wordCount = length (words text)
                    outTexts = linesByWordCount wordCount inText
                 in length outTexts == 1 - if null text then 1 else 0
        it "is almost always same" $
            property $ \text len ->
                let outText = concat $ linesByWordCount len text
                 in outText == text || outText == (text ++ "\n")

    describe "linesByWordCount basic tests" $ do
        it "returns []" $
            let inText = ""
                outTexts = linesByWordCount 0 inText
             in outTexts `shouldBe` []
        it "returns []" $
            let inText = ""
                outTexts = linesByWordCount (-100) inText
             in outTexts `shouldBe` []
        it "returns 1 element" $
            let inText = "\n"
                outTexts = linesByWordCount (-1) inText
             in outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = "\n"
                outTexts = linesByWordCount 0 inText
             in outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = "\n"
                outTexts = linesByWordCount 1 inText
             in outTexts `shouldBe` ["\n"]
        it "returns 1 element" $
            let inText = "\n\n"
                outTexts = linesByWordCount 0 inText
             in outTexts `shouldBe` ["\n\n"]
        it "returns 1 element" $
            let inText = "\n\n"
                outTexts = linesByWordCount 1 inText
             in outTexts `shouldBe` ["\n\n"]
        it "returns 1 element" $
            let inText = "\n\n"
                outTexts = linesByWordCount 2 inText
             in outTexts `shouldBe` ["\n\n"]
        it "returns 2 elements" $
            let inText = "\n\n"
                outTexts = linesByWordCount (-1) inText
             in outTexts `shouldBe` ["\n","\n"]

    describe "linesByWordCount sample tests" $ do
        it "returns 3 elements" $
            linesByWordCount 0 sampleText
                `shouldBe` [ "12\t45\n"
                           , "12  56\n"
                           , "1234567\n"
                           ]
        it "returns 2 elements" $
            linesByWordCount 3 sampleText
                `shouldBe` [ "12\t45\n"
                           , "12  56\n1234567\n"
                           ]
        it "returns 2 elements" $
            linesByWordCount 4 sampleText
                `shouldBe` [ "12\t45\n12  56\n"
                           , "1234567\n"
                           ]
        it "returns 1 element" $
            linesByWordCount (length (words sampleText)) sampleText
                `shouldBe` [ sampleTextLn ]

    describe "linesByWordCount utf-8 sample tests" $ do
        it "returns 2 elements" $
            linesByWordCount 0 sampleTextUtf8
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 2 elements" $
            linesByWordCount 1 sampleTextUtf8
                `shouldBe` [ "안녕하세요.\n"
                           , "반갑습니다.\n"
                           ]
        it "returns 1 element" $
            linesByWordCount 2 sampleTextUtf8
                `shouldBe` [ sampleTextUtf8Ln ]
