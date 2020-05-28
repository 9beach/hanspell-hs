{-# OPTIONS_HADDOCK hide #-}

module Language.Hanspell.Decoder (decodeEntity) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import HTMLEntities.Decoder
import Data.Text.Lazy.Builder

-- HTML entity decoder.
decodeEntity :: String -> String
decodeEntity = T.unpack . TL.toStrict . toLazyText . htmlEncodedText . T.pack
