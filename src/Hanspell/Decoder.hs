{-# OPTIONS_HADDOCK hide #-}

module Hanspell.Decoder where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import HTMLEntities.Decoder
import Data.Text.Lazy.Builder

-- HTML entity decoder.
decodeEntity :: T.Text -> T.Text
decodeEntity = TL.toStrict . toLazyText . htmlEncodedText 
