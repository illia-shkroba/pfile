{- |
Module:      PFile.Aeson
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Helper functions for 'Data.Aeson'.
-}

{-# LANGUAGE OverloadedStrings #-}

module PFile.Aeson
  ( encodePretty
  ) where

import           Data.Aeson               (ToJSON)
import qualified Data.Aeson.Encode.Pretty as Aeson
import           Protolude

-- | Wrapper over 'Data.Aeson.Encode.Pretty.encodePretty' that returns 'Text'
-- instead of 'ByteString'.
--
-- @since 0.1.0.0
encodePretty :: ToJSON a => a -> Text
encodePretty
  = either (\error -> "<Utf8 decoding error: " <> show error <> ">") identity
  . decodeUtf8' . toS . Aeson.encodePretty
