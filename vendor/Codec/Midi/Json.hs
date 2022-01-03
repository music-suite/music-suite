{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provides JSON encoding of `Midi` data.
module Codec.Midi.Json (encode) where

import Codec.Midi (Midi, Message, TimeDiv, FileType)
import Codec.Midi.Generic ()
import Data.Aeson (ToJSON(..))
import Data.ByteString.Lazy (ByteString)
import Data.Aeson.Encode.Pretty (encodePretty)

instance ToJSON Midi

instance ToJSON Message

instance ToJSON TimeDiv

instance ToJSON FileType

-- | This is here to so that @ToJSON Message@ can be derived using defaults. Please do not use.
--
-- Better solutions: remove this instance and write a proper encoder
-- for @Message@, or use a Midi type without @Sysex@ and @Reserved@.
instance ToJSON ByteString where
  -- Unique number to make this easy to find
  toJSON = error "Fake instance, grep for 238495723058047239"

encode :: Midi -> ByteString
encode = encodePretty
