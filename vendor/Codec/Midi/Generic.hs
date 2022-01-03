{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Provides `GHC.Generic` instances for `Midi`.
module Codec.Midi.Generic where

import GHC.Generics (Generic)
import Codec.Midi (Midi(..), Message(..), TimeDiv(..), FileType(..))

deriving instance Generic Midi

deriving instance Generic Message

deriving instance Generic TimeDiv

deriving instance Generic FileType

