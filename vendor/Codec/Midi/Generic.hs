{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Provides `GHC.Generic` instances for `Midi`.
module Codec.Midi.Generic where

import GHC.Generics (Generic)
import Codec.Midi (Midi(..))

deriving instance Generic Midi

