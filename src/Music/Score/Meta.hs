
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- 
--
-------------------------------------------------------------------------------------


module Music.Score.Meta (
        TimeSignature,
        KeySignature,
        Tempo
  ) where

import Data.Void
import Data.Maybe
import Data.Foldable
import Data.Typeable
import Data.Semigroup
import Control.Monad.Plus       
import qualified Data.List          as List
import qualified Data.List.NonEmpty as NonEmpty

import Music.Time
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Combinators

type TimeSignature = ([Integer], Integer)

type KeySignature = (Integer, Bool)

type Tempo = Duration



