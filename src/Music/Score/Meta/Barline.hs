
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds,
    FlexibleContexts, 
    GADTs, 
    ViewPatterns,
    TypeFamilies,
    MultiParamTypeClasses, 
    FlexibleInstances #-}

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
-------------------------------------------------------------------------------------

module Music.Score.Meta.Barline (
        -- * Barline type
        BarlineType(..),
        Barline,

        -- ** Adding barlines to scores
        barline,
        doubleBarline,
        finalBarline,
        barlineDuring,
        
        -- ** Extracting barlines
        withBarline,
  ) where


import Control.Arrow
import Control.Monad.Plus       
import Data.Default
import Data.Void
import Data.Maybe
import Data.Semigroup
import Data.Monoid.WithSemigroup
import Data.Typeable
import Data.String
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Music.Time
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Meta
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Util
import Music.Pitch.Literal

-- | Represents a barline.
--
-- TODO repeats
data Barline = Barline BarlineType
    deriving (Eq, Ord, Show, Typeable)

data BarlineType = StandardBarline | DoubleBarline | FinalBarline
    deriving (Eq, Ord, Show, Typeable)

-- | Add a barline over the whole score.
barline :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Barline -> a -> a
barline c x = barlineDuring (era x) c x

doubleBarline = undefined
finalBarline = undefined

-- | Add a barline to the given score.
barlineDuring :: (HasMeta a, HasPart' a) => Span -> Barline -> a -> a
barlineDuring s c = addMetaNote (s =: (Option $ Just $ Last c))

-- | Extract barlines in from the given score, using the given default barline.
withBarline :: (Barline -> Score a -> Score a) -> Score a -> Score a
withBarline f = withGlobalMeta (maybe id f . fmap getLast . getOption)
