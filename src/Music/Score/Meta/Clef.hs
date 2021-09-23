{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides clefs as meta-data.
--
-- /Warning/ Experimental module.
module Music.Score.Meta.Clef
  ( -- * Clef type
    Clef (..),

    -- ** Adding clefs to scores
    clef,
    clefDuring,
  )
where

import Control.Lens (view)
import Control.Monad.Plus
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Typeable
import Music.Pitch.Literal
import Music.Score.Internal.Util
import Music.Score.Meta
import Music.Score.Part
import Music.Score.Pitch
import Music.Time
import Music.Time.Reactive

data Clef = GClef | CClef | FClef
  deriving (Eq, Ord, Show, Typeable)

instance IsPitch Clef where
  fromPitch l
    | l == c = CClef
    | l == f = FClef
    | otherwise = GClef

-- | Set clef of the given score.
clef :: (HasMeta a, HasPosition a) => Clef -> a -> a
clef c x = case _era x of
  Nothing -> x
  Just e -> clefDuring e c x

-- | Set clef of the given part of a score.
clefDuring :: HasMeta a => Span -> Clef -> a -> a
clefDuring s c = addMetaEvent $ view event (s, Just $ Last c)
