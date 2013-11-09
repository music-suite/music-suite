
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds, 
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
-- Provides the 'Score' type.
--
-------------------------------------------------------------------------------------

module Music.Score.Score (
        -- * Score type
        Score,
        renderScore,
        getScoreMeta,
        setScoreMeta,
  ) where

import Data.Dynamic
import Control.Newtype                
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Data.Pointed
import Control.Arrow
import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Plus
import Control.Monad.Compose

import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Test.QuickCheck (Arbitrary(..), Gen(..))

import Data.Default
import Data.Typeable
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
import Music.Pitch.Literal
import Music.Dynamics.Literal   
import Music.Score.Note
import Music.Score.Meta
import Music.Score.Pitch
import Music.Score.Part
import Music.Score.Util

newtype Score a = Score { getScore :: (Meta, NScore a) }
    deriving (Functor, Semigroup, Monoid, Foldable, Traversable, Typeable)

inScore f = Score . f . getScore

renderScore :: Score a -> Score (Note a)
renderScore = inScore (second (renderNScore))

type instance Container (Score a) = Score
type instance Event (Score a)     = a

instance Newtype (Score a) (Meta, NScore a) where
    pack = Score
    unpack = getScore

instance Monad Score where
    return = pack . return . return
    xs >>= f = pack $ mbind (unpack . f) (unpack xs)

instance Pointed Score where
    point = return

instance Applicative Score where
    pure = return
    (<*>) = ap

instance MonadPlus Score where
    mzero = mempty
    mplus = mappend

instance HasOnset (Score a) where
    onset (Score (m,x)) = onset x

instance HasOffset (Score a) where
    offset (Score (m,x)) = offset x

instance Delayable (Score a) where
    delay n (Score (m,x)) = Score (delay n m, delay n x)

instance Stretchable (Score a) where
    stretch n (Score (m,x)) = Score (stretch n m, stretch n x)

instance HasDuration (Score a) where
    duration = durationDefault

renderScore' :: Score a -> [Note a]
renderScore' = renderNScore' . snd . getScore


renderNScore' :: NScore a -> [Note a]
renderNScore' = List.sortBy (comparing $ getNoteSpan) . getNScore

instance Performable (Score a) where
    perform = fmap ((\(delta -> (t,d),x) -> (t,d,x)) . getNote) . renderScore'
instance Composable (Score a) where

instance Reversible a => Reversible (Score a) where
    rev = fmap rev . withSameOnset (stretch (-1))

instance HasMeta (Score a) where
    applyMeta n (Score (m,x)) = Score (applyMeta n m,x)

-- | Set the meta information of a score.
-- TODO more generic version
setScoreMeta :: Meta -> Score a -> Score a
setScoreMeta m (Score (_,a)) = Score (m,a)

-- | Get the meta information of a score.
-- TODO more generic version
getScoreMeta :: Score a -> Meta
getScoreMeta (Score (m,_)) = m


-- |
-- Score without meta-events.
--
-- Semantics: a list of 'Note'. The semantics of each instances follow the instances of
-- the semantics.
-- 
newtype NScore a = NScore { getNScore :: [Note a] } -- sorted
    deriving (Functor, Foldable, Semigroup, Monoid, Traversable, Delayable, Stretchable, HasOnset, HasOffset)

inNScore f = NScore . f . getNScore

renderNScore :: NScore a -> NScore (Note a)
renderNScore = inNScore $ fmap duplicate

instance Newtype (NScore a) [Note a] where
    pack = NScore
    unpack = getNScore

instance Monad NScore where
    return = pack . return . return
    xs >>= f = pack $ mbind (unpack . f) (unpack xs)

instance Applicative NScore where
    pure = return
    (<*>) = ap

instance MonadPlus NScore where
    mzero = mempty
    mplus = mappend

instance HasDuration (Note a) where
    duration = durationDefault


-- The following instances allow us to write expressions like [c..g]

instance IsPitch a => IsPitch (Score a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Score a) where
    fromDynamics = pure . fromDynamics

instance Enum a => Enum (Score a) where
    toEnum = return . toEnum
    fromEnum = list 0 (fromEnum . head) . F.toList


-- Bogus VectorSpace instance, so we can use c^*2 etc.
-- If you hate this instance, please open an issue.

instance AdditiveGroup (Score a) where
    zeroV   = error "Not impl"
    (^+^)   = error "Not impl"
    negateV = error "Not impl"

instance VectorSpace (Score a) where
    type Scalar (Score a) = Duration
    d *^ s = d `stretch` s



instance Arbitrary a => Arbitrary (Score a) where
    arbitrary = do
        x <- arbitrary
        t <- fmap realToFrac (arbitrary::Gen Double)
        d <- fmap realToFrac (arbitrary::Gen Double)
        return $ delay t $ stretch d $ return x

instance HasPitch a => HasPitch (Score a) where
    type Pitch (Score a) = Pitch a
    getPitches      = F.foldMap getPitches
    modifyPitch f   = fmap (modifyPitch f)

instance HasPart a => HasPart (Score a) where
    type Part (Score a) = Part a
    -- FIXME strange
    getPart         = fromMaybe def . fmap getPart . listToMaybe . F.toList
    modifyPart f    = fmap (modifyPart f)
