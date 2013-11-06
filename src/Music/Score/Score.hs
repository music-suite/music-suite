
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    DeriveDataTypeable,
    StandaloneDeriving,

    ViewPatterns,
    TypeFamilies,

    -- For Newtype
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
  ) where

import Data.Dynamic
import Control.Newtype                
import Data.Ord
import Data.Semigroup
import Data.Pointed
import Control.Applicative
import Control.Monad
import Control.Monad.Compose

import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Test.QuickCheck (Arbitrary(..), Gen(..))

import Data.Typeable
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal   
import Music.Score.Note
import Music.Score.Pitch
import Music.Score.Util


newtype Score a = Score { getScore :: (MScore, NScore a) }
    deriving (Functor, Semigroup, Monoid, Foldable, Traversable, Typeable)

type instance Container (Score a) = Score
type instance Event (Score a)     = a

instance Newtype (Score a) (MScore, NScore a) where
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

perform' :: Score a -> [Note a]
perform' = List.sortBy (comparing $ fst . getNote) . getNScore . snd . getScore

instance Performable (Score a) where
    perform = fmap ((\(delta -> (t,d),x) -> (t,d,x)) . getNote) . perform'
instance Composable (Score a) where

-- FIXME Reversible instance

    
{-
    TODO how to extract meta-events
    Meta-events give us the possibility to annotate spans in the score with various attributes

    Basically, for each score there is always a "initial" value valid from -Inifinity
    There may be any number of "updates" which last until the next update and so on
    
    More formally, for each attribute type there is a function

        getMeta :: Score b -> (a, [(Time, a)]) -- This is essentially a Reactive

    Note that when using join, the meta-events of each note merge with all simultaneous notes and
    the default value (XXX does this mean that the merge op must be commutative?). Compare the
    (lifted) Monoid instance for Reactive.

    TODO optionally attack to part (use Maybe (Part b))

        API something like:

            addMeta :: (Typeable a, Semigroup a) => Span -> Maybe (Part b) -> a -> Score b -> Score b
            addMeta = addMetaWith (<>)
            
            addMetaWith :: Typeable a => (a -> a -> a) -> Span -> Maybe (Part b) -> a -> Score b -> Score b
            
            getMeta :: Typeable a => a -> Maybe (Part a) -> Score b -> Reactive a
            getMeta whitness score = ...
-}




type MScore = NScore Dynamic -- or similar

-- TODO convert to something more friendly
meta :: Score a -> MScore 
meta = fst . getScore


-- |
-- Score without meta-events.
--
-- Semantics: a list of 'Note'. The semantics of each instances follow the instances of
-- the semantics.
-- 
newtype NScore a = NScore { getNScore :: [Note a] } -- sorted
    deriving (Functor, Foldable, Semigroup, Monoid, Traversable, Delayable, Stretchable, HasOnset, HasOffset)

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


-- The following instances allow us to write expressions like [c..g]

instance IsPitch a => IsPitch (Score a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Score a) where
    fromDynamics = pure . fromDynamics

instance Enum a => Enum (Score a) where
    toEnum = return . toEnum
    fromEnum = list 0 (fromEnum . head) . F.toList

-- Bogus VectorSpace instance, so we can use c^*2 etc.

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
    getPitches as    = F.foldMap getPitches as
    modifyPitch f    = fmap (modifyPitch f)

