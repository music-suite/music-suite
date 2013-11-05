
{-# LANGUAGE

    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    DeriveDataTypeable,
    StandaloneDeriving,

    ViewPatterns,
    TypeFamilies, -- Debug

    MultiParamTypeClasses,
    FlexibleInstances       -- for Newtype
    #-}

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

import Data.Semigroup
import Control.Newtype                
import Data.Dynamic
import Data.Pointed
import Control.Monad
import Control.Applicative
import Data.AffineSpace.Point
import Data.Set (Set)
import Data.Map (Map)
import Data.Typeable
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Set as Set
import qualified Data.Map as Map

import Music.Score.Pitch
import Music.Score.Util
import Music.Time
import Music.Score.Note

import Music.Pitch.Literal
import Music.Dynamics.Literal   


import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Test.QuickCheck          (Arbitrary(..), Gen(..))

asScore :: Score a -> Score a
asScore x = x

perform' :: Score a -> [Note a]
perform' = getNScore . snd . getScore

instance Performable (Score a) where
    perform = fmap ((\(delta -> (t,d),x) -> (t,d,x)) . getNote) . perform'

instance Composable (Score a) where

-- FIXME Reversible instance

-- TODO convert to something more friendly
meta :: Score a -> MScore 
meta = fst . getScore

-- Possible implementations for Score
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

-- etc

-- instance Delayable
-- instance Stretchable
-- instance HasOnset
-- instance HasOffset
-- instance HasDuration
-- instance Traversable
-- instance MonadPlus

    
type MScore = NScore Dynamic -- or similar
-- instance Delayable
-- instance Stretchable
-- instance HasOnset
-- instance HasOffset
-- instance HasDuration
-- instance Monoid -- !!!!

{-
    TODO how to extract meta-events
    Meta-events give us the possibility to annotate spans in the score with various attributes
    Each overlapping region composes type-wise using the relevant monoid
    
    Some examples:
        (First TimeSignature) uses the outermost time signature, and mempty if none applies
        (Last (Option Clef)) uses the innermost clef type, and the default if none applies
        [String] concatenates strings (useful for names)

        Extract is as a (Map Span Dynamic)
-}


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
    -- Functor
    -- MonadPlus
    -- Traversable





-- These instances allow us to write expressions like [c..g]

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


-- TODO move

-- | Intution:
-- Starts off with                      m (n (m (n a)))
-- Sequences inner structure to get     m (m (n (n a)))
-- Folds outer level to get             m (n (n a))
-- Folds inner level to get             m (n a)
mjoin :: (Monad m, Monad n, Functor m, Traversable n) => m (n (m (n a))) -> m (n a)
mjoin = fmap join . join . fmap T.sequence

mbind :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mbind = (join .) . fmap . (fmap join .) . T.mapM


