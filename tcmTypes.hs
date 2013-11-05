
{-# LANGUAGE

    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    StandaloneDeriving,

    TypeFamilies, -- Debug

    MultiParamTypeClasses,
    FlexibleInstances       -- for Newtype
    #-}

import Music.Time
import Data.Semigroup
import Control.Newtype                
import Data.Dynamic
import Control.Monad
import Control.Applicative
import Data.AffineSpace.Point
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Set as Set
import qualified Data.Map as Map

asScore :: Score a -> Score a
asScore x = x

perform :: Score a -> [Note a]
perform = getNScore . snd . getScore


-- TODO convert to something more friendly
meta :: Score a -> MScore 
meta = fst . getScore

-- Possible implementations for Score
newtype Score a = Score { getScore :: (MScore, NScore a) }
    deriving (Functor, Semigroup, Monoid, Foldable, Traversable)
instance Newtype (Score a) (MScore, NScore a) where
    pack = Score
    unpack = getScore
instance Monad Score where
    return = pack . return . return
    xs >>= f = pack $ mbind (unpack . f) (unpack xs)
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

-- type NScore a = Free Note a
    -- are these equivalent?



newtype Note a = Note (Span, a)
    deriving (Eq, Ord, Show, {-Read, -}Functor, Applicative, Monad, Foldable, Traversable)

instance Delayable (Note a) where
    delay n (Note (s,x)) = Note (delay n s, x)
instance Stretchable (Note a) where
    stretch n (Note (s,x)) = Note (stretch n s, x)
instance HasOnset (Note a) where
    onset (Note (s,x)) = onset s
instance HasOffset (Note a) where
    offset (Note (s,x)) = offset s
    -- Functor
    -- Monad
    -- Traversable
    -- NOT MonaPlus



-- TODO move
instance HasOnset a => HasOnset [a] where
    onset = list origin (minimum . fmap onset)

instance HasOffset a => HasOffset [a] where
    offset = list origin (maximum . fmap offset)

instance HasOnset a => HasOnset (Set a) where
    onset = list origin (onset . head) . Set.toList

instance HasOffset a => HasOffset (Set a) where
    offset = list origin (offset . last) . Set.toList

instance HasOnset k => HasOnset (Map k a) where
    onset = list origin (onset . head) . Map.keys

instance HasOffset k => HasOffset (Map k a) where
    offset = list origin (offset . last) . Map.keys

-- instance HasDuration a => HasDuration [a] where
    -- duration = getSum . F.foldMap (Sum . duration)


list :: r -> ([a] -> r) -> [a] -> r
list z f [] = z
list z f xs = f xs








-- | Intution:
-- Starts off with                      m (n (m (n a)))
-- Sequences inner structure to get     m (m (n (n a)))
-- Folds outer level to get             m (n (n a))
-- Folds inner level to get             m (n a)
mjoin :: (Monad m, Monad n, Functor m, Traversable n) => m (n (m (n a))) -> m (n a)
mjoin = fmap join . join . fmap T.sequence

mbind :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mbind = (join .) . fmap . (fmap join .) . T.mapM


-- Equivalent to the Monad Writer instance.
instance Monoid o => Monad ((,) o) where
  return      = pure
  (o,a) >>= f = (o `mappend` o', a') where (o',a') = f a

deriving instance Foldable ((,) o)
deriving instance Traversable ((,) o)



