
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Provides functions for manipulating parts.
module Music.Score.Part (
        -- ** Articulation type functions
        Part,
        SetPart,
        -- ** Accessing parts
        HasParts(..),
        HasPart(..),
        HasPart',
        HasParts',
        part',
        parts',

        -- * Listing parts
        allParts,

        -- * Extracting parts
        extracted,
        extractedWithInfo,
        extractPart,
        extractParts,
        extractPartsWithInfo,

        -- * Manipulating parts

        -- * Part representation
        PartT(..),

        -- Part,
        -- HasPart(..),
        -- HasPart',
        -- PartT(..),
        -- getParts,
        
        (</>),
        rcat,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens                  hiding (parts, transform)
import           Control.Monad.Plus
-- import           Data.Default
import           Data.Foldable
import           Data.Functor.Couple
import qualified Data.List                     as List
import qualified Data.List                     as List
import           Data.Ord                      (comparing)
import           Data.PairMonad
import           Data.Ratio
import           Data.Semigroup
import           Data.Traversable
import           Data.Typeable

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Ties
import           Music.Score.Internal.Util     (through)
import           Music.Time
import           Music.Time.Internal.Transform


-- |
-- Parts type.
--
type family Part (s :: *) :: * -- Part s   = a

-- |
-- Part type.
--
type family SetPart (b :: *) (s :: *) :: * -- Part b s = t

-- |
-- Class of types that provide a single part.
--
class (HasParts s t) => HasPart s t where

  -- | Part type.
  part :: Lens s t (Part s) (Part t)

-- |
-- Class of types that provide a part traversal.
--
class (Transformable (Part s),
       Transformable (Part t)
       -- , SetPart (Part t) s ~ t
       ) => HasParts s t where

  -- | Part type.
  parts :: Traversal s t (Part s) (Part t)

type HasPart' a  = HasPart a a
type HasParts' a = HasParts a a

-- |
-- Part type.
--
part' :: (HasPart s t, s ~ t) => Lens' s (Part s)
part' = part

-- |
-- Part type.
--
parts' :: (HasParts s t, s ~ t) => Traversal' s (Part s)
parts' = parts

type instance Part Bool = Bool
type instance SetPart a Bool = a
instance (b ~ Part b, Transformable b) => HasPart Bool b where
  part = ($)
instance (b ~ Part b, Transformable b) => HasParts Bool b where
  parts = ($)

type instance Part Ordering = Ordering
type instance SetPart a Ordering = a
instance (b ~ Part b, Transformable b) => HasPart Ordering b where
  part = ($)
instance (b ~ Part b, Transformable b) => HasParts Ordering b where
  parts = ($)

type instance Part () = ()
type instance SetPart a () = a
instance (b ~ Part b, Transformable b) => HasPart () b where
  part = ($)
instance (b ~ Part b, Transformable b) => HasParts () b where
  parts = ($)

type instance Part Int = Int
type instance SetPart a Int = a
instance HasPart Int Int where
  part = ($)
instance HasParts Int Int where
  parts = ($)

type instance Part Integer = Integer
type instance SetPart a Integer = a
instance HasPart Integer Integer where
  part = ($)
instance HasParts Integer Integer where
  parts = ($)

type instance Part Float = Float
type instance SetPart a Float = a
instance HasPart Float Float where
  part = ($)
instance HasParts Float Float where
  parts = ($)

type instance Part (c,a) = Part a
type instance SetPart b (c,a) = (c,SetPart b a)
type instance Part [a] = Part a
type instance SetPart b [a] = [SetPart b a]
type instance Part (Maybe a) = Part a
type instance SetPart b (Maybe a) = Maybe (SetPart b a)
type instance Part (Either c a) = Part a
type instance SetPart b (Either c a) = Either c (SetPart b a)

type instance Part (Aligned a) = Part a
type instance SetPart b (Aligned a) = Aligned (SetPart b a)

instance HasParts a b => HasParts (Aligned a) (Aligned b) where
  parts = _Wrapped . parts

instance HasPart a b => HasPart (c, a) (c, b) where
  part = _2 . part

instance HasParts a b => HasParts (c, a) (c, b) where
  parts = traverse . parts

instance HasParts a b => HasParts [a] [b] where
  parts = traverse . parts

instance HasParts a b => HasParts (Maybe a) (Maybe b) where
  parts = traverse . parts

instance HasParts a b => HasParts (Either c a) (Either c b) where
  parts = traverse . parts


type instance Part (Event a) = Part a
type instance SetPart g (Event a) = Event (SetPart g a)

instance (HasPart a b) => HasPart (Event a) (Event b) where
  part = from event . whilstL part

instance (HasParts a b) => HasParts (Event a) (Event b) where
  parts = from event . whilstL parts

type instance Part (Note a) = Part a
type instance SetPart g (Note a) = Note (SetPart g a)

instance (HasPart a b) => HasPart (Note a) (Note b) where
  part = from note . whilstLD part

instance (HasParts a b) => HasParts (Note a) (Note b) where
  parts = from note . whilstLD parts

-- |
-- List all the parts
--
allParts :: (Ord (Part a), HasParts' a) => a -> [Part a]
allParts = List.nub . List.sort . toListOf parts

-- |
-- List all the parts
--
extractPart :: (Eq (Part a), HasPart' a) => Part a -> Score a -> Score a
extractPart = extractPartG

extractPartG :: (Eq (Part a), MonadPlus f, HasPart' a) => Part a -> f a -> f a
extractPartG p x = head $ (\p s -> filterPart (== p) s) <$> [p] <*> return x

-- |
-- List all the parts
--
extractParts :: (Ord (Part a), HasPart' a) => Score a -> [Score a]
extractParts = extractPartsG

extractPartsG :: (
  MonadPlus f, HasParts' (f a), HasPart' a, 
  Part (f a) ~ Part a, Ord (Part a)
  ) => f a -> [f a]
extractPartsG x = (\p s -> filterPart (== p) s) <$> allParts x <*> return x

filterPart :: (MonadPlus f, HasPart a a) => (Part a -> Bool) -> f a -> f a
filterPart p = mfilter (\x -> p (x ^. part))

extractPartsWithInfo :: (Ord (Part a), HasPart' a) => Score a -> [(Part a, Score a)]
extractPartsWithInfo x = zip (allParts x) (extractParts x)

extracted :: (Ord (Part a), HasPart' a{-, HasPart a b-}) => Iso (Score a) (Score b) [Score a] [Score b]
extracted = iso extractParts mconcat

extractedWithInfo :: (Ord (Part a), Ord (Part b), HasPart' a, HasPart' b) => Iso (Score a) (Score b) [(Part a, Score a)] [(Part b, Score b)]
extractedWithInfo = iso extractPartsWithInfo $ mconcat . fmap (uncurry $ set parts')



newtype PartT n a = PartT { getPartT :: (n, a) }
  deriving (Eq, Ord, Show, Typeable, Functor,
    Applicative, Comonad, Monad, Transformable)

instance Wrapped (PartT p a) where
  type Unwrapped (PartT p a) = (p, a)
  _Wrapped' = iso getPartT PartT

instance Rewrapped (PartT p a) (PartT p' b)


type instance Part (PartT p a) = p
type instance SetPart p' (PartT p a) = PartT p' a

instance (Transformable p, Transformable p') => HasPart (PartT p a) (PartT p' a) where
  part = _Wrapped . _1

instance (Transformable p, Transformable p') => HasParts (PartT p a) (PartT p' a) where
  parts = _Wrapped . _1


instance (IsPitch a, Enum n) => IsPitch (PartT n a) where
  fromPitch l = PartT (toEnum 0, fromPitch l)

instance (IsDynamics a, Enum n) => IsDynamics (PartT n a) where
  fromDynamics l = PartT (toEnum 0, fromDynamics l)

instance Reversible a => Reversible (PartT p a) where
  rev = fmap rev

instance Tiable a => Tiable (PartT n a) where
  isTieEndBeginning (PartT (_,a)) = isTieEndBeginning a
  toTied (PartT (v,a)) = (PartT (v,b), PartT (v,c)) where (b,c) = toTied a

type instance Part                 (Behavior a) = Behavior (Part a)
type instance SetPart (Behavior g) (Behavior a) = Behavior (SetPart g a)

instance (HasPart a a, HasPart a b) => HasParts (Behavior a) (Behavior b) where
  parts = through part part
instance (HasPart a a, HasPart a b) => HasPart (Behavior a) (Behavior b) where
  part = through part part

type instance Part (Score a) = Part a
type instance SetPart g (Score a) = Score (SetPart g a)

instance (HasParts a b) => HasParts (Score a) (Score b) where
  parts =
    _Wrapped . _2   -- into NScore
    . _Wrapped
    . traverse
    . from event    -- this needed?
    . whilstL parts

type instance Part (Voice a) = Part a
type instance SetPart g (Voice a) = Voice (SetPart g a)

instance (HasParts a b) => HasParts (Voice a) (Voice b) where
  parts =
    _Wrapped
    . traverse
    . _Wrapped      -- this needed?
    . whilstLD parts


infixr 6 </>


-- |
-- Concatenate parts.
--
rcat :: (HasParts' a, Enum (Part a)) => [Score a] -> Score a
rcat = List.foldr (</>) mempty

-- |
-- Similar to '<>', but increases parts in the second part to prevent collision.
--
(</>) :: (HasParts' a, Enum (Part a)) => Score a -> Score a -> Score a
a </> b = a <> moveParts offset b
    where
        -- max voice in a + 1
        offset = succ $ maximum' 0 $ fmap fromEnum $ toListOf parts a

        -- |
        -- Move down one voice (all parts).
        --
        moveParts :: (Integral b, HasParts' a, Enum (Part a)) => b -> Score a -> Score a
        moveParts x = parts %~ (successor x)

        -- |
        -- Move top-part to the specific voice (other parts follow).
        --
        moveToPart :: (Enum b, HasParts' a, Enum (Part a)) => b -> Score a -> Score a
        moveToPart v = moveParts (fromEnum v)


        iterating :: (a -> a) -> (a -> a) -> Int -> a -> a
        iterating f g n
            | n <  0 = f . iterating f g (n + 1)
            | n == 0 = id
            | n >  0 = g . iterating f g (n - 1)

        successor :: (Integral b, Enum a) => b -> a -> a
        successor n = iterating pred succ (fromIntegral n)

        maximum' :: (Ord a, Foldable t) => a -> t a -> a
        maximum' z = option z getMax . foldMap (Option . Just . Max)

