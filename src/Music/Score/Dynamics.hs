

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

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
-- Provides functions for manipulating dynamics.
--
-------------------------------------------------------------------------------------


module Music.Score.Dynamics (
        -- ** Dynamic type functions
        Dynamic,
        SetDynamic,
        -- ** Accessing dynamics
        HasDynamics(..),
        HasDynamic(..),
        dynamic',
        dynamics',
        -- * Manipulating dynamics
        Level,
        Attenuable,
        louder,
        softer,
        level,
        compressor,
        fadeIn,
        fadeOut,

        -- -- * Dynamics representation
        -- HasDynamic(..),
        -- DynamicT(..),
        -- dynamics,
        -- dynamicVoice,
        -- dynamicSingle,
        -- 
        -- -- * Dynamic transformations
        -- -- ** Crescendo and diminuendo
        -- Level(..),
        -- cresc,
        -- dim,
        -- 
        -- -- ** Miscellaneous
        -- resetDynamics,  
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens            hiding (Level, transform)
import           Control.Monad
import           Data.AffineSpace
import           Data.Foldable
import qualified Data.List               as List
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace        hiding (Sum)

import           Music.Time
import Music.Time.Internal.Transform
import Music.Score.Part
import           Music.Dynamics.Literal


-- |
-- Dynamics type.
--
type family Dynamic (s :: *) :: *

-- |
-- Dynamic type.
--
type family SetDynamic (b :: *) (s :: *) :: *

-- |
-- Class of types that provide a single dynamic.
--
class (HasDynamics s t) => HasDynamic s t where

  -- |
  dynamic :: Lens s t (Dynamic s) (Dynamic t)

-- |
-- Class of types that provide a dynamic traversal.
--
class (Transformable (Dynamic s),
       Transformable (Dynamic t),
       SetDynamic (Dynamic t) s ~ t) => HasDynamics s t where

  -- | Dynamic type.
  dynamics :: Traversal s t (Dynamic s) (Dynamic t)

-- |
-- Dynamic type.
--
dynamic' :: (HasDynamic s t, s ~ t) => Lens' s (Dynamic s)
dynamic' = dynamic

-- |
-- Dynamic type.
--
dynamics' :: (HasDynamics s t, s ~ t) => Traversal' s (Dynamic s)
dynamics' = dynamics

#define PRIM_DYNAMIC_INSTANCE(TYPE)       \
                                          \
type instance Dynamic TYPE = TYPE;        \
type instance SetDynamic a TYPE = a;      \
                                          \
instance (Transformable a, a ~ Dynamic a) \
  => HasDynamic TYPE a where {            \
  dynamic = ($)              } ;          \
                                          \
instance (Transformable a, a ~ Dynamic a) \
  => HasDynamics TYPE a where {           \
  dynamics = ($)               } ;        \

PRIM_DYNAMIC_INSTANCE(())
PRIM_DYNAMIC_INSTANCE(Bool)
PRIM_DYNAMIC_INSTANCE(Ordering)
PRIM_DYNAMIC_INSTANCE(Char)
PRIM_DYNAMIC_INSTANCE(Int)
PRIM_DYNAMIC_INSTANCE(Integer)
PRIM_DYNAMIC_INSTANCE(Float)
PRIM_DYNAMIC_INSTANCE(Double)

type instance Dynamic (c,a) = Dynamic a
type instance SetDynamic b (c,a) = (c,SetDynamic b a)

instance HasDynamic a b => HasDynamic (c, a) (c, b) where
  dynamic = _2 . dynamic

instance HasDynamics a b => HasDynamics (c, a) (c, b) where
  dynamics = traverse . dynamics


type instance Dynamic [a] = Dynamic a
type instance SetDynamic b [a] = [SetDynamic b a]

instance HasDynamics a b => HasDynamics [a] [b] where
  dynamics = traverse . dynamics


type instance Dynamic (Note a) = Dynamic a
type instance SetDynamic g (Note a) = Note (SetDynamic g a)

type instance Dynamic (Note a) = Dynamic a

instance HasDynamic a b => HasDynamic (Note a) (Note b) where
  dynamic = _Wrapped . whilstL dynamic

instance HasDynamics a b => HasDynamics (Note a) (Note b) where
  dynamics = _Wrapped . whilstL dynamics


-- |
-- Associated interval type.
--
type Level a = Diff (Dynamic a)

-- |
-- Class of types that can be transposed.
--
type Attenuable a 
  = (HasDynamics a a,
     VectorSpace (Level a), AffineSpace (Dynamic a),
     {-IsLevel (Level a), -} IsDynamics (Dynamic a))

-- |
-- Transpose up.
--
louder :: Attenuable a => Level a -> a -> a
louder a = dynamics %~ (.+^ a)

-- |
-- Transpose down.
--
softer :: Attenuable a => Level a -> a -> a
softer a = dynamics %~ (.-^ a)

-- |
-- Transpose down.
--
volume :: (Num (Dynamic t), HasDynamics s t, Dynamic s ~ Dynamic t) => Dynamic t -> s -> t
volume a = dynamics *~ a

-- |
-- Transpose down.
--
level :: Attenuable a => Dynamic a -> a -> a
level a = dynamics .~ a

compressor :: Attenuable a => 
  Dynamic a           -- ^ Threshold
  -> Scalar (Level a) -- ^ Ratio
  -> a 
  -> a
compressor = error "Not implemented: compressor"

--
-- TODO non-linear fades etc
--

-- |
-- Fade in.
--
fadeIn :: (HasPosition a, HasDynamics a a, Dynamic a ~ Behavior c, Fractional c) => Duration -> a -> a
fadeIn d x = x & dynamics *~ (_onset x >-> d `transform` unit)

-- |
-- Fade in.
--
fadeOut :: (HasPosition a, HasDynamics a a, Dynamic a ~ Behavior c, Fractional c) => Duration -> a -> a
fadeOut d x = x & dynamics *~ (d <-< _offset x `transform` rev unit)



{-
class HasDynamic a where
    setBeginCresc   :: Bool -> a -> a
    setEndCresc     :: Bool -> a -> a
    setBeginDim     :: Bool -> a -> a
    setEndDim       :: Bool -> a -> a
    setLevel        :: Double -> a -> a

-- end cresc/dim, level, begin cresc/dim
newtype DynamicT a = DynamicT { getDynamicT :: (((Any, Any), Option (First Double), (Any, Any)), a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)

instance HasDynamic (DynamicT a) where
    setBeginCresc (Any -> bc) (DynamicT (((ec,ed),l,(_ ,bd)),a))   = DynamicT (((ec,ed),l,(bc,bd)),a)
    setEndCresc   (Any -> ec) (DynamicT (((_ ,ed),l,(bc,bd)),a))   = DynamicT (((ec,ed),l,(bc,bd)),a)
    setBeginDim   (Any -> bd) (DynamicT (((ec,ed),l,(bc,_ )),a))   = DynamicT (((ec,ed),l,(bc,bd)),a)
    setEndDim     (Any -> ed) (DynamicT (((ec,_ ),l,(bc,bd)),a))   = DynamicT (((ec,ed),l,(bc,bd)),a)
    setLevel      ((Option . Just . First) -> l ) (DynamicT (((ec,ed),_,(bc,bd)),a))   = DynamicT (((ec,ed),l,(bc,bd)),a)

instance HasDynamic b => HasDynamic (a, b) where
    setBeginCresc n = fmap (setBeginCresc n)
    setEndCresc   n = fmap (setEndCresc n)
    setBeginDim   n = fmap (setBeginDim n)
    setEndDim     n = fmap (setEndDim n)
    setLevel      n = fmap (setLevel n)



--------------------------------------------------------------------------------
-- Dynamics
--------------------------------------------------------------------------------

-- |
-- Represents dynamics over a duration.
--
data Level a
    = Level  a
    | Change a a
    deriving (Eq, Show)

instance Fractional a => IsDynamics (Level a) where
    fromDynamics (DynamicsL (Just a, Nothing)) = Level (realToFrac a)
    fromDynamics (DynamicsL (Just a, Just b)) = Change (realToFrac a) (realToFrac b)
    fromDynamics x = error $ "fromDynamics: Invalid dynamics literal " ++ show x


-- |
-- Apply a dynamic level over the score.
--
dynamics :: (HasDynamic a, HasPart' a) => Score (Level Double) -> Score a -> Score a
dynamics d a = (duration a `stretchTo` d) `dynamics'` a

dynamicSingle :: HasDynamic a => Score (Level Double) -> Score a -> Score a
dynamicSingle d a  = (duration a `stretchTo` d) `dynamicsSingle'` a

-- |
-- Apply a dynamic level over a voice.
--
dynamicVoice :: HasDynamic a => Score (Level Double) -> Voice (Maybe a) -> Voice (Maybe a)
dynamicVoice d = scoreToVoice . dynamicSingle d . removeRests . voiceToScore


dynamics' :: (HasDynamic a, HasPart' a) => Score (Level Double) -> Score a -> Score a
dynamics' ds = mapAllParts (fmap $ dynamicsSingle' ds)

dynamicsSingle' :: HasDynamic a => Score (Level Double) -> Score a -> Score a
dynamicsSingle' ds = applyDynSingle (fmap fromJust $ scoreToVoice ds)


applyDynSingle :: HasDynamic a => Voice (Level Double) -> Score a -> Score a
applyDynSingle ds = applySingle ds3
    where
        -- ds2 :: Voice (Dyn2 Double)
        ds2 = mapValuesVoice dyn2 ds
        -- ds3 :: Voice (Score a -> Score a)
        ds3 = fmap g ds2

        g (ec,ed,l,bc,bd) = id
                . (if ec then mapFirstSingle (setEndCresc     True) else id)
                . (if ed then mapFirstSingle (setEndDim       True) else id)
                . (if bc then mapFirstSingle (setBeginCresc   True) else id)
                . (if bd then mapFirstSingle (setBeginDim     True) else id)
                . maybe id (mapFirstSingle . setLevel) l
        mapFirstSingle f = mapPhraseSingle f id id

-- end cresc, end dim, level, begin cresc, begin dim
type LevelDiff a = (Bool, Bool, Maybe a, Bool, Bool)

dyn2 :: Ord a => [Level a] -> [LevelDiff a]
dyn2 = snd . List.mapAccumL g (Nothing, False, False) -- level, cresc, dim
    where
        g (Nothing, False, False) (Level b)     = ((Just b,  False, False), (False, False, Just b,  False, False))
        g (Nothing, False, False) (Change b c)  = ((Just b,  b < c, b > c), (False, False, Just b,  b < c, b > c))

        g (Just a , cr, dm) (Level b)
            | a == b                            = ((Just b,  False, False), (cr,    dm,    Nothing, False, False))
            | a /= b                            = ((Just b,  False, False), (cr,    dm,    Just b,  False, False))
        g (Just a , cr, dm) (Change b c)
            | a == b                            = ((Just b,  b < c, b > c), (cr,    dm,    Nothing, b < c, b > c))
            | a /= b                            = ((Just b,  b < c, b > c), (cr,    dm,    Just b,  b < c, b > c))



mapValuesVoice :: ([a] -> [b]) -> Voice a -> Voice b
mapValuesVoice f = (^. voice) . uncurry zip . second f . unzip . (^. from voice)




cresc :: IsDynamics a => Double -> Double -> a
cresc a b = fromDynamics $ DynamicsL (Just a, Just b)

dim :: IsDynamics a => Double -> Double -> a
dim a b = fromDynamics $ DynamicsL (Just a, Just b)


resetDynamics :: HasDynamic c => c -> c
resetDynamics = setBeginCresc False . setEndCresc False . setBeginDim False . setEndDim False

-}
