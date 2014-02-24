
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    TypeOperators,
    TypeFamilies,
    ViewPatterns,
    GeneralizedNewtypeDeriving #-}

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
        -- * Dynamics representation
        HasDynamic(..),
        DynamicT(..),
        dynamics,
        dynamicVoice,
        dynamicSingle,

        -- * Dynamic transformations
        -- ** Crescendo and diminuendo
        Level(..),
        cresc,
        dim,

        -- ** Miscellaneous
        resetDynamics,
  ) where

import Control.Lens hiding (Level)
import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Semigroup
import Data.Ratio
import Data.Maybe
import Data.Foldable
import Data.Typeable
import Data.VectorSpace hiding (Sum)
import Data.AffineSpace
import qualified Data.List as List

import Music.Score.Voice
import Music.Score.Score
import Music.Time
import Music.Score.Part
import Music.Score.Combinators
import Music.Score.Convert

import Music.Dynamics.Literal

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
            | a == b                            = ((Just b,  False, False), (cr,    dm,    Nothing, False, False))
            | a /= b                            = ((Just b,  False, False), (cr,    dm,    Just b,  False, False))
        g (Just a , cr, dm) (Change b c)
            | a == b                            = ((Just b,  b < c, b > c), (cr,    dm,    Nothing, b < c, b > c))
            | a /= b                            = ((Just b,  b < c, b > c), (cr,    dm,    Just b,  b < c, b > c))



mapValuesVoice :: ([a] -> [b]) -> Voice a -> Voice b
mapValuesVoice f = (^. voice) . uncurry zip . second f . unzip . (^. from voice)




cresc :: IsDynamics a => Double -> Double -> a
cresc a b = fromDynamics $ DynamicsL (Just a, Just b)

dim :: IsDynamics a => Double -> Double -> a
dim a b = fromDynamics $ DynamicsL (Just a, Just b)


resetDynamics :: HasDynamic c => c -> c
resetDynamics = setBeginCresc False . setEndCresc False . setBeginDim False . setEndDim False

