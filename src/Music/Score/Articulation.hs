
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
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
-- Provides functions for manipulating articulation.
--
-------------------------------------------------------------------------------------


module Music.Score.Articulation (
        -- * Representation
        HasArticulation(..),
        ArticulationT(..),

        -- * Transformations
        -- ** Accents
        accent,
        marcato,
        accentLast,
        marcatoLast,
        accentAll,
        marcatoAll,

        -- ** Phrasing
        tenuto,
        separated,
        staccato,
        portato,
        legato,
        spiccato,

{-
        -- ** Miscellaneous
        resetArticulation,
-}

  ) where

import           Control.Applicative
import           Data.Foldable
import           Data.Semigroup
import           Data.Typeable

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Combinators
import           Music.Score.Part
import           Music.Score.Score

class HasArticulation a where
    setBeginSlur :: Bool -> a -> a
    setContSlur :: Bool -> a -> a
    setEndSlur :: Bool -> a -> a
    setAccLevel :: Int -> a -> a
    setStaccLevel :: Int -> a -> a

newtype ArticulationT a = ArticulationT { getArticulationT :: (((Any, Any, Any), (Sum Int, Sum Int)), a) }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad)

-- instance Monad ArticulationT where
    -- return = undefined
    -- return x = ArticulationT (Any False,Any False,0,0,x,False)
    -- (>>=) = error "No ArticulationT.(>>=)"

instance Semigroup a => Semigroup (ArticulationT a) where
    ArticulationT (((es,us,bs),(al,sl)),a) <> ArticulationT (_,b) = ArticulationT (((es,us,bs),(al,sl)), a <> b)

instance (Semigroup a, Monoid a) => Monoid (ArticulationT a) where
    mempty = return mempty
    mappend = (<>)

instance IsPitch a => IsPitch (ArticulationT a) where
    fromPitch l = return (fromPitch l)

instance IsDynamics a => IsDynamics (ArticulationT a) where
    fromDynamics l = return (fromDynamics l)

instance HasArticulation (ArticulationT a) where
    setEndSlur    (Any -> es) (ArticulationT (((_ ,us,bs),(al,sl)),a)) = ArticulationT (((es,us,bs),(al,sl)),a)
    setContSlur   (Any -> us) (ArticulationT (((es,_ ,bs),(al,sl)),a)) = ArticulationT (((es,us,bs),(al,sl)),a)
    setBeginSlur  (Any -> bs) (ArticulationT (((es,us,_ ),(al,sl)),a)) = ArticulationT (((es,us,bs),(al,sl)),a)
    setAccLevel   (Sum -> al) (ArticulationT (((es,us,bs),(_ ,sl)),a)) = ArticulationT (((es,us,bs),(al,sl)),a)
    setStaccLevel (Sum -> sl) (ArticulationT (((es,us,bs),(al,_ )),a)) = ArticulationT (((es,us,bs),(al,sl)),a)

instance HasArticulation b => HasArticulation (a,b) where
    setEndSlur    n = fmap (setEndSlur n)
    setContSlur   n = fmap (setContSlur n)
    setBeginSlur  n = fmap (setBeginSlur n)
    setAccLevel   n = fmap (setAccLevel n)
    setStaccLevel n = fmap (setStaccLevel n)

--------------------------------------------------------------------------------
-- Articulation
--------------------------------------------------------------------------------

-- Accents

-- | Add a normal accent at the beginning of each phrase in each part in the given score.
accent      :: (HasPart' a, HasArticulation a) => Score a -> Score a
accent      = mapPhrase (setAccLevel 1) id id

-- | Add a marcato accent at the beginning of each phrase in each part in the given score.
marcato     :: (HasPart' a, HasArticulation a) => Score a -> Score a
marcato     = mapPhrase (setAccLevel 2) id id

-- | Add a normal accent to all notes in the given score.
accentAll   :: (HasPart' a, HasArticulation a) => Score a -> Score a
accentAll   = mapPhrase (setAccLevel 1) (setAccLevel 1) (setAccLevel 1)

-- | Add a marcato accent to all notes in the given score.
marcatoAll  :: (HasPart' a, HasArticulation a) => Score a -> Score a
marcatoAll  = mapPhrase (setAccLevel 2) (setAccLevel 2) (setAccLevel 2)

-- | Add a normal accent at the end of each phrase in each part in the given score.
accentLast  :: (HasPart' a, HasArticulation a) => Score a -> Score a
accentLast  = mapPhrase id id (setAccLevel 1)

-- | Add a marcato accent at the end of each phrase in each part in the given score.
marcatoLast :: (HasPart' a, HasArticulation a) => Score a -> Score a
marcatoLast = mapPhrase id id (setAccLevel 2)


-- Phrasing

-- | Add tenuto marks to each phrase in each part in the given score.
tenuto      :: (HasPart' a, HasArticulation a) => Score a -> Score a
tenuto      = mapPhrase (setStaccLevel (-2)) (setStaccLevel (-2)) (setStaccLevel (-2))

-- | Add combined staccato and tenuto marks to each phrase in each part in the given score.
separated   :: (HasPart' a, HasArticulation a) => Score a -> Score a
separated   = mapPhrase (setStaccLevel (-1)) (setStaccLevel (-1)) (setStaccLevel (-1))

-- | Add staccato marks to each phrase in each part in the given score.
staccato    :: (HasPart' a, HasArticulation a) => Score a -> Score a
staccato    = mapPhrase (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1)

-- | Add portato marks to each phrase in each part in the given score.
portato     :: (HasPart' a, HasArticulation a) => Score a -> Score a
portato     = staccato . legato

-- | Add legato marks to each phrase in each part in the given score.
legato      :: (HasPart' a, HasArticulation a) => Score a -> Score a
legato      = mapPhrase (setBeginSlur True) id (setEndSlur True)

-- | Add spiccatto marks to the given score.
spiccato    :: (HasPart' a, HasArticulation a) => Score a -> Score a
spiccato    = mapPhrase (setStaccLevel 2) (setStaccLevel 2) (setStaccLevel 2)

-- | Remove all articulation from the given note or notes.
resetArticulation :: HasArticulation c => c -> c
resetArticulation = setBeginSlur False . setContSlur False . setEndSlur False . setAccLevel 0 . setStaccLevel 0

-- Safe for tuple-like types
get1 = head . toList

