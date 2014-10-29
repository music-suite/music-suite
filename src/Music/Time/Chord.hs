
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

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
-------------------------------------------------------------------------------------

module Music.Time.Chord (

      -- * Chord type
      Chord,

      -- * Construction
      chord,
      unchord,
      unsafeChord,

      -- invertChord,
      -- inversions,
      -- chordToScore,
      -- arpUp3,
      -- arpDown3,
      -- arpUpDown3,
      -- arpDownUp3,
      -- alberti3,
      -- triad,
      -- mtriad,
      -- sixthChord,
      -- sixthFourthChord,
      -- fromBass,     
  ) where


import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace
import           Data.String

import           Music.Time.Placed
import           Music.Time.Reverse
import           Music.Time.Split

import           Control.Applicative
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as Foldable
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable

import           Music.Dynamics.Literal
import           Music.Pitch.Literal


-- |
-- A 'Chord' is a parallel composition of values.
--
-- @
-- type Chord a = [Placed a]
-- @
--
newtype Chord a = Chord { getChord :: ChordList (ChordEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

-- Can use [] or Seq here
type ChordList = []

-- Can use any type as long as chordEv provides an Iso
type ChordEv a = Placed a

chordEv :: Iso (Placed a) (Placed b) (ChordEv a) (ChordEv b)
chordEv = id

instance Applicative Chord where
  pure  = return
  (<*>) = ap

instance Monad Chord where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance Wrapped (Chord a) where
  type Unwrapped (Chord a) = (ChordList (ChordEv a))
  _Wrapped' = iso getChord Chord

instance Rewrapped (Chord a) (Chord b)

-- instance Transformable (Chord a) where
--   transform s = over _Wrapped' (transform s)

-- instance HasDuration (Chord a) where
  -- TODO

-- instance Splittable a => Splittable (Chord a) where
--   -- TODO

-- instance Reversible a => Reversible (Chord a) where
--   rev = over _Wrapped' (fmap rev) -- TODO OK?

-- TODO
-- instance HasMeta (Chord a) where
  -- meta = error "Not implemented: meta"

chord :: Getter [Placed a] (Chord a)
chord = from unsafeChord

unchord :: Lens (Chord a) (Chord b) [Placed a] [Placed b]
unchord = _Wrapped

-- TODO names are not consistent with other types
unsafeChord :: Iso (Chord a) (Chord b) [Placed a] [Placed b]
unsafeChord = _Wrapped

instance IsString a => IsString (Chord a) where
  fromString = pure . fromString

deriving instance IsPitch a => IsPitch (Chord a)	 
deriving instance IsInterval a => IsInterval (Chord a)	 
deriving instance IsDynamics a => IsDynamics (Chord a)


{-
-- |
-- Invert a chord, i.e. transpose its lowest pitch up one octave.
--
-- To access higher-numbered inversions, iterate this function, i.e.
--
-- @
-- 'iterate' 'invertC' ('triad' c) !! 2
-- @
--
invertC :: Transposable a => Chord a -> Chord a
invertC = over chord (rotlAnd $ up _P8)

-- TODO include transp
inversions :: Transposable a => Chord a -> [Chord a]
inversions = iterate invertC

chordToScore :: Chord a -> Score a
chordToScore = pcat . map pure . toListOf traverse

-- TODO
unchord =  toListOf traverse


arpUp3 :: Chord a -> Score a
arpUp3 x = scat $ map ((^/16) . pure) [a,b,c]
  where
    [a,b,c] = unchord x

arpDown3 :: Chord a -> Score a
arpDown3 x = scat $ map ((^/16) . pure) [c,b,a]
  where
    [a,b,c] = unchord x

arpUpDown3 x = arpUp3 x |> arpDown3 x
arpDownUp3 x = arpDown3 x |> arpUp3 x

alberti3 :: Chord a -> Score a
alberti3 x = scat $ map ((^/16) . pure) [a,c,b,c]
  where
    [a,b,c] = unchord x



triad :: Transposable a => a -> Chord a
triad x = mconcat $ map pure [x, up _M3 x, up _P5 x]

mtriad :: Transposable a => a -> Chord a
mtriad x = mconcat $ map pure [x, up m3 x, up _P5 x]

sixthChord       = down m3 . invertC . mtriad
sixthFourthChord = down _P5 . invertC . invertC . triad


-- TODO better parsing
fromBass :: Transposable a => String -> a -> Chord a
fromBass "" x = triad x
-}
