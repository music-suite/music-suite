                              
{-# LANGUAGE
    TypeFamilies,
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,     
    ScopedTypeVariables,
    FlexibleInstances,
    NoMonomorphismRestriction #-} 

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------

module Music.Score.Duration where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Data.Ratio
import Control.Applicative
import Control.Monad (ap, join, MonadPlus(..))
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Function (on)
import Data.Ord (comparing)

import Data.VectorSpace
import Data.AffineSpace
import Data.Basis

import Data.Functor.Identity
import Text.Parsec hiding ((<|>))
import Text.Parsec.Pos

import System.Posix -- debug
import System.IO
import System.IO.Unsafe --debug

import Music.Pitch.Literal
import Music.Dynamics.Literal

import Control.Reactive
import Control.Reactive.Midi

import qualified Codec.Midi as Midi
import qualified Music.MusicXml.Simple as Xml
import qualified Data.Map as Map
import qualified Data.List as List


-------------------------------------------------------------------------------------
-- Duration type
-------------------------------------------------------------------------------------

-- |
-- This type represents relative time in seconds.
--
newtype Duration = Duration { getDuration::Rational }                                  
    deriving (Eq, Ord, Num, Enum, Real, Fractional, RealFrac)
    -- Note: no Floating as we want to be able to switch to rational

instance Show Duration where 
    show = show . getDuration

instance AdditiveGroup Duration where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace Duration where
    type Scalar Duration = Duration
    (*^) = (*)

instance InnerSpace Duration where (<.>) = (*)


class HasDuration a where
    duration :: a -> Duration


-------------------------------------------------------------------------------------
-- Delayable class
-------------------------------------------------------------------------------------

-- |
-- Delayable values. This is really similar to 'AffineSpace', except that there
-- is no '.-.'.
-- 
class Delayable a where

    -- |
    -- Delay a score.
    -- > Duration -> Score a -> Score a
    -- 
    delay :: Duration -> a -> a
