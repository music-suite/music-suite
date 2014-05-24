
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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
-- Provides a way of adding text to notes.
--
-------------------------------------------------------------------------------------


module Music.Score.Text (
        -- * Text
        HasText(..),
        TextT(..),
        text,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens            hiding (transform)
import           Data.Foldable
import           Data.Foldable
import           Data.Functor.Couple
import           Data.Ratio
import           Data.Semigroup
import           Data.Typeable
import           Data.Word

-- import           Music.Score.Combinators
import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Literal
import           Music.Score.Part
import           Music.Score.Phrases
import           Music.Time



class HasText a where
    addText :: String -> a -> a

newtype TextT a = TextT { getTextT :: Couple [String] a }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable, Applicative, Monad, Comonad)

instance HasText a => HasText (b, a) where
    addText       s                                 = fmap (addText s)

instance HasText a => HasText (Couple b a) where
    addText       s                                 = fmap (addText s)

instance HasText a => HasText [a] where
    addText       s                                 = fmap (addText s)

instance HasText a => HasText (Score a) where
    addText       s                                 = fmap (addText s)


-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (TextT a) where
  type Unwrapped (TextT a) = Couple [String] a
  _Wrapped' = iso getTextT TextT

instance Rewrapped (TextT a) (TextT b)

instance HasText (TextT a) where
    addText      s (TextT (Couple (t,x)))                    = TextT (Couple (t ++ [s],x))

-- Lifted instances
deriving instance Num a => Num (TextT a)
deriving instance Fractional a => Fractional (TextT a)
deriving instance Floating a => Floating (TextT a)
deriving instance Enum a => Enum (TextT a)
deriving instance Bounded a => Bounded (TextT a)
deriving instance (Num a, Ord a, Real a) => Real (TextT a)
deriving instance (Real a, Enum a, Integral a) => Integral (TextT a)

-- |
-- Attach the given text to the first note in the score.
--
text :: (HasPhrases' s a, HasText a) => String -> s -> s
text s = over (phrases'.headV) (addText s)


headV :: Traversal' (Voice a) a
headV = (eventsV._head._2)

middleV :: Traversal' (Voice a) a
middleV = (eventsV._middle.traverse._2)

lastV :: Traversal' (Voice a) a
lastV = (eventsV._last._2)

-- Traverse writing to all elements *except* first and last
_middle :: (Snoc s s a a, Cons s s b b) => Traversal' s s
_middle = _tail._init
