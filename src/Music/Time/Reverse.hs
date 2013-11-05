
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleContexts,
    ConstraintKinds,
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
-- Provides reversible values.
--
-------------------------------------------------------------------------------------

module Music.Time.Reverse (
        -- * Reversible class
        Reversible(..),
        -- ** Utility
        -- NoRev(..),
        -- WithRev(..),
        -- withRev,
        -- fromWithRev,
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

import Music.Time.Relative
import Music.Time.Time
import Music.Time.Span
import Music.Time.Onset

-- |
-- Reversible values.
--
-- For instances of 'Reversible' and 'HasOnset', the following laws should hold:
--
-- > onset a    = onset (rev a)
-- > duration a = duration (rev a)
--
class Reversible a where

    -- |
    -- Reverse a value.
    -- 
    -- Reverse is an involution, meaning that:
    --
    -- > rev (rev a) = a
    -- 
    rev :: a -> a

-- instance Reversible Time where
--     rev t = mirror t

instance Reversible Span where
    -- rev = inSpan g where g (t, d) = (mirror (t .+^ d), d)
    rev (getSpanAbs -> (x, y)) = mirror y `between` mirror x

instance Reversible a => Reversible [a] where
    rev = fmap rev






{-
newtype NoRev a = NoRev { getNoRev :: a }
    deriving (Eq, Ord, Enum, Show, Semigroup, Monoid)

instance Reversible (NoRev a) where
    rev = id


newtype WithRev a = WithRev (a,a)
    deriving (Eq, Ord, Semigroup, Monoid)

withRev :: Reversible a => a -> WithRev a
withRev x = WithRev (rev x, x)

fromWithRev :: Reversible a => WithRev a -> a
fromWithRev (WithRev (_,x)) = x

instance Reversible a => Reversible (WithRev a) where
    rev (WithRev (r,x)) = WithRev (x,r)
-}



