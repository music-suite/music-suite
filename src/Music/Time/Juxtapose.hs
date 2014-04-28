
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}


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
-------------------------------------------------------------------------------------

module Music.Time.Juxtapose (
       -- * Align without composition
        lead,
        follow,
        -- * Align and compose
        after,
        before,
        during,
        sustain,
        palindrome,
        
        -- ** Composition operators
        (|>),
        (<|),

        -- ** Catenation
        scat,
        pcat,

        -- ** Repetition
        times,
  ) where


import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Monoid.WithSemigroup
import           Data.Semigroup
import           Data.VectorSpace

import           Music.Time.Split
import           Music.Time.Reverse

-----
-- import Data.Fixed
-- import           Data.Default
-- import           Data.Ratio
-- 
-- import           Control.Applicative
-- import           Control.Arrow                (first, second, (***), (&&&))
-- import qualified Control.Category
-- import           Control.Comonad
-- import           Control.Comonad.Env
-- import           Control.Lens                 hiding (Indexable, Level, above,
--                                                below, index, inside, parts,
--                                                reversed, transform, (|>), (<|))
-- import           Control.Monad
-- import           Control.Monad.Plus
-- import           Data.AffineSpace
-- import           Data.AffineSpace.Point
-- import           Data.Distributive
-- import           Data.Foldable                (Foldable)
-- import qualified Data.Foldable                as Foldable
-- import           Data.Functor.Rep
-- import qualified Data.List
-- import           Data.List.NonEmpty           (NonEmpty)
-- import           Data.Maybe
-- import           Data.NumInstances
-- import           Data.Semigroup               hiding ()
-- import           Data.Sequence                (Seq)
-- import qualified Data.Sequence                as Seq
-- import           Data.Traversable             (Traversable)
-- import qualified Data.Traversable             as T
-- import           Data.Typeable
-- import           Data.VectorSpace hiding (Sum(..))
-- import           Music.Dynamics.Literal
-- import           Music.Pitch.Literal
-- 
-- import qualified Data.Ratio                   as Util_Ratio
-- import qualified Data.List as List
-- import qualified Data.Foldable as Foldable
-- import qualified Data.Ord as Ord
-----


--
-- TODO names
-- Especially 'after' is counter-intuitive
--

-- |
-- Move a value so that
--
-- @
-- '_offset' (a ``lead`` b) = '_onset' b
-- @
--
--
lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
a `lead` b   = placeAt 1 (b `_position` 0) a

-- |
-- Move a value so that
--
-- @
-- '_offset' a = '_onset' (a ``follow`` b)
-- @
--
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `follow` b = placeAt 0 (a `_position` 1) b

-- |
-- Move a value so that
--
after :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `after` b =  a <> (a `follow` b)

-- |
-- Move a value so that
--
before :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `before` b =  (a `lead` b) <> b

-- |
-- A value followed by its reverse (retrograde).
--
palindrome :: (Semigroup a, Reversible a, HasPosition a) => a -> a
palindrome a = a `after` rev a

infixr 6 |>
infixr 6 <|

(|>) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(|>) = after

(<|) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(<|) = before

-- infixr 6 >|
-- infixr 6 |<

-- |
-- Compose a list of sequential objects, with onset and offset tangent to one another.
--
-- For non-positioned types, this is the often same as 'mconcat'
-- For positioned types, this is the same as 'afterAnother'
--
scat :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => [a] -> a
scat = Prelude.foldr (|>) mempty

-- |
-- Compose a list of parallel objects, so that their local origins align.
--
-- This not possible for non-positioned types, as they have no notion of an origin.
-- For positioned types this is the same as 'mconcat'.
--
pcat :: (Semigroup a, Monoid a) => [a] -> a
pcat = Prelude.foldr (<>) mempty

-- |
-- Move a value so that
--
during :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
y `during`  x = _placeAt (_era x) y

-- |
-- Move a value so that
--
sustain :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
x `sustain` y   = x <> y `during` x

-- |
-- Move a value so that
--
times :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => Int -> a -> a
times n   = scat . replicate n

{-
-- |
-- This pseudo-class gathers the restrictions needed to implement position a value at
-- any point and duration in time.
--
type Transformable a   =  (Stretchable a, Delayable a)


-------------------------------------------------------------------------------------
-- Juxtaposition
-------------------------------------------------------------------------------------

-- |
-- @a \`following\` b@ moves score /b/ so that its onset is at the offset of score
-- /a/ and returns the moved score.
--
following :: (HasOffset a, Delayable b, HasOnset b) => a -> b -> b
a `following` b =  startAt (offset a) b

-- |
-- @a \`preceding\` b@ moves score /a/ so that its offset is at the onset of score
-- /b/ and returns the moved score.
--
preceding :: (Delayable a, HasOffset a, HasOnset b) => a -> b -> a
a `preceding` b =  stopAt (onset b) a

-- | @a \`during\` b@ places /a/ at the same era as /b/ and returns the moved score.
during :: (Delayable a, Stretchable a, HasOnset a, HasDuration a, HasOnset b, HasDuration b) => a -> b -> a
a `during` b = startAt (onset b) $ stretchTo (duration b) a

-------------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------------

infixr 6 |>
infixr 6 >|
infixr 6 <|

-- |
-- Compose in sequence.
--
-- @a |> b@ moves score /b/ as per 'following' and then composes the resulting scores with '<>'.
--
-- > Score a -> Score a -> Score a
--
(|>)            :: (Semigroup a, HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a
-- |
-- Compose in sequence.
--
-- @a >| b@ moves score /a/ as per 'preceding' and then composes the resulting scores with '<>'.
--
-- > Score a -> Score a -> Score a
--
(>|)            :: (Semigroup a, HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a

-- |
-- Compose in reverse sequence.
--
-- To compose in parallel, use '<>'.
--
-- > Score a -> Score a -> Score a
--
(<|)            :: (Semigroup a, HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a

a |> b =  a <> (a `following` b)
a >| b =  (a `preceding` b) <> b
a <| b =  b |> a

-- |
-- Sequential catenation.
--
-- > [Score a] -> Score a
--
scat            :: (Monoid' a, HasOnset a, HasOffset a, Delayable a) =>
                [a] -> a
-- |
-- Parallel catenation.
--
-- > [Score a] -> Score a
--
pcat            :: Monoid' a =>
                [a] -> a

scat = Prelude.foldr (|>) mempty
pcat = Prelude.foldr (<>) mempty


-- |
-- Like '<>', but scaling the second agument to the duration of the first.
--
-- > Score a -> Score a -> Score a
--
sustain         :: (Semigroup a, Stretchable a, HasDuration a, Fractional d, d ~ Duration) =>
                a -> a -> a

-- |
-- Like '|>' but with a negative delay on the second element.
--
-- > Duration -> Score a -> Score a -> Score a
--
anticipate      :: (Semigroup a, Transformable a, HasOnset a, HasOffset a, Ord d, d ~ Duration) =>
                d -> a -> a -> a

x `sustain` y     = x <> duration x `stretchTo` y
anticipate t a b  =  a <> startAt (offset a .-^ t) b

-- Like '<>', but truncating the second agument to the duration of the first.
-- prolong x y = x <> before (duration x) y


-- --------------------------------------------------------------------------------
-- -- Structure
-- --------------------------------------------------------------------------------

-- |
-- Repeat exact amount of times.
--
-- > Duration -> Score Note -> Score Note
--
times           :: (Monoid' a, Transformable a, HasOnset a, HasOffset a) =>
                Int -> a -> a

-- |
-- Repeat once for each element in the list.
--
-- Example:
--
-- > repeated [1,2,1] (c^*)
--
-- Simple type:
--
-- > [a] -> (a -> Score Note) -> Score Note
--
repeated        :: (Monoid' b, Transformable b, HasOnset b, HasOffset b) =>
                [a] -> (a -> b) -> b

-- |
-- Repeat a number of times and scale down by the same amount.
--
-- > Duration -> Score a -> Score a
--
group           :: (Monoid' a, Transformable a, Fractional d, d ~ Duration, HasOnset a, HasOffset a) =>
                Int -> a -> a

times n     = scat . replicate n
repeated    = flip (\f -> scat . fmap f)
group n     = times n . (fromIntegral n `compress`)

-}
