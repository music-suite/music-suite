
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
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

module Music.Time.Note (
        -- * Note type
        Note,

        -- * Construction
        note,
        event,
        noteValue,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens             hiding (Indexable, Level, above,
                                           below, index, inside, parts,
                                           reversed, transform, (<|), (|>))
import           Data.PairMonad
import           Data.String
import           Data.VectorSpace
import           Data.Foldable            (Foldable)
import qualified Data.Foldable            as Foldable
import           Data.Typeable

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Internal.Util (through, tripped)
import           Music.Time.Reverse
import           Music.Time.Split


-- |
-- A 'Note' is a value with an 'onset' and and 'offset' in time. It is an instance
-- of 'Transformable'.
--
-- You can use 'value' to apply a function in the context of the transformation,
-- i.e.
--
-- @
-- over value (* line) (delay 2 $ return line)
-- @
--
-- @
-- ('view' 'value') . 'transform' s = 'transform' s . ('view' 'value')
-- @
--
newtype Note a = Note { _noteValue :: (Span, a) }
  deriving (Eq,
            Functor,
            Foldable,
            Traversable,
            Comonad,
            Typeable)

instance (Show a, Transformable a) => Show (Note a) where
  show x = show (x^.from note) ++ "^.note"

-- |
-- Note is a 'Monad' and 'Applicative' in the style of pair, with 'return' placing a value
-- at the default span 'mempty' and 'join' composing time transformations.
deriving instance Monad Note
deriving instance Applicative Note

instance Wrapped (Note a) where
  type Unwrapped (Note a) = (Span, a)
  _Wrapped' = iso _noteValue Note

instance Rewrapped (Note a) (Note b)

instance Transformable (Note a) where
  transform t = over (_Wrapped . _1) (transform t)

instance HasDuration (Note a) where
  _duration = _duration . fst . view _Wrapped

instance HasPosition (Note a) where
  _era = view (from note . _1)

instance Splittable a => Splittable (Note a) where
  -- beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning (transform (negateV s) d) v)
  beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning (d / _duration s) v)
  ending    d = over _Wrapped $ \(s, v) -> (ending    d s, ending    (d / _duration s) v)

instance Reversible (Note a) where
  rev = revDefault

-- Lifted instances

instance IsString a => IsString (Note a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Note a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Note a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Note a) where
  fromDynamics = pure . fromDynamics

-- |
-- View a note as a pair of the original value and the transformation (and vice versa).
--
note :: ({-Transformable a, Transformable b-}) => Iso (Span, a) (Span, b) (Note a) (Note b)
note = _Unwrapped

-- |
-- View the value in the note.
--
noteValue :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b
noteValue = lens runNote (flip $ mapNote . const)
  where
    runNote = uncurry transform . view _Wrapped
    -- setNote f (view (from note) -> (s,x)) = view note (s, itransform s x)
    mapNote f (view (from note) -> (s,x)) = view note (s, f `whilst` negateV s $ x)
    f `whilst` t = over (transformed t) f
{-# INLINE noteValue #-}

-- |
-- View a note as an events, i.e. a time-duration-value triplet.
--
event :: Iso (Note a) (Note b) (Time, Duration, a) (Time, Duration, b)
event = from note . bimapping delta id . tripped



