
{-# LANGUAGE
    GeneralizedNewtypeDeriving, 
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    TypeFamilies,
    MultiParamTypeClasses,
    FlexibleInstances #-}

module Music.Score.Note (
        Note(..),
        unnote,
        getNoteSpan,
        getNoteValue,
        (=:),
  ) where

import Control.Monad
import Control.Comonad
import Control.Applicative

import Data.PairMonad ()
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Music.Time

newtype Note a = Note { getNote :: (Span, a) }
    deriving (Eq, Ord, Show, {-Read, -}Functor, Applicative, Monad, Foldable, Traversable)

unnote :: Note a -> (Span, a)
unnote (Note x) = x

-- | Get the span of the note. Same as 'era'.
getNoteSpan :: Note a -> Span
getNoteSpan = fst . unnote

-- | Get the value of the note.
getNoteValue :: Note a -> a
getNoteValue = snd . unnote 

instance Comonad Note where
    {-
        extract . duplicate      = id
            \(Note (s,x) -> x . \(Note (s,x)) -> Note (s,(Note (s,x))) = id
            \(Note (s,x)) -> (\(Note (s,x) -> x) Note (s,(Note (s,x))) = id
            (Note (s,x)) = (\(Note (s,x) -> x) Note (s,(Note (s,x)))
            (Note (s,x)) = (Note (s,x)
        fmap extract . duplicate = id
            TODO
        duplicate . duplicate    = fmap duplicate . duplicate
            TODO
    -}
    extract = getNoteValue
    duplicate (Note (s,x)) = Note (s,(Note (s,x)))

instance Delayable (Note a) where
    delay n (Note (s,x)) = Note (delay n s, x)

instance Stretchable (Note a) where
    stretch n (Note (s,x)) = Note (stretch n s, x)

instance HasOnset (Note a) where
    onset (Note (s,x)) = onset s

instance HasOffset (Note a) where
    offset (Note (s,x)) = offset s


-- | Construct a note from a span and value.
-- 
-- Typically used with the span constructors as in:
--
-- > 0 <-> 2 =: c
-- > 0 --> 1 =: d
--
(=:) :: Span -> a -> Note a
s =: x  =  Note (s,x)
