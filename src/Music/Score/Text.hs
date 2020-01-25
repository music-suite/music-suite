{-# OPTIONS_GHC
  -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  #-}
{-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE StandaloneDeriving, DerivingStrategies, DeriveAnyClass #-}
-- | Provides a way of adding text to notes.
module Music.Score.Text
  ( -- * Text
    HasText (..),
    TextT (..),
    runTextT,
    text,
    textLast,
  )
where

import Control.Applicative
import Control.Comonad
import Control.Lens hiding (transform)
import Data.Foldable
import Data.Foldable
import Data.Functor.Couple
import Data.Ratio
import Data.Semigroup
import Data.Typeable
import Data.Word
import Music.Dynamics.Literal
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Music.Pitch.Literal
import Music.Score.Part
import Music.Score.Phrases
import Music.Time.Note
import Music.Time.Score
import Music.Time.Voice

class HasText a where
  addText :: String -> a -> a

  default addText :: forall f b . (a ~ f b, Functor f, HasText b) => String -> a -> a
  addText s = fmap (addText s)

newtype TextT a = TextT {getTextT :: Couple [String] a}
  deriving
    ( Eq,
      Show,
      Ord,
      Functor,
      Foldable,
      Typeable,
      Applicative,
      Monad,
      Comonad
    )

runTextT :: TextT a -> ([String], a)
runTextT (TextT (Couple (ts, x))) = (ts, x)


instance HasText a => HasText (b, a)

instance HasText a => HasText (Couple b a)

instance HasText a => HasText [a]

instance HasText a => HasText (Note a)

instance HasText a => HasText (Voice a)

instance HasText a => HasText (Score a)

instance Wrapped (TextT a) where

  type Unwrapped (TextT a) = Couple [String] a

  _Wrapped' = iso getTextT TextT

instance Rewrapped (TextT a) (TextT b)

instance HasText (TextT a) where
  addText s (TextT (Couple (t, x))) = TextT (Couple (t ++ [s], x))

deriving instance Num a => Num (TextT a)

deriving instance Fractional a => Fractional (TextT a)

deriving instance Floating a => Floating (TextT a)

deriving instance Enum a => Enum (TextT a)

deriving instance Bounded a => Bounded (TextT a)

deriving instance (Num a, Ord a, Real a) => Real (TextT a)

deriving instance (Real a, Enum a, Integral a) => Integral (TextT a)

-- |
-- Attach the given text to the first note.
text :: (HasPhrases' s a, HasText a) => String -> s -> s
text s = over (phrases' . _head) (addText s)

-- |
-- Attach the given text to the last note.
textLast :: (HasPhrases' s a, HasText a) => String -> s -> s
textLast s = over (phrases' . _last) (addText s)
