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
import Music.Time.Voice
import Music.Time.Score

class HasText a where
  addText :: String -> a -> a

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

instance HasText a => HasText (b, a) where
  addText s = fmap (addText s)

instance HasText a => HasText (Couple b a) where
  addText s = fmap (addText s)

instance HasText a => HasText [a] where
  addText s = fmap (addText s)

instance HasText a => HasText (Note a) where
  addText s = fmap (addText s)

instance HasText a => HasText (Voice a) where
  addText s = fmap (addText s)

instance HasText a => HasText (Score a) where
  addText s = fmap (addText s)

instance Wrapped (TextT a) where

  type Unwrapped (TextT a) = Couple [String] a

  _Wrapped' = iso getTextT TextT

instance Rewrapped (TextT a) (TextT b)

instance HasText (TextT a) where
  addText s (TextT (Couple (t, x))) = TextT (Couple (t ++ [s], x))

-- Lifted instances
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
