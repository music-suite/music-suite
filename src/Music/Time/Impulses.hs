{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Music.Time.Impulses
  (
    Impulses(..),
    at,
    OnOff(..),
  )
where
import Data.Maybe
import Data.Bifunctor (first)
import Data.Map(Map)
import qualified Data.Map
import Music.Time.Juxtapose

newtype Impulses a = Impulses { getImpulses :: Map Time a }
  deriving (Eq, Ord, Show, Semigroup, Monoid)

at :: Monoid a => Impulses a -> Time -> a
at (Impulses xs) k = fromMaybe mempty $ Data.Map.lookup k xs

instance Transformable (Impulses a) where
  transform t (Impulses xs) = Impulses $ Data.Map.fromList $ fmap (first $ transform t) $ Data.Map.toList xs


data OnOff = On | Off deriving (Eq, Show, Ord)
instance Transformable OnOff where
  transform _ x = x
instance Semigroup OnOff where
  Off <> Off = Off
  _ <> _ = On
instance Monoid OnOff where
  mempty = Off