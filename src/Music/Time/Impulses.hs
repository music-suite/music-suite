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

-- |
-- >>> (Impulses $ Data.Map.singleton 2 On) `at ` 1
-- Off
--
-- >>> (Impulses $ Data.Map.singleton 2 On) `at` 2
-- On
--
-- >>> (Impulses $ Data.Map.singleton 2 On) `at` 3
-- Off
-- 
-- >>> ((Impulses $ Data.Map.singleton 2 On) <> (Impulses $ Data.Map.singleton 4 On)) `at` 1
-- Off
--
-- >>> ((Impulses $ Data.Map.singleton 2 On) <> (Impulses $ Data.Map.singleton 4 On)) `at` 2
-- On
--
-- >>> ((Impulses $ Data.Map.singleton 2 [1]) <> (Impulses $ Data.Map.singleton 2 [3])) `at` 2
-- [1]
--
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