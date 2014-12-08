
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- import Music.Prelude hiding (Reactive(..))
import Music.Time.Split
import Music.Time.Reverse
import Music.Time.Future
import Data.Semigroup
import Data.Maybe
import Data.List (sortBy)
import Data.Ord
import Control.Applicative

-- chords = [[c,e,g],[g,d,b]]

-- instance Applicative Future where
--   pure x = Future (pure x)
--   Future f <*> Future x = Future (f <*> x) 
-- instance Applicative Past

data Reactive a = Reactive a [Future a]

initial :: Reactive a -> a
initial (Reactive z xs) = z

final :: Reactive a -> a
final (Reactive z []) = z
final (Reactive _ xs) = snd $ getFuture $ last xs

-- intermediate
updates :: Reactive a -> [(Time, a)]
updates (Reactive _ []) = []
updates (Reactive _ xs) = fmap ((\(Max t, x) -> (t, x)) . getFuture) xs

occs :: Reactive a -> [Time]
occs = map fst . updates

atTime :: Reactive a -> Time -> a
atTime (Reactive z xs) t = fromMaybe z (indexFuture xs t)


----

-- MOVE
indexFuture :: [Future a] -> Time -> Maybe a
indexFuture ps t = firstTrue $ fmap (\p -> future p t) $ sortBy (comparing tv) ps
  where
    tv (Future (Max t, _)) = t
