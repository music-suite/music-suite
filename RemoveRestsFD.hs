
{-# LANGUAGE OverlappingInstances, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module RemoveRests where
import Music.Score hiding (removeRests)
import Music.Pitch
import Control.Monad.Plus    
    
class Rests a b | a -> b where
    removeRests :: (Functor m, MonadPlus m) => m a -> m b

-- type Score = []

instance Rests a b => Rests (Score a) (Score b) where
    removeRests = fmap removeRests

instance Rests (Maybe a) a where
    removeRests = mcatMaybes

-- Everything else is just identities

instance Rests Double Double where
    removeRests = id
instance Rests Int Int where
    removeRests = id
instance Rests a a => Rests (b, a) (b, a) where
    removeRests = id

-- etc

{-
    removeRests [Nothing, Just 1, Just 2]
        ==> [1,2]

    removeRests [[Nothing, Just 1, Just 2], [Just 3]]
        ==> [[1,2],[3]]
        
    removeRests [[("h",1),("h",2::Int)]]
        ==> [[("h",1),("h",2)]]

    removeRests [Just [1, 2, 3], Just [], Nothing]
        ==> [[1,2,3],[]]

    etc
-}