
{-# LANGUAGE OverlappingInstances, FlexibleInstances, TypeFamilies #-}

module RemoveRests where
import Control.Monad.Plus    
    
class Rests a where
    type NoRests a
    removeRests :: (Functor m, MonadPlus m) => m a -> m (NoRests a)

type Score = []

instance Rests a => Rests (Score a) where
    type NoRests (Score a) = Score (NoRests a)
    removeRests = fmap removeRests

instance Rests (Maybe a) where
    type NoRests (Maybe a) = a
    removeRests = mcatMaybes

-- Everything else is just identities

instance Rests Double where
    type NoRests Double = Double
    removeRests = id
instance Rests Int where
    type NoRests Int = Int
    removeRests = id
instance Rests a => Rests (b, a) where
    type NoRests (b,a) = (b,a)
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