

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

{-
class Mon m where
    type Typ m :: *          
    ret :: a -> m a

instance Mon [] where
    type Typ [] = Int
    ret x = [x]
    
instance Mon Maybe where
    type Typ Maybe = Float
    ret x = Just x
-}

{-
-- type family Typ a :: (* -> *) -> *
type family Typ (a :: * -> *) :: *

type instance Typ []    = Int
type instance Typ Maybe = Float


-- See that we can unify (Float ~ Typ Maybe)    
float = (undefined :: Float) :: Typ Maybe

-- See that we can unify (Int ~ Typ [])    
int   = (undefined :: Int)   :: Typ []

-}

type family Typ (a :: *) :: *

type instance Typ [a] = Int

_ = (undefined :: Typ [Int]) :: Int
_ = (undefined :: Typ [Float]) :: Int
_ = (undefined :: Typ [Float]) :: Typ [()]

(Typ s a ~ Typ s b)

-- -- See that we can unify (Float ~ Typ Maybe)    
-- _ = (undefined :: Float) :: Typ Maybe
-- 
-- -- See that we can unify (Int ~ Typ [])    
-- _   = (undefined :: Int)   :: Typ []



