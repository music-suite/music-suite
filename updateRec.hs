
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Set(Set)
import Data.Map(Map)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Control.Monad (join)


-- type family   Rep a b x
-- type instance Rep a b [x] = [Rep a b x]
-- type instance (a ~ b) => Rep a b x = x 


class (SetFoo (Foo a) a ~ a) => HasFoo a where
    type Foo a
    type SetFoo b s
    getFoo  :: a -> Foo a
    setFoo' :: Foo a -> a -> a
    setFoo :: (b ~ SetFoo (Foo b) a) => Foo b -> a -> b
    setFoo' = setFoo

modifyFoo :: (HasFoo a, b ~ SetFoo (Foo b) a) => (Foo a -> Foo b) -> a -> b
modifyFoo f x = setFoo (f $ getFoo x) x

modifyFoo' :: HasFoo a => (Foo a -> Foo a) -> a -> a
modifyFoo' = modifyFoo

data FooT f a = FooT f a

instance HasFoo (FooT f a) where
    type Foo      (FooT f a) = f
    type SetFoo g (FooT f a) = FooT g a 
    getFoo        (FooT f a) = f
    setFoo      g (FooT f a) = FooT g a

instance HasFoo a => HasFoo [a] where
    type Foo [a]      = Foo a
    type SetFoo b [a] = [SetFoo b a]
    getFoo   = getFoo . head
    setFoo x = fmap (setFoo x)

instance HasFoo a => HasFoo (Map k a) where
    type Foo (Map k a) = Foo a
    type SetFoo b (Map k a) = Map k (SetFoo b a)
    getFoo   = getFoo . toList
    setFoo x = fmap (setFoo x)


class (SetBar (Bar a) a ~ a) => HasBar a where
    type Bar a
    type SetBar b s
    getBar  :: a -> Bar a
    setBar' :: Bar a -> a -> a
    setBar :: (b ~ SetBar (Bar b) a) => Bar b -> a -> b
    setBar' = setBar

modifyBar :: (HasBar a, b ~ SetBar (Bar b) a) => (Bar a -> Bar b) -> a -> b
modifyBar f x = setBar (f $ getBar x) x

modifyBar' :: HasBar a => (Bar a -> Bar a) -> a -> a
modifyBar' f x = setBar' (f $ getBar x) x

data BarT f a = BarT f a

instance HasBar (BarT f a) where
    type Bar      (BarT f a) = f
    type SetBar g (BarT f a) = BarT g a 
    getBar        (BarT f a) = f
    setBar      g (BarT f a) = BarT g a

instance HasBar a => HasBar [a] where
    type Bar [a]      = Bar a
    type SetBar b [a] = [SetBar b a]
    getBar   = getBar . head
    setBar x = fmap (setBar x)

instance HasBar a => HasBar (Map k a) where
    type Bar (Map k a) = Bar a
    type SetBar b (Map k a) = Map k (SetBar b a)
    getBar   = getBar . toList
    setBar x = fmap (setBar x)

instance HasFoo a => HasFoo (BarT b a) where
    type Foo (BarT b a) = Foo a
    type SetFoo c (BarT b a) = BarT b (SetFoo c a)
    getFoo (BarT _ x) = getFoo x
    setFoo f (BarT b x) = BarT b (setFoo f x)


        
    -- modifyFoo' :: (Foo a -> Foo a) -> a -> a
    -- 
    -- setFoo     :: (b ~ (ReplaceFoo a (Foo a) (Foo b))) => Foo b -> a -> b
    -- modifyFoo  :: (b ~ (ReplaceFoo a (Foo a) (Foo b))) => (Foo a -> Foo b) -> a -> b
    -- modifyFoo' = modifyFoo
    --                                  
-- 
-- -- modifyFoo' :: (HasFoo a, a ~ b, Foo a ~ Foo b) => (Foo a -> Foo b) -> a -> b
-- -- modifyFoo' f x = setFoo (getFoo x) x
-- 
-- -- modifyFoo' :: (HasFoo a, Foo (UnFoo a fooB) ~ fooB) => (Foo a -> fooB) -> a -> UnFoo a fooB
-- modifyFoo' :: (HasFoo a) => (Foo a -> Foo a) -> a -> a
-- modifyFoo' = modifyFoo

-- class HasBar a where
--     type Bar a
--     type UnBar a b
--     getBar    :: a -> Bar a
--     setBar    :: (b ~ (UnBar a (Bar b))) => Bar b -> a -> b
--     modifyBar :: (b ~ (UnBar a (Bar b))) => (Bar a -> Bar b) -> a -> b
-- 
-- 

-- 
-- instance HasFoo a => HasFoo [a] where
--     type Foo [a] = Foo a
--     type UnFoo [a] g = [UnFoo a g]
--     getFoo = getFoo . head
--     setFoo x = fmap (setFoo x)
--     modifyFoo g = fmap (modifyFoo g)
-- 
-- instance HasFoo a => HasFoo (Map k a) where
--     type Foo (Map k a) = Foo a
--     type UnFoo (Map k a) g = Map k (UnFoo a g)
--     getFoo = getFoo . head . toList
--     setFoo x = fmap (setFoo x)
--     modifyFoo g = fmap (modifyFoo g)
-- 
-- 
-- 
-- 
-- data BarT f a = BarT f a
-- 
-- instance HasBar (BarT f a) where
--     type Bar   (BarT f a)    = f
--     type UnBar (BarT f a) g  = BarT g a
--     getBar (BarT f _)       = f
--     setBar f (BarT _ x)     = BarT f x 
--     modifyBar g (BarT f x)  = BarT (g f) x
-- 
-- instance HasBar a => HasBar [a] where
--     type Bar [a] = Bar a
--     type UnBar [a] g = [UnBar a g]
--     getBar = getBar . head
--     setBar x = fmap (setBar x)
--     modifyBar g = fmap (modifyBar g)
-- 
-- instance HasBar a => HasBar (Map k a) where
--     type Bar (Map k a) = Bar a
--     type UnBar (Map k a) g = Map k (UnBar a g)
--     getBar = getBar . head . toList
--     setBar x = fmap (setBar x)
--     modifyBar g = fmap (modifyBar g)
-- 
-- 
-- 
-- 
-- 
-- -- Cross-instances
-- instance HasFoo a => HasFoo (BarT b a) where
--     type Foo (BarT b a) = Foo a
--     type UnFoo (BarT b a) c = BarT b (UnFoo a c)
--     getFoo (BarT _ x) = getFoo x
--     setFoo f (BarT b x) = BarT b (setFoo f x)
--     modifyFoo f (BarT b x) = BarT b (modifyFoo f x)
-- 
-- instance HasBar a => HasBar (FooT b a) where
--     type Bar (FooT b a) = Bar a
--     type UnBar (FooT b a) c = FooT b (UnBar a c)
--     getBar (FooT _ x) = getBar x
--     setBar f (FooT b x) = FooT b (setBar f x)
--     modifyBar f (FooT b x) = FooT b (modifyBar f x)
--                                              


   
-- class HasFoo a where
--     type Foo a
--     getFoo    :: a -> Maybe (Foo a)
--     setFoo    :: (b ~ (a /~ Foo b)) => Foo b -> a -> b
--     modifyFoo :: (b ~ (a /~ Foo b)) => (Foo a -> Foo b) -> a -> b
--     setFoo x      = modifyFoo (const x)
--     -- modifyFoo f x = setFoo (maybe undefined f $ getFoo x) x




-- 
-- import Data.Set(Set)
-- import Data.Map(Map)
-- import Data.Foldable (toList)
-- import Data.Maybe (listToMaybe)
-- import Control.Monad (join)
-- 
-- type family a /~ b
-- 
-- type instance [a]       /~ g    = [a /~ g]
-- type instance (Set a)   /~ g    = Set (a /~ g)
-- type instance (Map k a) /~ g    = Map k (a /~ g)
-- type instance (b -> a)  /~ g    = b -> (a /~ g)
-- type instance (b, a)    /~ g    = (b, a /~ g)
-- 
-- class HasFoo a where
--     type Foo a
--     getFoo    :: a -> Maybe (Foo a)
--     setFoo    :: (b ~ (a /~ Foo b)) => Foo b -> a -> b
--     modifyFoo :: (b ~ (a /~ Foo b)) => (Foo a -> Foo b) -> a -> b
--     setFoo x      = modifyFoo (const x)
--     -- modifyFoo f x = setFoo (maybe undefined f $ getFoo x) x
-- 
-- -- modifyFoo' :: (Foo a -> Foo a) -> a -> a
-- -- modifyFoo' = modifyFoo
-- 
-- 
-- data FooT f a = FooT f a
-- type instance (FooT f a) /~ g = FooT g a
-- instance HasFoo (FooT f a) where
--     type Foo (FooT f a)     = f
--     getFoo (FooT f _)       = Just f
--     modifyFoo g (FooT f x)  = FooT (g f) x
-- 
-- 
-- instance HasFoo a => HasFoo [a] where
--     type Foo [a] = Foo a
--     getFoo = join . fmap getFoo . listToMaybe
--     modifyFoo g = fmap (modifyFoo g)
-- 
-- instance HasFoo a => HasFoo (Map k a) where
--     type Foo (Map k a) = Foo a
--     getFoo = getFoo . toList
--     modifyFoo g = fmap (modifyFoo g)
-- 
-- 
-- 
-- 
-- class HasBar a where
--     type Bar a
--     getBar    :: a -> Maybe (Bar a)
--     setBar    :: (b ~ (a /~ Bar b)) => Bar b -> a -> b
--     modifyBar :: (b ~ (a /~ Bar b)) => (Bar a -> Bar b) -> a -> b
--     setBar x      = modifyBar (const x)
--     -- modifyBar f x = setBar (f $ getBar x) x
-- 
-- 
-- data BarT f a = BarT f a
-- type instance (BarT f a) /~ g = BarT g a
-- instance HasBar (BarT f a) where
--     type Bar (BarT f a)     = f
--     getBar (BarT f _)       = Just f
--     modifyBar g (BarT f x)  = BarT (g f) x
-- 
-- 
-- instance HasBar a => HasBar [a] where
--     type Bar [a] = Bar a
--     getBar = join . fmap getBar . listToMaybe
--     modifyBar g = fmap (modifyBar g)
--     
-- 
--                                              