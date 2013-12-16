
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.Set(Set)
import Data.Map(Map)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Control.Monad (join)

type family a /~ b

type instance [a] /~ g       = [a /~ g]
type instance (Set a) /~ g   = Set (a /~ g)
type instance (Map k a) /~ g = Map k (a /~ g)
type instance (b -> a) /~ g  = b -> (a /~ g)
type instance (b, a) /~ g    = (b, a /~ g)

class HasFoo a where
    type Foo a
    getFoo    :: a -> Maybe (Foo a)
    setFoo    :: (b ~ (a /~ Foo b)) => Foo b -> a -> b
    modifyFoo :: (b ~ (a /~ Foo b)) => (Foo a -> Foo b) -> a -> b
    setFoo x      = modifyFoo (const x)
    -- modifyFoo f x = setFoo (maybe undefined f $ getFoo x) x


data FooT f a = FooT f a
type instance (FooT f a) /~ g = FooT g a
instance HasFoo (FooT f a) where
    type Foo (FooT f a)     = f
    getFoo (FooT f _)       = Just f
    modifyFoo g (FooT f x)  = FooT (g f) x


instance HasFoo a => HasFoo [a] where
    type Foo [a] = Foo a
    getFoo = join . fmap getFoo . listToMaybe
    modifyFoo g = fmap (modifyFoo g)

instance HasFoo a => HasFoo (Map k a) where
    type Foo (Map k a) = Foo a
    getFoo = getFoo . toList
    modifyFoo g = fmap (modifyFoo g)




class HasBar a where
    type Bar a
    getBar    :: a -> Maybe (Bar a)
    setBar    :: (b ~ (a /~ Bar b)) => Bar b -> a -> b
    modifyBar :: (b ~ (a /~ Bar b)) => (Bar a -> Bar b) -> a -> b
    setBar x      = modifyBar (const x)
    -- modifyBar f x = setBar (f $ getBar x) x


data BarT f a = BarT f a
type instance (BarT f a) /~ g = BarT g a
instance HasBar (BarT f a) where
    type Bar (BarT f a)     = f
    getBar (BarT f _)       = Just f
    modifyBar g (BarT f x)  = BarT (g f) x


instance HasBar a => HasBar [a] where
    type Bar [a] = Bar a
    getBar = join . fmap getBar . listToMaybe
    modifyBar g = fmap (modifyBar g)
    

