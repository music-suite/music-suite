
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative hiding (Const(..))
import Data.Monoid hiding (Sum(..), Product(..))
-- import Control.Monad.Identity

-- instance Show a => Show (Identity a) where
    -- show (Identity x) = "(Identity " ++ show x ++ ")"

data    Const m a       = Const { getConst :: m }
data    Identity a      = Identity { getIdentity :: a }
data    Product f g a   = Product (f a) (g a) deriving Show
data    Sum f g a       = InL (f a) | InR (g a) deriving Show
data    Compose f g a   = Compose { getCompose :: f (g a) } deriving Show
data    FixF f a        = FixF (f (FixF f) a)
-- Identity :: * -> *
-- Const    :: * -> * -> *
-- Product  :: (* -> *) -> (* -> *) -> * -> *
-- Sum      :: (* -> *) -> (* -> *) -> * -> *
-- Compose  :: (* -> *) -> (* -> *) -> * -> *
-- FixF     :: ((* -> *) -> * -> *) -> * -> *

-- Built from primitives

newtype Empty   a   = Empty   (Const () a)
newtype Maybe_    b = Maybe_  (Sum     Empty     Identity b)
newtype Either_ a b = Either_ (Sum     (Const a) Identity b)
newtype Pair_   a b = Pair_   (Product (Const a) Identity b)
-- Empty    :: * -> *
-- Maybe_   :: * -> *
-- Pair_    :: * -> * -> *
-- Either   :: * -> * -> *

{-
  TODO relate to terminology in Abjad/other environs/FRP
-}
type Rest     = Empty
type Note     = Identity
type NoteRest = Maybe_
type Chord = []






instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose $ (fmap . fmap) f x
 
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure
    (Compose f) <*> (Compose x) = Compose $ (<*>) <$> f <*> x

instance (Functor f, Functor g) => Functor (Product f g) where
    fmap f (Product  x y) = Product (fmap f x) (fmap f y)
 
instance (Applicative f, Applicative g) => Applicative (Product f g) where
    pure x = Product (pure x) (pure x)
    (Product f g) <*> (Product  x y) = Product (f <*> x) (g <*> y)


deriving instance (Show (f (FixF f) a)) => Show (FixF f a)
instance Functor (f (FixF f)) => Functor (FixF f) where
    fmap f (FixF x) = FixF $ fmap f x
 
instance Applicative (f (FixF f)) => Applicative (FixF f) where
    pure x = FixF $ pure x
    (FixF f) <*> (FixF x) = FixF (f <*> x)