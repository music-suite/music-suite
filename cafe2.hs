{-# LANGUAGE     
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    NoMonomorphismRestriction,
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    TypeFamilies,
    ViewPatterns,

    TypeSynonymInstances,
    FlexibleInstances,
    
    OverloadedStrings,
    TypeOperators
    #-}

import Data.AffineSpace
import Data.AffineSpace.Point
import Data.AdditiveGroup hiding (Sum, getSum)
import Data.VectorSpace hiding (Sum, getSum)
import Data.LinearMap

import Control.Monad
import Control.Arrow
import Control.Applicative
import Control.Monad.Writer hiding ((<>))
import Data.String
import Data.Semigroup
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import qualified Diagrams.Core.Transform as D

{-
    Compare
        - Update Monads: Cointerpreting directed containers
-}

newtype Foo m a = Foo (Writer m a)
    deriving (Monad, MonadWriter m, Functor, Foldable, Traversable)

newtype Writer2 m a = Writer2 (a, m)
    deriving (Show, Functor, Foldable, Traversable)
instance Monoid m => Monad (Writer2 m) where
    return x = Writer2 (x, mempty)
    Writer2 (x1,m1) >>= f = let
        Writer2 (x2,m2) = f x1
        in Writer2 (x2,m1 `mappend` m2)

-- instance Functor ((,) a)
deriving instance Monoid m => Foldable ((,) m)
deriving instance Monoid m => Traversable ((,) m)
instance Monoid m => Monad ((,) m) where
    return x =  (mempty, x)
    (m1,x1) >>= f = let
        (m2,x2) = f x1
        in (m1 `mappend` m2,x2)


newtype RawBar m a = RawBar { getRawBar :: [(m,a)] }
    deriving (Show, Semigroup, Monoid, Functor, Foldable, Traversable)
instance Monoid m => Monad (RawBar m) where
    return = RawBar . return . return
    RawBar xs >>= f = RawBar $ xs >>= joinTrav (getRawBar . f)



newtype Bar m a = Bar { getBar :: [Foo m a] }
    deriving (Semigroup, Monoid, Functor, Foldable, Traversable)
instance Monoid m => Applicative (Bar m) where
    pure = return
    (<*>) = ap
instance Monoid m => Monad (Bar m) where
    return = Bar . return . return
    Bar xs >>= f = Bar $ xs >>= joinTrav (getBar . f)

joinTrav :: (Monad t, Traversable t, Applicative f) => (a -> f (t b)) -> t a -> f (t b)
joinTrav f = fmap join . T.traverse f


{-
    Free theorem of sequence/dist    
        sequence . fmap (fmap k)  =  fmap (fmap k) . sequence

    Corollaries
        traverse (f . g)  =  traverse f . fmap g
        traverse (fmap k . f)  =  fmap (fmap k)  =   traverse f

-}





ns = undefined
f  = undefined
g = undefined

ns :: [Foo m a]
f :: (a -> [Foo m a])
g :: (a -> Foo m a)


-- f ns                             :: [Foo  m [Foo m1 a]]
-- T.mapM f ns                      :: [[Foo m (Foo m1 a)]]
-- T.mapM (T.mapM f) ns             :: [[Foo m (Foo m1 a)]]
-- T.mapM (T.mapM (T.mapM f)) ns    :: [[Foo m [(Foo m1 a)]]]

bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)



-- joinedSeq2 = fmap join . Traversable.mapM const
runFoo (Foo x) = runWriter x
runBar (Bar xs) = fmap runFoo xs

tells :: Monoid m => m -> Bar m a -> Bar m a
tells a (Bar xs) = Bar $ fmap (tell a >>) xs


type Annotated a = Bar [String] a
runAnnotated :: Annotated a -> [(a, [String])]
runAnnotated = runBar

-- annotate all elements in bar
annotate :: String -> Annotated a -> Annotated a
annotate x = tells [x]

-- a bar with no annotations
x :: Annotated Int
x = return 0

-- annotations compose with >>=
y :: Annotated Int
y = x <> annotate "a" x >>= (annotate "b" . return)

-- and with join
z :: Annotated Int
z = join $ annotate "d" $ return (annotate "c" (return 0) <> return 1)

-- runBar y ==> [(0,"b"),(0,"ab")]
-- runBar z ==> [(0,"dc"),(1,"d")]


{-
join' :: Monad t => t (t b) -> t b
join' = join

joinedSeq :: (Monad t, Traversable t, Applicative f) => t (f (t a)) -> f (t a)
joinedSeq = fmap join . T.sequenceA


bindSeq :: (Monad f, Applicative f, Traversable t) => f (t (f a)) -> f (t a)
bindSeq = bind T.sequenceA 

travBind :: (Monad f, Applicative f, Traversable t) => (a -> f b) -> t (f a) -> f (t b)
travBind f = T.traverse (bind f)
-}

type Time = Double
type Dur = Double

type TT = D.Transformation (Time,Dur)

applyTT :: TT -> (Time, Dur) -> (Time, Dur)
applyTT t = unPoint . D.papply t . P
    
delaying :: Time -> TT
delaying x = D.translation (x,0)

stretching :: Dur -> TT
stretching = D.scaling



-- type TT = [String]
-- 
-- applyTT m x = (m,x)
-- 
-- delaying :: Time -> TT
-- delaying x = return $ "delay " ++ show x
-- 
-- stretching :: Dur -> TT
-- stretching x = return $ "stretch " ++ show x



-- time transformation
-- type TT = (Onset, Scaling)
-- getTT = getSum *** getProduct

-- unitTT :: TT
-- unitTT = mempty


-- TODO this is wroing
-- scaling needs to be applied to onset as well!



addTT = tells
delay x = addTT (delaying x)
stretch x = addTT (stretching x)

type Score = Bar TT
-- Monoid, Functor, Applicative, Monad, Foldable, Traversable
instance (IsString a, t ~ TT) => IsString (Bar t a) where
    fromString = return . fromString

-- runScore :: Score a -> [((Time, Time), a)]
runScore = fmap (swap . fmap (flip applyTT (0,1))) . runBar

foo :: Score String
foo = stretch 2 $
    "c" <> (delay 1 ("d" <> stretch 0.1 "e"))



-- ((0.0,2.0),"c")
-- ((2.0,2.0),"d")
-- ((2.0,0.2),"e")
-- ((["stretch 2.0"],(0,1)),"c")
-- ((["stretch 2.0","delay 1.0"],(0,1)),"d")
-- ((["stretch 2.0","delay 1.0","stretch 0.1"],(0,1)),"e")

-- (0,1)
-- (0,0.5)
-- (1,0.5)
-- (2,1)



swap (x,y) = (y,x)