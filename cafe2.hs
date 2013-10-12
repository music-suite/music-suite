{-# LANGUAGE     
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    NoMonomorphismRestriction,
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    TypeFamilies,
    ViewPatterns,

    MultiParamTypeClasses,
    TypeSynonymInstances,
    FlexibleInstances,
    
    OverloadedStrings,
    TypeOperators
    #-}

import Data.Monoid.Action
import Data.Monoid.MList -- misplaced Action () instance

import Data.Default
import Data.Basis
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

-- Simplified version of the standard writer monad.
--
newtype Write m a = Write (Writer m a)
    deriving (Monad, MonadWriter m, Functor, Foldable, Traversable)

{-
Alternative implementation:

newtype Write m a = Write (a, m)
    deriving (Show, Functor, Foldable, Traversable)
instance Monoid m => Monad (Write m) where
    return x = Write (x, mempty)
    Write (x1,m1) >>= f = let
        Write (x2,m2) = f x1
        in Write (x2,m1 `mappend` m2)
-}

-- Same thing with orinary pairs:

-- instance Functor ((,) a)
deriving instance Monoid m => Foldable ((,) m)
deriving instance Monoid m => Traversable ((,) m)
instance Monoid m => Monad ((,) m) where
    return x =  (mempty, x)
    (m1,x1) >>= f = let
        (m2,x2) = f x1
        in (m1 `mappend` m2,x2)

-- |
-- List of values in the Write monad.
--
newtype WList m a = WList { getWList :: [Write m a] }
    deriving (Semigroup, Monoid, Functor, Foldable, Traversable)
instance (IsString a, Monoid m) => IsString (WList m a) where
    fromString = return . fromString
instance Monoid m => Applicative (WList m) where
    pure = return
    (<*>) = ap
instance Monoid m => Monad (WList m) where
    return = WList . return . return
    WList xs >>= f = WList $ xs >>= joinTrav (getWList . f)

-- Same thing with orinary pairs:

newtype WList' m a = WList' { getWList' :: [(m, a)] }
    deriving (Show, Semigroup, Monoid, Functor, Foldable, Traversable)
instance Monoid m => Applicative (WList' m) where
    pure = return
    (<*>) = ap
instance Monoid m => Monad (WList' m) where
    return = WList' . return . return
    WList' xs >>= f = WList' $ xs >>= joinTrav (getWList' . f)

joinTrav :: (Monad t, Traversable t, Applicative f) => (a -> f (t b)) -> t a -> f (t b)
joinTrav f = fmap join . T.traverse f

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

{-
    Free theorem of sequence/dist    
        sequence . fmap (fmap k)  =  fmap (fmap k) . sequence

    Corollaries
        traverse (f . g)  =  traverse f . fmap g
        traverse (fmap k . f)  =  fmap (fmap k)  =   traverse f

-}

runWrite :: Write w a -> (a, w)
runWrite (Write x) = runWriter x

runWList :: WList w a -> [(a, w)]
runWList (WList xs) = fmap runWrite xs

-- tells :: Monoid m => m -> WList m a -> WList m a
tells a (WList xs) = WList $ fmap (tell a >>) xs




----------------------------------------------------------------------

type Annotated a = WList [String] a
runAnnotated :: Annotated a -> [(a, [String])]
runAnnotated = runWList

-- annotate all elements in bar
annotate :: String -> Annotated a -> Annotated a
annotate x = tells [x]

-- a bar with no annotations
ann1 :: Annotated Int
ann1 = return 0

-- annotations compose with >>=
ann2 :: Annotated Int
ann2 = ann1 <> annotate "a" ann1 >>= (annotate "b" . return)

-- and with join
ann3 :: Annotated Int
ann3 = join $ annotate "d" $ return (annotate "c" (return 0) <> return 1)
 


----------------------------------------------------------------------



----------------------------------------------------------------------

type T = ((((),DT),PT),TT)
idT = (mempty::T)

-- TODO speficy
instance (Action t a, Action u b) => Action (t, u) (a, b) where
    act (t, u) (a, b) = (act t a, act u b)  

-- This is the raw writer defined above
monR :: Monoid b => a -> (b, a)
monR = return

monL :: Monoid b => a -> (a, b)
monL = swap . return

class HasT a where
    liftT :: a -> T
instance HasT TT where
    liftT = monR
instance HasT PT where
    liftT = monL . monR
instance HasT DT where
    liftT = monL . monL . monR


-- untrip (a,b,c) = ((a,b),c)
-- trip ((a,b),c) = (a,b,c)

unpack3 ((c,b),a)                               = (c,b,a)
unpack4 (first unpack3 -> ((d,c,b),a))          = (d,c,b,a)
unpack5 (first unpack4 -> ((e,d,c,b),a))        = (e,d,c,b,a)
unpack6 (first unpack5 -> ((f,e,d,c,b),a))      = (f,e,d,c,b,a)

pack3 (c,b,a) = ()


-- first f = swap . fmap f . swap

actT :: T -> ((((),Amplitude),Pitch),Span) -> ((((),Amplitude),Pitch),Span)
actT = act


----------------------------------------------------------------------

-- Minimal API

-- type T = () -- Monoid
-- class HasT a where
--     liftT :: a -> T
-- instance HasT TT where
--     liftT = const ()
-- instance HasT PT where
--     liftT = const ()
-- instance HasT DT where
--     liftT = const ()
-- 
-- actT :: T -> ((((),Amplitude),Pitch),Span) -> ((((),Amplitude),Pitch),Span)
-- actT = const id  

----------------------------------------------------------------------

type Time = Double
type Dur  = Double
type Span = (Time, Dur)

newtype TT = TT (D.Transformation Span)
    deriving (Monoid, Semigroup)

-- TODO generalize all of these from TT to arbitrary T

instance Action TT Span where
    act (TT t) = unPoint . D.papply t . P
    
delaying :: Time -> T
delaying x = liftT $ TT $ D.translation (x,0)

stretching :: Dur -> T
stretching = liftT . TT . D.scaling


----------------------------------------------------------------------

type Pitch = Double

newtype PT = PT (D.Transformation Pitch)
    deriving (Monoid, Semigroup)

instance Action PT Pitch where
    act (PT t) = unPoint . D.papply t . P
    
transposing :: Pitch -> T
transposing x = liftT $ PT $ D.translation x

----------------------------------------------------------------------

type Amplitude = Double

newtype DT = DT (D.Transformation Amplitude)
    deriving (Monoid, Semigroup)

instance Action DT Amplitude where
    act (DT t) = unPoint . D.papply t . P
    
amplifying :: Amplitude -> T
amplifying = liftT . DT . D.scaling

----------------------------------------------------------------------


-- Accumulate transformations
delay x = tells (delaying x)
stretch x = tells (stretching x)
transpose x = tells (transposing x)
amplify x = tells (amplifying x)


-- newtype Score a = Score { getScore :: WList T a }
    -- deriving (Monoid, Functor, Applicative, Monad, Foldable, Traversable)
-- instance IsString a => IsString (Score a) where
--     fromString = Score . fromString

type Score = WList T

-- TODO use value
runScore' = fmap (\(x,t) -> 
    (actT t ((((),1),60),(0,1)), x)
    ) . runWList {- . getScore -}

runScore :: Score a -> [(Dur, Time, Pitch, Amplitude, a)]
runScore = fmap (\(((((),n), p), (t, d)), x) -> (d,t,p,n,x)) . runScore'


foo :: Score String    
foo = stretch 2 $ "c" <> (delay 1 ("d" <> stretch 0.1 "e"))

-- foo :: Score String
-- foo = amplify 2 $ transpose 1 $ delay 2 $ "c"




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