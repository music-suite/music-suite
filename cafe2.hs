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

tells :: Monoid m => m -> WList m a -> WList m a
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

type Time = Double
type Dur  = Double
type Span = (Time, Dur)

newtype TT = TT (D.Transformation Span)
    deriving (Monoid, Semigroup)

-- TODO generalize all of these from TT to arbitrary T

instance Action TT Span where
    act (TT t) = unPoint . D.papply t . P
    
delaying :: Time -> TT
delaying x = TT $ D.translation (x,0)

stretching :: Dur -> TT
stretching = TT . D.scaling


----------------------------------------------------------------------

type Pitch = Double

newtype PT = PT (D.Transformation Pitch)
    deriving (Monoid, Semigroup)

instance Action PT Pitch where
    act (PT t) = unPoint . D.papply t . P
    
transposing :: Pitch -> PT
transposing x = PT $ D.translation x

----------------------------------------------------------------------

type Amplitude = Double

newtype DT = DT (D.Transformation Amplitude)
    deriving (Monoid, Semigroup)

instance Action DT Amplitude where
    act (DT t) = unPoint . D.papply t . P
    
amplifying :: Amplitude -> DT
amplifying = DT . D.scaling


-- ----------------------------------------------------------------------
-- 
-- -- Compose time transformations with another Monoid
-- -- Generalize this pattern?
-- 
-- -- Monoid and Semigroup instances compose by default
-- -- 'tells' works with all Monoids
-- -- Need a way to generalize constructors and apply
-- 
-- type TT2 = ([String],TT)
-- -- Monoid, Semigroup
-- 
-- liftTT2 :: TT -> TT2
-- liftTT2 = monR
-- 
-- monL :: Monoid b => a -> (a, b)
-- monL = swap . return
-- 
-- -- This is the Writer monad again
-- monR :: Monoid b => a -> (b, a)
-- monR = return
-- 
-- type PT = () -- Semigroup, Monoid
-- type DT = () -- Semigroup, Monoid
-- type AT = () -- Semigroup, Monoid
-- type RT = () -- Semigroup, Monoid
-- 
-- -- Nice pattern here!
-- type T = ((((((),AT),RT),DT),PT),TT)
--  -- Semigroup, Monoid
-- idT = (mempty::T)
-- 
-- newtype Tx = Tx T
--     deriving (Monoid, Semigroup)
-- 
-- -- TODO How to generalize applyTT2 (?)
-- -- All apply functions convert the monoidal transformation to
-- -- an endofunction (a -> a)
-- 
-- -- applyTT :: TT -> (Time, Dur) -> (Time, Dur)
-- -- applyPT :: PT -> Pitch     -> Pitch
-- -- applyDT :: PT -> Amplitude -> Amplitude
-- -- applyRT :: RT -> Part      -> Part
-- -- applyAT :: AT -> (Pitch, Amplitude) -> (Pitch, Amplitude)
-- -- applyST :: AT -> Point R3 -> Point R3
-- -- applyUnit :: () -> a -> a
-- 
-- -- This is Monoidal actions!
-- 
-- instance (Action t a, Action u b) => Action (t, u) (a, b) where
--     act (t, u) (a, b) = (act t a, act u b)
-- 
-- 
-- 
-- applyTT2 :: TT2 -> (Time, Dur) -> ((Time, Dur), [String])
-- applyTT2 (as,t) x = (applyTT t x, as)
--     
-- delaying2 :: Time -> TT2
-- delaying2 x = liftTT2 $ delaying x
-- 
-- stretching2 :: Dur -> TT2
-- stretching2 x = liftTT2 $ stretching x
-- 
-- -- addTT2 :: TT2 -> WList TT2 a -> WList TT2 a
-- -- addTT2 = tells
-- 
-- ----------------------------------------------------------------------
-- 
-- ----------------------------------------------------------------------

-- Accumulate transformations
delay x = tells (delaying x)
stretch x = tells (stretching x)
transpose x = tells (transposing x)
amplify x = tells (amplifying x)


type Score = WList DT
--  Monoid, Functor, Applicative, Monad, Foldable, Traversable

-- foo :: Score String
-- foo = stretch 2 $ "c" <> (delay 1 ("d" <> stretch 0.1 "e"))

foo :: Score String
foo = amplify 2 "c"



-- runScore' :: Action m b => b -> WList m a -> [(b, a)]
-- runScore' x = fmap (swap . fmap (flip act x)) . runWList
-- 
-- runScore :: Score a -> [(Span, a)]
-- runScore = runScore' ((0,1)::Span)




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