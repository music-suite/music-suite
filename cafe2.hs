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
    TypeSynonymInstances, -- TODO remove
    FlexibleInstances,
    
    OverloadedStrings,
    TypeOperators
    #-}

module Cafe2 (
    -- * Internal
    Trans,
    tapp,
    runTrans,
    runTransWith,
    renderTrans,
    TList,
    tlist,
    tapply,
    fromList,
    runTList,
    runTListWith,
    renderTList,
    
    -- * Annotations
    Annotated(..),
    runAnnotated,
    annotate,

    -- * Transformations
    Transformation,
    HasTransformation(..),
    transform,
    
    -- ** Specific transformations
    -- *** Time
    Time,
    Dur,
    Span,
    TT(..),
    delaying,
    stretching,
    delay,
    stretch,

    -- *** Pitch
    Pitch,
    PT(..),
    transposing,
    transpose,

    -- *** Dynamics
    Amplitude,
    AT(..),
    amplifying,
    amplify,

    -- * Score
    Score,   
    runScore,
) where

import Data.Monoid.Action
import Data.Monoid.MList -- misplaced Action () instance

import Data.Key
import Data.Default
import Data.Basis
import Data.Maybe
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.AdditiveGroup hiding (Sum, getSum)
import Data.VectorSpace hiding (Sum, getSum)
import Data.LinearMap

import Control.Monad
import Control.Monad.Plus
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

-- |
-- 'Trans' is a restricted version of the 'Writer' monad.
--
newtype Trans m a = Trans (Writer m a)
    deriving (Monad, MonadWriter m, Functor, Applicative, Foldable, Traversable)
instance (Monoid m, Monoid a) => Semigroup (Trans m a) where 
    -- TODO not sure this must require Monoid m
    Trans w <> Trans u = writer $ runWriter w `mappend` runWriter u
instance (Monoid m, Monoid a) => Monoid (Trans m a) where 
    mempty  = return mempty
    mappend = (<>)

type instance Key (Trans m) = m
instance Keyed (Trans m) where
    mapWithKey f (Trans w) = Trans $ mapWriter (\(a,w) -> (f w a, w)) w

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
-- Transformable list: semantically a list of values in the 'Trans' monad.
--
-- TODO This will actually work with any traversable monad, including Writer, Logic, List and Maybe.
--
newtype LTrans m a = LTrans { getLTrans :: Trans m [a] }
    deriving (Semigroup, Monoid, Functor, Foldable, Traversable)
instance (IsString a, Monoid m) => IsString (LTrans m a) where
    fromString = return . fromString
instance Monoid m => Applicative (LTrans m) where
    pure = return
    (<*>) = ap
instance Monoid m => Monad (LTrans m) where
    return = LTrans . return . return
    LTrans xs >>= f = LTrans $ xs >>= joinTrav (getLTrans . f)
instance Monoid m => MonadPlus (LTrans m) where
    mzero = mempty
    mplus = (<>)


-- |
-- Transformable list: semantically a list of values in the 'Trans' monad.
--
-- TODO This will actually work with any traversable monad, including Writer, Logic, List and Maybe.
--
newtype TList m a = TList { getTList :: [Trans m a] }
    deriving (Semigroup, Monoid, Functor, Foldable, Traversable)
instance (IsString a, Monoid m) => IsString (TList m a) where
    fromString = return . fromString
instance Monoid m => Applicative (TList m) where
    pure = return
    (<*>) = ap
instance Monoid m => Monad (TList m) where
    return = TList . return . return
    TList xs >>= f = TList $ xs >>= joinTrav (getTList . f)
instance Monoid m => MonadPlus (TList m) where
    mzero = mempty
    mplus = (<>)
type instance Key (TList m) = m
instance Keyed (TList m) where
    mapWithKey f (TList xs) = TList $ fmap (mapWithKey f) xs

joinTrav :: (Monad t, Traversable t, Applicative f) => (a -> f (t b)) -> t a -> f (t b)
joinTrav f = fmap join . T.traverse f

runTrans :: Action m a => Trans m a -> a
runTrans = runTransWith act

runTransWith :: (m -> a -> a) -> Trans m a -> a
runTransWith f = uncurry (flip f) . renderTrans

renderTrans :: Trans m a -> (a, m)
renderTrans (Trans x) = runWriter x

tapp m = (tell m >>)

-- | Construct a 'TList' from a transformation and a list.
tlist :: Monoid m => m -> [a] -> TList m a
tlist m xs = tapply m (fromList xs)

-- | Construct a 'TList' from a list.
fromList :: Monoid m => [a] -> TList m a
fromList = mfromList

-- | Transform a list using the given transformation.
--
-- > tapply (f <> g) = tapply f . tapply g
--
tapply :: Monoid m => m -> TList m a -> TList m a
tapply m (TList xs) = TList $ fmap (tapp m) xs

-- | Extract the components.
runTList :: Action m a => TList m a -> [a]
runTList (TList xs) = fmap runTrans xs

-- | Extract the components using the supplied function to render the cached transformations.
runTListWith :: (m -> b -> b) -> TList m b -> [b]
runTListWith f (TList xs) = fmap (runTransWith f) xs

-- | Extract the components and cached transformations.
renderTList :: TList m a -> [(a, m)]
renderTList (TList xs) = fmap renderTrans xs




-- TODO move

mapplyIf :: (Functor f, MonadPlus f) => (a -> Maybe a) -> f a -> f a
mapplyIf f = mapplyWhen (predicate f) (fromMaybe (error "mapplyIf") . f)

mapplyWhen :: (Functor f, MonadPlus f) => (a -> Bool) -> (a -> a) -> f a -> f a
mapplyWhen p f xs = let (ts, fs) = mpartition p xs
    in fmap f ts `mplus` fs



----------------------------------------------------------------------

newtype Annotated a = Annotated (TList [String] a)
    deriving (Functor, Applicative, Monad, MonadPlus, Semigroup, Monoid)
instance Num a => Num (Annotated a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = pure . fromInteger

runAnnotated :: Annotated a -> [(a, [String])]
runAnnotated (Annotated a) = renderTList a

-- annotate all elements in bar
annotate :: String -> Annotated a -> Annotated a
annotate x (Annotated a) = Annotated $ tapply [x] $ a

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



-- ----------------------------------------------------------------------
-- 
-- type T = ((((),AT),PT),TT)
-- idT = (mempty::T)
-- 
-- -- TODO speficy
-- instance (Action t a, Action u b) => Action (t, u) (a, b) where
--     act (t, u) (a, b) = (act t a, act u b)  
-- 
-- -- This is the raw writer defined above
-- monR :: Monoid b => a -> (b, a)
-- monR = return
-- 
-- monL :: Monoid b => a -> (a, b)
-- monL = swap . return
-- 
-- class HasT a where
--     makeTransformation :: a -> T
-- instance HasT TT where
--     makeTransformation = monR
-- instance HasT PT where
--     makeTransformation = monL . monR
-- instance HasT AT where
--     makeTransformation = monL . monL . monR
-- 
-- 
-- -- untrip (a,b,c) = ((a,b),c)
-- -- trip ((a,b),c) = (a,b,c)
-- 

unpack3 :: ((a, b), c) -> (a, b, c)
unpack4 :: (((a, b), c), d) -> (a, b, c, d)
unpack5 :: ((((a, b), c), d), e) -> (a, b, c, d, e)
unpack6 :: (((((a, b), c), d), e), f) -> (a, b, c, d, e, f)

unpack3 ((c,b),a)                           = (c,b,a)
unpack4 ((unpack3 -> (d,c,b),a))            = (d,c,b,a)
unpack5 ((unpack4 -> (e,d,c,b),a))          = (e,d,c,b,a)
unpack6 ((unpack5 -> (f,e,d,c,b),a))        = (f,e,d,c,b,a)

pack3 :: (b, c, a) -> ((b, c), a)
pack4 :: (c, d, b, a) -> (((c, d), b), a)
pack5 :: (d, e, c, b, a) -> ((((d, e), c), b), a)
pack6 :: (e, f, d, c, b, a) -> (((((e, f), d), c), b), a)

pack3 (c,b,a)                               = ((c,b),a)
pack4 (d,c,b,a)                             = (pack3 (d,c,b),a)
pack5 (e,d,c,b,a)                           = (pack4 (e,d,c,b),a)
pack6 (f,e,d,c,b,a)                         = (pack5 (f,e,d,c,b),a)


-- -- first f = swap . fmap f . swap
-- 
-- actT :: T -> (Amplitude,Pitch,Span) -> (Amplitude,Pitch,Span)
-- actT = act


----------------------------------------------------------------------

-- Minimal API

type Transformation t p a = TT ::: PT ::: AT ::: () -- Monoid
class HasTransformation a where
    makeTransformation :: a -> Transformation t p a
instance HasTransformation TT where
    makeTransformation = inj
instance HasTransformation PT where
    makeTransformation = inj
instance HasTransformation AT where
    makeTransformation = inj

-- Use identity if no such transformation
get' = option mempty id . get

transform :: Transformation t p a -> (Amplitude,Pitch,Span) -> (Amplitude,Pitch,Span)
transform u (a,p,s) = (a2,p2,s2)  
    where
        a2 = act (get' u :: AT) a
        p2 = act (get' u :: PT) p   
        s2 = act (get' u :: TT) s

----------------------------------------------------------------------

type Time = Double
type Dur  = Double
type Span = (Time, Dur)

newtype TT = TT (D.Transformation Span)
    deriving (Monoid, Semigroup)

-- TODO generalize all of these from TT to arbitrary T

instance Action TT Span where
    act (TT t) = unPoint . D.papply t . P
    
delaying :: Time -> Transformation t p a
delaying x = makeTransformation $ TT $ D.translation (x,0)

stretching :: Dur -> Transformation t p a
stretching = makeTransformation . TT . D.scaling


----------------------------------------------------------------------

type Pitch = Double

newtype PT = PT (D.Transformation Pitch)
    deriving (Monoid, Semigroup)

instance Action PT Pitch where
    act (PT t) = unPoint . D.papply t . P
    
transposing :: Pitch -> Transformation t p a
transposing x = makeTransformation $ PT $ D.translation x

----------------------------------------------------------------------

type Amplitude = Double

newtype AT = AT (D.Transformation Amplitude)
    deriving (Monoid, Semigroup)

instance Action AT Amplitude where
    act (AT t) = unPoint . D.papply t . P
    
amplifying :: Amplitude -> Transformation t p a
amplifying = makeTransformation . AT . D.scaling

----------------------------------------------------------------------


-- Accumulate transformations
delay x     = tapply (delaying x)
stretch x   = tapply (stretching x)
transpose x = tapply (transposing x)
amplify x   = tapply (amplifying x)


-- type Score = TList (Transformation Time Pitch Amplitude)
-- 
-- -- TODO move act formalism up to TList in generalized form
-- -- TODO generalize transformations
-- runScore' :: Score a -> [(Amplitude, Pitch, Span)]
-- runScore' = runTListWith (\t b -> transform t b) . fmap (const defT)
-- 
-- defT = (1,60,(0,1))
-- 
-- runScore :: Score a -> [(Time, Dur, Pitch, Amplitude)]
-- runScore = fmap (\(n,p,(t,d)) -> (t,d,p,n)) . runScore'
-- 
-- 
-- foo :: Score String    
-- foo = stretch 2 $ "c" <> (delay 1 ("d" <> stretch 0.1 "e"))
-- 
-- foo :: Score String
-- foo = amplify 2 $ transpose 1 $ delay 2 $ "c"

type Onset   = Sum Time
type Offset  = Sum Time
type Score a = TList (Onset, Offset) a
runScore = runTList


{-
    Time:
        For each note: onset and onset (or equivalently onset and duration)
        Optionally more paramters such as pre-onset, post-onset, post-offset
            [preOn A on D postOn S off R postOff]
    Pitch:
        For each note a custom pitch value (usually Common.Pitch)
        Optionally for each note a gliss/slide curve, indicating shape to next note
            (generally a simple slope function)
        Alternatively, for each part a (Behaviour Frequency) etc
    Dynamics:
        For each note an integral dynamic value (usually Common.Level)
        Optionally for each note a cresc/dim curve, indicating shape to next note
            (generally a simple slope function)
        Alternatively, for each part a (Behaviour Amplitude) etc
    Part:
        For each note an ordered integral (or fractional) indicator of instrument and/or subpart
    Space:
        For each note a point in R2 or R3
        Alternatively, for each part a (Behaviour R2) etc
    Misc/Timbre:
        Instrument changes, playing technique or timbral changes, i.e "sul pont -> sul tasto"
    Articulation:
        Affects pitch, dynamics and timbre
-}

-- data DScore m a = Node a | Nest (TList m (DScore m a)) 
--     deriving (Functor, Foldable)
-- instance Monoid m => Semigroup (DScore m a) where
--     Node x <> y      = Nest (return (Node x)) <> y
--     x      <> Node y = x <> Nest (return (Node y))
--     Nest x <> Nest y = Nest (x <> y)
-- -- TODO Monad
-- -- TODO MonadPlus



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

