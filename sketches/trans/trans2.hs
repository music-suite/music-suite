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

module Trans2 (
    -- * Basics
    -- $basics
    
    module Control.Monad.Plus,

    PTrans(..),
    papp,
    papp', -- :: PTrans a -> (a -> Maybe a)
    pact,
    palways, -- :: (a -> a) -> PTrans a
    pnever, -- :: PTrans a
    pwhen, -- :: (a -> Bool) -> PTrans a
    peither, -- :: PTrans a -> PTrans a -> PTrans a
    
    
    -- * Internal
    Trans,
    tapp,
    runTrans,

    TList,
    tlist,
    tlapp,
    tlappWhen,
    runTList,
    
    -- Tree,
    -- MTree,
    -- TMTree,
) 
where

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
import qualified Control.Category as Category
import Control.Monad.Writer hiding ((<>))
import Data.String
import Data.Semigroup
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import qualified Diagrams.Core.Transform as D



{- $basics

   A score is a list- or tree like structure, and has a 'MonadPlus' instance. This gives
   us composition using '<>' or 'mplus', as well as nesting using '>>=' and 'return',
   We also get /filtering/ using 'mmapMaybe' and 'mpartition' etc.
   
   We /transform/ a score by adding /transformation monoids/. These are 'Monoid' values
   which have an 'Action' into the /note type/ of the score. For example, time transformations
   act on time and duration, pitch transformations act on pitch etc.
   
   TODO Main challenge is how to combine 'MonadPlus' etc with filtering and `ordinary` 
   transformations such as fmap.
   
-}




-- | Partial transformation semigroup.

newtype PTrans a = PTrans { getPTrans :: (Partial a a) }

-- TODO move to monadplus
backtrack f x = fromMaybe x (f x)

papp :: PTrans a -> (a -> a)
papp = backtrack . papp'

papp' :: PTrans a -> (a -> Maybe a)
papp' = getPartial . getPTrans

pact :: (Action m a) => m -> PTrans a
pact = palways . act

palways :: (a -> a) -> PTrans a
palways = PTrans . Partial . always

pnever :: PTrans a
pnever = PTrans . Partial $ never

pwhen :: (a -> Bool) -> PTrans a
pwhen = PTrans . Partial . partial

peither :: PTrans a -> PTrans a -> PTrans a
peither = inPTrans2 mplus

inPTrans  = getPTrans ~> PTrans
inPTrans2 = getPTrans ~> inPTrans

instance Semigroup (PTrans a) where
    (<>) = inPTrans2 (Category..)
instance Monoid (PTrans a) where
    mempty  = PTrans $ Category.id
    mappend = inPTrans2 (Category..)


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

    Write (x1,m1) >> y = let
        Write (x2,m2) = y
        in Write (x2,m1 `mappend` m2)

    
-}

{-
Same thing with orinary pairs:

deriving instance Monoid m => Foldable ((,) m)
deriving instance Monoid m => Traversable ((,) m)
instance Monoid m => Monad ((,) m) where
    return x =  (mempty, x)
    (m1,x1) >>= f = let
        (m2,x2) = f x1
        in (m1 `mappend` m2,x2)
-}


{-
newtype LTrans m a = LTrans { getLTrans :: Trans m [a] }
    deriving (Semigroup, Monoid, Functor, Foldable, Traversable)

Trans m [a]
Trans m [Trans m [a]]            -- fmap (fmap f)
Trans m (Trans m [[a]])          -- fmap sequence
Trans m (Trans m [a])            -- fmap (fmap join)
Trans m [a]                      -- join

This is NOT equivalent to TList, it will compose transformations over the
*entire* list using monadic sequencing rather than *propagating* the traversion over the list 

Compare:

> runTransWith (fmap.appEndo) $ T.sequence $ fmap (tapp (e (+1)) . Trans . return) [1,2,3]

> fmap (runTransWith appEndo) $ T.sequence $ (tapp (e (+1)) . Trans . return) [1,2,3]
-}

{-
    TODO
    Assure consistency of act with fmap
    
    We need something like:
        
    > tlapp m . fmap g = fmap (act m . g)
    > fmap g . tlapp m = fmap (g . act m)

    
    Can this be derived from the Action laws (satisfied for Endo).
    
    > act mempty = id
    > act (m1 `mappend` m2) = act m1 . act m2

    Try
    > fmap g . tlapp m = fmap (g . act m)
    -- tlapp law
    > fmap g . fmap (tapp m) = fmap (g . act m)
    -- definition
    > fmap (g . tapp m) = fmap (g . act m)
    -- functor law

    Need to prove
    > fmap (g . tapp m)      = fmap (g . act m)
    -- theorem
    > fmap (g . (tell m >>)) = fmap (g . act m)
    -- definition
    > fmap (g . (Write ((), m) >>)) = fmap (g . act m)
    -- definition
    > fmap (g . (\x -> Write ((), m) >> x)) = fmap (g . act m)
    -- expand section
    > fmap (g . (\Write (x2,m2) -> Write ((), m) >> Write (x2,m2))) = fmap (g . act m)
    -- expand
    > fmap (g . (\Write (x2,m2) -> Write (x2,m <> m2))) = fmap (g . act m)
    -- expand
    > \Write (x2,m2) -> Write (x2,m <> m2))) = act m
    -- simplify
    

    
    

    > act mempty = id
    > act (m1 `mappend` m2) = act m1 . act m2

    Try
    > fmap g . tlapp m = fmap (g . act m)
    -- tlapp law
    > fmap g . fmap (ar . tapp m) = fmap (g . act m)
    -- definition
    > fmap (g . ar . tapp m) = fmap (g . act m)
    -- functor law

    Need to prove
    > fmap (g . ar . tapp m)      = fmap (g . act m)
    -- theorem
    > fmap (g . ar . (tell m >>)) = fmap (g . act m)
    -- definition
    > fmap (g . ar . (Write ((), m) >>)) = fmap (g . act m)
    -- definition
    > fmap (g . ar . (\x -> Write ((), m) >> x)) = fmap (g . act m)
    -- expand section
    > fmap (g . ar . (\Write (x2,m2) -> Write ((), m) >> Write (x2,m2))) = fmap (g . act m)
    -- expand
    > fmap (g . ar . (\Write (x2,m2) -> Write (x2,m <> m2))) = fmap (g . act m)
    -- expand
    > ar . \Write (x2,m2) -> Write (x2,m <> m2) = act m
    -- simplify
    > return . uncurry (flip act) . runWriter . \Write (x2,m2) -> Write (x2,m <> m2) = act m
    -- simplify
    > return . uncurry (flip act) . \(x2,m2) -> (x2,m <> m2))) = act m
    -- removes Write wrapper
    > (\x -> (x,mempty)) . uncurry (flip act) . \(x2,m2) -> (x2,m <> m2) = act m

    > (\x -> (uncurry (flip act) x,mempty)) . \(x2,m2) -> (x2,m <> m2) = act m


    > (\(x,m2) -> (act m2 x, mempty)) . fmap (m <>) = act m
    
    > (\(x,m2) -> (act (m <> m2) x, mempty)) = act m



    ar = return . uncurry (flip act) . runWriter

    tell w = Write ((), w)
    Write (_,m1) >> Write (x2,m2) = Write (x2,m1 `mappend` m2)

-}

-- |
-- Transformable list.
--
-- > type TList m a = [Trans m a]
--
-- See Jones and Duponcheel, /Composing Monads/ (1993).
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
    TList xs >>= f = TList $ join . fmap (fmap join . T.sequence . fmap (getTList . f)) $ xs
instance Monoid m => MonadPlus (TList m) where
    mzero = mempty
    mplus = (<>)
type instance Key (TList m) = m
instance Keyed (TList m) where
    mapWithKey f (TList xs) = TList $ fmap (mapWithKey f) xs

-- [Trans m a]
-- [Trans m [Trans m a]]            -- fmap (fmap f)
-- [[Trans m (Trans m a)]]          -- fmap sequence
-- [[Trans m a]]                    -- fmap (fmap join)
-- [Trans m a]                      -- join

runTrans :: Action m a => Trans m a -> a
runTrans = runTransWith act

runTransWith :: (m -> a -> a) -> Trans m a -> a
runTransWith f = uncurry (flip f) . renderTrans

assureRun :: (Monoid m, Action m a) => Trans m a -> Trans m a
assureRun = return . runTrans

renderTrans :: Trans m a -> (a, m)
renderTrans (Trans x) = runWriter x

-- |
-- > tapp m = fmap (act m)
tapp :: (Action m a, Monoid m) => m -> Trans m a -> Trans m a
tapp m = assureRun . tapp' m

tapp' :: Monoid m => m -> Trans m a -> Trans m a
tapp' m = (tell m >>)


-- | Construct a 'TList' from a transformation and a list.
tlist :: (Action m a, Monoid m)  => m -> [a] -> TList m a
tlist m xs = tlapp m (fromList xs)

-- | Construct a 'TList' from a list.
fromList :: Monoid m => [a] -> TList m a
fromList = mfromList

-- | Transform a list.
--
-- > tlapp m = fmap (act m)
--
-- TODO does it follow:
--
-- > tlapp (f <> g) = tlapp f . tlapp g
--
tlapp :: (Action m a, Monoid m)  => m -> TList m a -> TList m a
tlapp m (TList xs) = TList $ fmap (tapp m) xs


-- FIXME violates which law?
-- | Transform a list if the predicate holds.
--
tlappWhen :: (Action m a, Monoid m)  => (a -> Bool) -> m -> TList m a -> TList m a
tlappWhen p f xs = let (ts, fs) = mpartition p xs
    in tlapp f ts `mplus` fs


-- | Extract the components.
runTList :: Action m a => TList m a -> [a]
runTList (TList xs) = fmap runTrans xs

-- | Extract the components using the supplied function to render the cached transformations.
runTListWith :: (m -> a -> a) -> TList m a -> [a]
runTListWith f (TList xs) = fmap (runTransWith f) xs
                               
-- FIXME not what you'd expect!
getTListMonoid :: Monoid m => TList m a -> m
getTListMonoid = mconcat . runTListWith (const) . fmap (const undefined)

-- | Extract the components and cached transformations.
renderTList :: TList m a -> [(a, m)]
renderTList (TList xs) = fmap renderTrans xs



data Tree a = Tip a | Bin (Tree a) (Tree a)
    deriving (Functor, Foldable, Traversable)
instance Semigroup (Tree a) where
    (<>) = Bin       
-- TODO Default
instance Monoid a => Monoid (Tree a) where
    mempty  = Tip mempty
    mappend = (<>)
instance Monad Tree where
    return = Tip
    Tip x   >>= f = f x
    Bin x y >>= f = Bin (x >>= f) (y >>= f)

newtype TTree m a = TTree { getTTree :: Tree (Trans m a) }
    deriving (Semigroup, Monoid, Functor, Foldable, Traversable)
instance (IsString a, Monoid m) => IsString (TTree m a) where
    fromString = return . fromString
instance Monoid m => Applicative (TTree m) where
    pure = return
    (<*>) = ap
instance Monoid m => Monad (TTree m) where
    return = TTree . return . return
    TTree xs >>= f = TTree $ join . fmap (fmap join . T.mapM (getTTree . f)) $ xs

newtype MTree a = MTree { getMTree :: (Maybe (Tree a)) }
    deriving (Semigroup, Functor, Foldable, Traversable)
instance Monoid (MTree a) where
    mempty  = MTree Nothing
    mappend = (<>)
instance Monad MTree where
    return = MTree . return . return
    MTree xs >>= f = MTree $ join . fmap (fmap join . T.mapM (getMTree . f)) $ xs
instance MonadPlus MTree where
    mzero = mempty
    mplus = (<>)             

newtype TMTree m a = TMTree { getTMTree :: MTree (Trans m a) }
    deriving (Semigroup, Monoid, Functor, Foldable, Traversable)
instance (IsString a, Monoid m) => IsString (TMTree m a) where
    fromString = return . fromString
instance Monoid m => Applicative (TMTree m) where
    pure = return
    (<*>) = ap
instance Monoid m => Monad (TMTree m) where
    return = TMTree . return . return
    TMTree xs >>= f = TMTree $ join . fmap (fmap join . T.mapM (getTMTree . f)) $ xs
instance Monoid m => MonadPlus (TMTree m) where
    mzero = mempty
    mplus = (<>)




e = Endo
appE = tlapp . e
appWhenE p = tlappWhen p . e

default (Integer)

test :: [Integer]
test = runTList $ 
    appWhenE (/= (0::Integer)) (*(10::Integer)) $ (return (-1)) <> return 2


main = print test



swap (x,y) = (y,x)

-- FIXME move
instance Action (Endo a) a where
    act = appEndo


(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
(i ~> o) f = o . f . i
