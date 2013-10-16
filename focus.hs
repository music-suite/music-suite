
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
    
    OverloadedStrings,
    TypeOperators,
    FlexibleContexts,
    
    TemplateHaskell
    #-}


module Data.SpanList where

import Prelude hiding (span) -- TODO

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Plus

import Control.Lens
import Data.Key
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Foldable
import Data.Traversable
import qualified Data.Foldable as F
import qualified Data.Traversable as T

-- type family RelPos (a :: * -> *) :: *
-- type family Pos  (a :: * -> *) :: *
-- 
-- type instance RelPos [] = Maybe Pos -- Nothing means above
-- type instance Pos    [] = Integer
-- 
-- data TreeRelPos = Maybe TreePos -- Nothing means up
-- data TreePos    = Here | Up | Child Int TreePos
-- type instance RelPos Tree = Int
-- -- type instance Pos Tree = Int
-- 
-- 



newtype Write m a = Write (a, m)
    deriving (Show, Functor, Foldable, Traversable, Eq)
instance Monoid m => Monad (Write m) where
    return x = Write (x, mempty)
    Write (x1,m1) >>= f = let
        Write (x2,m2) = f x1
        in Write (x2,m1 `mappend` m2)

    Write (x1,m1) >> y = let
        Write (x2,m2) = y
        in Write (x2,m1 `mappend` m2)

written f (Write (a, m)) = Write (a, f m)


{-
    The Key type family associates "indexed" types with their index.
    Instances include container-like types, functions and some other common functors.

    A "focused" value is just a pair (or writer), where the associated monoid is assumed to 
    be the key.
-}

-- TODO move
mcompose :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mcompose = (join .) . fmap . (fmap join .) . T.mapM


-- | Value with a focus.
newtype Focus' k f a = Focus' { unFocus' :: (Write k (f a)) }
    deriving (Functor, Foldable, Traversable, Eq, Show)
inFocus' = unFocus' ~> Focus'
instance (Monoid k, k ~ Key f, Monad f, Traversable f) => Monad (Focus' k f) where
    return = Focus' . return . return
    Focus' xs >>= f = Focus' $ mcompose (unFocus' . f) $ xs

type Focus f a = Focus' (Key f) f a

newtype Select c a = Select ([Key c], c a)


-- TODO should be called focus
-- | Create a focus by picking an element.
pick :: Key f -> f a -> Focus f a
pick n xs = Focus' $ Write (xs, n)

unFocus :: Focus f a -> f a
unFocus (Focus' (Write (x,_))) = x

-- | Move the focus.
step :: (Key f -> Key f) -> Focus f a -> Focus f a
step f = inFocus' (written f)

previous, next :: Enum (Key f) => Focus f a -> Focus f a
-- | Move the focus to the previous value.
previous = step pred
-- | Move the focus to the next value.
next = step succ

past, future :: Enum (Key f) => Focus f a -> [Focus f a]
future = iterate next
past   = iterate previous

get :: Lookup f => Focus f a -> Maybe a
get x = Data.Key.lookup (getKey x) (unFocus x)

getKey :: Focus f a -> Key f
getKey (Focus' (Write (x,k))) = k

pastKeys, futureKeys :: Enum (Key f) => Focus f a -> [Key f]
futureKeys  = fmap getKey . future
pastKeys    = fmap getKey . past







example1 :: Focus (Map Int) String
example1 = pick 0 $ Map.fromList [(0, "hans"), (1, "music")]

example2 :: Focus ((->) Int) Int
example2 = pick 100 (+ 10)


newtype Range f a = Range { getRange :: [Focus f a] }
    deriving (Functor)
instance Lookup f => Foldable (Range f) where
    foldMap f (Range xs) = mconcat $ fmap f $ mcatMaybes $ fmap get xs

range :: Enum (Key f) => Key f -> Key f -> f a -> Range f a
range a b x = Range $ fmap (flip pick $ x) $ enumFromTo a b







range1 = range 30 60 (* 10)





{-
data Spanning p s a = Forward p s (Spanning p s a) | Backward p s (Spanning p s a) | NoSpan a
instance Semigroup (Spanning p s a) where
instance Monoid (Spanning p s a) where

-- reverseS :: [a] -> [a] -> Spanning Int s a -> Spanning Int s a

-- TODO safety
selected :: Int -> (a -> a) -> [a] -> [a]
selected n f []     = []
selected 0 f [x]    = [f x]
selected n f xs     = take n xs ++ [f (xs !! n)] ++ drop (n+1) xs

selectedRange :: Int -> Int -> (a -> a) -> [a] -> [a]
selectedRange = undefined


span :: Semigroup s => Int -> Int -> s -> [Spanning Int s a] -> [Spanning Int s a]
span a b s = selected a (\x -> Forward b s x)
-- left or right?

render :: [Spanning p s a] -> ([a], [(p, p, s)])
render = undefined


renderWith :: (s -> a -> a) -> [Spanning p s a] -> [a]
renderWith = undefined
-- TODO use selectedRange
-}

-- -- |
-- -- 
-- newtype SpanList m a = SpanList { getSpanList :: (Map (Int,Int) m, [a]) }
--     deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Show)
-- -- TODO semigroup
-- 
-- -- TODO cleaner       
-- withSpans f (SpanList (m, xs)) = SpanList (f m, xs)
-- travValues f (SpanList (m, xs)) = fmap (\x -> SpanList (m,x)) $ f xs
-- 
-- values :: Lens (SpanList m a) (SpanList m b) [a] [b]
-- values f = travValues f
-- 
-- 
-- 
-- 
-- span :: Semigroup m => Int -> Int -> m -> SpanList m a -> SpanList m a
-- span a b x = withSpans $ Map.insertWith (<>) (a,b) x
-- 
-- spans :: SpanList m a -> [((Int, Int), m)]
-- spans = Map.toList . fst . getSpanList
-- 
-- 
-- headS :: SpanList m a -> a   
-- tailS :: SpanList m a -> SpanList m a
-- nullS :: SpanList t a -> Bool
-- consS :: a -> SpanList m a -> SpanList m a
-- deconsS :: SpanList m a -> Maybe (a, SpanList m a)
-- 
-- headS (SpanList (m, xs)) = Prelude.head xs
-- 
-- tailS (SpanList (_, []))   = SpanList (mempty, [])
-- tailS (SpanList (m, x:xs)) = SpanList (dec m, xs)
-- 
-- nullS (SpanList (m, xs)) = Prelude.null xs 
-- 
-- consS x (SpanList (m,xs)) = SpanList (inc m, x:xs)
-- deconsS x | nullS x    = Nothing
--           | otherwise  = Just (headS x, tailS x)
-- 
-- 
-- fromList xs = SpanList (mempty,xs)
-- 
-- -- TODO
-- -- consS' :: m -> a -> SpanList m a -> SpanList m a
-- -- deconsS' :: SpanList m a -> Either m (a, SpanList m a)
-- 
-- 
-- reverseS :: SpanList m a -> SpanList m a
-- reverseS l =  rev l mempty
--   where                    
--     rev :: SpanList m a -> SpanList m a -> SpanList m a
--     rev (deconsS -> Nothing)     a = a
--     rev (deconsS -> Just (x,xs)) a = rev xs (x `consS` a)
-- 
-- 
-- -- type Score = SpanList Articulation (Time, Dynamic)
-- 
-- 
-- 
-- dec = Map.mapKeys (subtract 1 *** subtract 1)
-- inc = Map.mapKeys ((+ 1) *** (+ 1))    



(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
(i ~> o) f = o . f . i
