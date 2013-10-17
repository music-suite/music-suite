
{-# LANGUAGE     
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    NoMonomorphismRestriction,
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    TypeFamilies,
    ViewPatterns,
    RankNTypes,

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
import Data.Maybe
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

writeFst (Write x) = fst x
writeSnd (Write x) = snd x
written f (Write (a, m)) = Write (a, f m)


{-
    The Key type family associates "indexed" types with their index.
    Instances include container-like types, functions and their composition.

    A /focused/ value a value of such a type paired up with its index. This
    is similar to a zipper, or a data structure paired with a lens into the
    structure.
-}

-- TODO move
mcompose :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mcompose = (join .) . fmap . (fmap join .) . T.mapM


-- | Value with a focus.
newtype Focus' k f a = Focus' { unFocus' :: Write k (f a) }
    deriving (Functor, Foldable, Traversable, Eq, Show)
inFocus' = unFocus' ~> Focus'
instance (Monoid k, k ~ Key f, Monad f, Traversable f) => Monad (Focus' k f) where
    return = Focus' . return . return
    Focus' xs >>= f = Focus' $ mcompose (unFocus' . f) $ xs

type Focus f a = Focus' (Key f) f a


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









-- | Value with a associated spans.
--   
--   Spans allow potentially overlapping subranges to be annotated with arbitrary
--   monoidal values.
--   
newtype Spanned' k m f a = Spanned' { unSpanned' :: Write (Map (k,k) m) (f a) }
    deriving (Functor, Foldable, Traversable, Eq, Show)
inSpanned' = unSpanned' ~> Spanned'

instance (Ord k, k ~ Key f, Monad f, Traversable f) => Monad (Spanned' k m f) where
    return = Spanned' . return . return
    Spanned' xs >>= f = Spanned' $ mcompose (unSpanned' . f) $ xs

type Spanned m f a = Spanned' (Key f) m f a

withSpans f (Spanned' (Write (a, m))) = (Spanned' (Write (a, f m)))

-- addSpan :: (Ord k, Semigroup m) => k -> k -> m -> Spanned' k m f a -> Spanned' k m f a
addSpan :: Semigroup m => Int -> Int -> m -> SList m a -> SList m a
addSpan a b x = withSpans $ Map.insertWith (<>) (a,b) x

slist :: [a] -> SList m a
slist xs = (Spanned' (Write (xs, mempty)))

reverseS = withValuesS reverse
takeS n = withValuesS (take n)
dropS n = withValuesS (drop n)
-- cons x = withValuesS ((:) x)

{-
FIXME
consS x = unsafeWithValuesS (mapFirst (const x)) . withValuesS (undefined :)
    where mapFirst f (x:xs) = (f x):xs

unsafeWithValuesS :: ([a] -> [a]) -> SList m a -> SList m a
unsafeWithValuesS f (Spanned' (Write (a, m))) = (Spanned' (Write (f a, m)))
-}

-- FIXME rename
-- Transform the structure of a list (but not its values)
-- Retains all current spans
withValuesS :: (forall a . [a] -> [a]) -> SList m a -> SList m a
withValuesS f sl = res
    where
        spans = writeSnd . unSpanned' $ sl
        xs = writeFst . unSpanned' $ sl
        ks = fmap fst . keyed $ xs -- unique keys
        
        ks2 = f ks `Prelude.zip` ks -- map old keys to new keys
        
        kf = flip Prelude.lookup ks2 -- function that maps old keys to new ones
        spans2 = removeNilKeys . Map.mapKeys (mapBothM kf) $ spans
        res = Spanned' (Write (f xs,spans2))
        
ex1, ex2, ex3, ex4 :: SList String Int
ex1 = reverseS $ addSpan 0 1 "h" $ slist [1..10]
ex2 = takeS 5 $ addSpan 0 1 "h" $ slist [1..10]
ex3 = dropS 5 $ addSpan 0 1 "h" $ slist [1..10]
ex4 = undefined
-- ex4 = consS 33 $ addSpan 0 1 "h" $ slist [1..10]

{-
    Test:
    
        
-}

    
-- (f, sl) = undefined
-- f :: ([a] -> [a])
-- sl :: SList m a
-- 
-- -- [a]
-- spans = writeSnd . unSpanned' $ sl
-- 
-- xs = writeFst . unSpanned' $ sl
-- ks = fmap fst . keyed $ xs -- unique keys
-- ks2 = ks `Prelude.zip` f ks -- map old keys to new keys
-- kf = flip Prelude.lookup ks2 -- function that maps old keys to new ones
-- spans2 = removeNilKeys . Map.mapKeys (mapBothM kf) $ spans
-- res = Spanned' (Write (xs,spans2))

-- List with a focus
type FList a = Focus [] a

-- List with spans
type SList m a = Spanned m [] a

removeNilKeys :: Ord a => Map (Maybe a) b -> Map a b
removeNilKeys = Map.mapKeys fromJust . Map.filterWithKey (\k v -> isJust k)

mapBothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
mapBothM f (a,b) = do
    a2 <- f a
    b2 <- f b
    return (a2, b2)

-- Transform the structure of a list (but not its values)
-- retains all current ranges
-- ([a] -> [a]) -> SList a -> SList a




{-
-- TODO misleading name, keys does not need to be consecutive
newtype Range f a = Range { getRange :: [Focus f a] }
    deriving (Functor)
instance Lookup f => Foldable (Range f) where
    foldMap f (Range xs) = mconcat $ fmap f $ mcatMaybes $ fmap get xs

range :: Enum (Key f) => Key f -> Key f -> f a -> Range f a
range a b x = Range $ fmap (flip pick $ x) $ enumFromTo a b

focus1 :: Focus (Map String) String
focus1 = pick "name" $ Map.fromList [("name", "hans"), ("interest", "music")]
focus2 = pick 100 (+ 10)
range1 = range 30 60 (* 10)
-}





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
