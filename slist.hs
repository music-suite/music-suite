
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


module Data.SpanList (
        -- Write,
        -- writeFst,
        -- writeSnd,
        -- written,
        -- mcompose,
        Spanned',
        Spanned,
        SList,
        -- withSpans,
        addSpan,
        slist,
        reverseS,
        takeS,
        dropS,
        duplicateS,
        -- withValuesS,
) where

import Prelude hiding (span) -- TODO

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Plus

import Control.Compose ((~>))
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


-- TODO move
mcompose :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mcompose = (join .) . fmap . (fmap join .) . T.mapM

-- | Value with a associated spans.
--   
--   Spans allow potentially overlapping subranges to be annotated with arbitrary
--   monoidal values.
--   
newtype Spanned' k m f a = Spanned' { getSpanned' :: Write (Map (k,k) m) (f a) }
    deriving (Functor, Foldable, Traversable, Eq, Show)
inSpanned' = getSpanned' ~> Spanned'

instance (Ord k, k ~ Key f, Monad f, Traversable f) => Applicative (Spanned' k m f) where
    pure = return
    (<*>) = ap
instance (Ord k, k ~ Key f, Monad f, Traversable f) => Monad (Spanned' k m f) where
    return = Spanned' . return . return
    Spanned' xs >>= f = Spanned' $ mcompose (getSpanned' . f) $ xs

-- Spanned m f a
--   m is a Monoid
--   f is a Traversable Monad
type Spanned m f a = Spanned' (Key f) m f a

-- List with spans
--   m is a Monoid
type SList m a = Spanned m [] a

-- getSpans :: Spanned m f a -> Map (Key f, Key f) m
getSpans :: SList m a -> Map (Int,Int) m
getSpans (Spanned' (Write (a, m))) = m

-- mapSpans :: (Map (Key f, Key f) m -> Map (Key f, Key f) m) -> Spanned m f a -> Spanned m f a
mapSpans :: (Map (Int,Int) m -> Map (Int,Int) m) -> SList m a -> SList m a
mapSpans f (Spanned' (Write (a, m))) = Spanned' (Write (a, f m))

addSpan :: Semigroup m => Int -> Int -> m -> SList m a -> SList m a
addSpan a b x = mapSpans $ Map.insertWith (<>) (a,b) x

slist :: [a] -> SList m a
slist = Spanned' . return

reverseS    :: SList m a -> SList m a
duplicateS  :: SList m a -> SList m a
takeS       :: Int -> SList m a -> SList m a
dropS       :: Int -> SList m a -> SList m a

reverseS    = withValuesS reverse
takeS n     = withValuesS (take n)
dropS n     = withValuesS (drop n)
duplicateS  = withValuesS (\xs -> xs <> xs)
-- cons x = withValuesS ((:) x)


-- FIXME rename

-- Transform the structure of a list (but not its values)
-- Retains all current spans under filtering or permutation (as long as the values are still there)
-- Note that the type guarantees that new values can not be added
-- TODO duplication etc is still problematic

withValuesS :: (forall a . [a] -> [a]) -> SList m a -> SList m a
withValuesS f sl = res
    where
        spans = writeSnd . getSpanned' $ sl
        xs = writeFst . getSpanned' $ sl
        ks = fmap fst . keyed $ xs -- unique keys
        
        ks2 = f ks `Prelude.zip` {-ks-}[0..] -- map old keys to new keys
        
        kf = flip Prelude.lookup ks2 -- function that maps old keys to new ones
        spans2 = removeNilKeys . Map.mapKeys (mapBothM kf) $ spans
        res = Spanned' (Write (f xs,spans2))
        
ex1, ex2, ex3, ex4 :: SList String Int
ex1 = reverseS $ addSpan 0 1 "h" $ slist [1..10]
ex2 = takeS 5 $ addSpan 0 1 "h" $ slist [1..10]
ex3 = dropS 5 $ addSpan 0 1 "h" $ slist [1..10]
ex4 = undefined
-- ex4 = consS 33 $ addSpan 0 1 "h" $ slist [1..10]
removeNilKeys :: Ord a => Map (Maybe a) b -> Map a b
removeNilKeys = Map.mapKeys fromJust . Map.filterWithKey (\k v -> isJust k)

mapBothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
mapBothM f (a,b) = do
    a2 <- f a
    b2 <- f b
    return (a2, b2)

