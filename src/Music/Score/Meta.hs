
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Combinators for manipulating scores and related structures.
--
-- /Warning/ This module is experimental.
--
-------------------------------------------------------------------------------------

module Music.Score.Meta (
        module Music.Time.Meta,

        -- TODO move
        (</>),
        rcat,

        -- * Meta-events
        addMetaNote,
        addGlobalMetaNote,
        fromMetaReactive,

        metaAt,
        metaAtStart,
        withMeta,
        withGlobalMeta,
        withMetaAtStart,
        withGlobalMetaAtStart,
   ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens           hiding (parts, perform)
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Foldable          (Foldable (..))
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import           Data.Semigroup
import           Data.String
import           Data.Traversable
import           Data.VectorSpace

import           Music.Score.Part
import           Music.Score.Internal.Util
import           Music.Time
import           Music.Time.Meta
import           Music.Time.Reactive

import qualified Data.Foldable          as Foldable
import qualified Data.List              as List

infixr 6 </>


-- |
-- Concatenate parts.
--
rcat :: (HasParts' a, Enum (Part a)) => [Score a] -> Score a
rcat = List.foldr (</>) mempty

-- |
-- Similar to '<>', but increases parts in the second part to prevent collision.
--
(</>) :: (HasParts' a, Enum (Part a)) => Score a -> Score a -> Score a
a </> b = a <> moveParts offset b
    where
        -- max voice in a + 1
        offset = succ $ maximum' 0 $ fmap fromEnum $ toListOf parts a

        -- |
        -- Move down one voice (all parts).
        --
        moveParts :: (Integral b, HasParts' a, Enum (Part a)) => b -> Score a -> Score a
        moveParts x = parts %~ (successor x)

        -- |
        -- Move top-part to the specific voice (other parts follow).
        --
        moveToPart :: (Enum b, HasParts' a, Enum (Part a)) => b -> Score a -> Score a
        moveToPart v = moveParts (fromEnum v)


        iterating :: (a -> a) -> (a -> a) -> Int -> a -> a
        iterating f g n
            | n <  0 = f . iterating f g (n + 1)
            | n == 0 = id
            | n >  0 = g . iterating f g (n - 1)

        successor :: (Integral b, Enum a) => b -> a -> a
        successor n = iterating pred succ (fromIntegral n)

        maximum' :: (Ord a, Foldable t) => a -> t a -> a
        maximum' z = option z getMax . foldMap (Option . Just . Max)



addMetaNote :: forall a b . (IsAttribute a, HasMeta b{-, HasPart' b-}) => Note a -> b -> b
addMetaNote x y = (applyMeta $ toMeta (Just y) $ noteToReactive x) y

addGlobalMetaNote :: forall a b . (IsAttribute a, HasMeta b) => Note a -> b -> b
addGlobalMetaNote x = applyMeta $ toMeta (Nothing::Maybe Int) $ noteToReactive x

fromMetaReactive :: forall a b . ({-HasPart' a, -}IsAttribute b) => Maybe a -> Meta -> Reactive b
fromMetaReactive part = fromMaybe mempty . fromMeta part



withSpan :: Score a -> Score (Span, a)
withSpan = mapEvents (\t d x -> (t >-> d,x))
withTime = mapEvents (\t d x -> (t, x))

inSpan t' (view range -> (t,u)) = t <= t' && t' < u

-- TODO clean
mapBefore :: Time -> (Score a -> Score a) -> Score a -> Score a
mapDuring :: Span -> (Score a -> Score a) -> Score a -> Score a
mapAfter :: Time -> (Score a -> Score a) -> Score a -> Score a
mapBefore t f x = let (y,n) = (fmap snd *** fmap snd) $ mpartition (\(t2,x) -> t2 < t) (withTime x) in (f y <> n)
mapDuring s f x = let (y,n) = (fmap snd *** fmap snd) $ mpartition (\(t,x) -> t `inSpan` s) (withTime x) in (f y <> n)
mapAfter t f x = let (y,n) = (fmap snd *** fmap snd) $ mpartition (\(t2,x) -> t2 >= t) (withTime x) in (f y <> n)


-- Transform the score with the current value of some meta-information
-- Each "update chunk" of the meta-info is processed separately

runScoreMeta :: forall a b . ({-HasPart' a, -}IsAttribute b) => Score a -> Reactive b
runScoreMeta = fromMetaReactive (Nothing :: Maybe a) . (view meta)

metaAt :: ({-HasPart' a, -}IsAttribute b) => Time -> Score a -> b
metaAt x = (`atTime` x) . runScoreMeta

metaAtStart :: ({-HasPart' a, -}IsAttribute b) => Score a -> b
metaAtStart x = _onset x `metaAt` x

withGlobalMeta :: IsAttribute a => (a -> Score b -> Score b) -> Score b -> Score b
withGlobalMeta = withMeta' (Nothing :: Maybe Int)

withMeta :: (IsAttribute a{-, HasPart' b-}) => (a -> Score b -> Score b) -> Score b -> Score b
withMeta f x = withMeta' (Just x) f x

withMeta' :: ({-HasPart' c, -}IsAttribute a) => Maybe c -> (a -> Score b -> Score b) -> Score b -> Score b
withMeta' part f x = let
    m = (view meta) x
    r = fromMetaReactive part m
    in case splitReactive r of
        Left  a -> f a x
        Right ((a, t), bs, (u, c)) ->
            (meta .~) m
                $ mapBefore t (f a)
                $ (composed $ fmap (\(view (from note) -> (s, a)) -> mapDuring s $ f a) $ bs)
                $ mapAfter u (f c)
                $ x

withGlobalMetaAtStart :: IsAttribute a => (a -> Score b -> Score b) -> Score b -> Score b
withGlobalMetaAtStart = withMetaAtStart' (Nothing :: Maybe Int)

withMetaAtStart :: (IsAttribute a{-, HasPart' b-}) => (a -> Score b -> Score b) -> Score b -> Score b
withMetaAtStart f x = withMetaAtStart' (Just x) f x

withMetaAtStart' :: (IsAttribute b{-, HasPart' p-}) =>
    Maybe p -> (b -> Score a -> Score a) -> Score a -> Score a
withMetaAtStart' partId f x = let
    m = (view meta) x
    in f (fromMetaReactive partId m `atTime` _onset x) x



    -- JUNK
-- TODO move
noteToReactive :: Monoid a => Note a -> Reactive a
noteToReactive n = (pure <$> n) `activate` pure mempty

activate :: Note (Reactive a) -> Reactive a -> Reactive a
activate (view (from note) -> (view range -> (start,stop), x)) y = y `turnOn` (x `turnOff` y)
    where
        turnOn  = switchR start
        turnOff = switchR stop


