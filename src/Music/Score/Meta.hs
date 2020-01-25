{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}

module Music.Score.Meta
  ( module Music.Time.Meta,

    -- * Meta-events
    addMetaNote,
    fromMetaReactive,
    -- metaAt,
    metaAtStart,
    withMeta,
    withMetaAtStart,
  )
where

import Control.Applicative
import Control.Lens hiding (parts)
import Control.Monad
import Control.Monad.Plus
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Bifunctor
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Maybe
import Data.Ord
import Data.Ratio
import Data.Semigroup
import Data.String
import Data.Traversable
import Data.VectorSpace
import Music.Score.Internal.Util
import Music.Score.Part
import Music.Time
import Music.Time.Meta
import Music.Time.Reactive

addMetaNote :: forall a b. (AttributeClass a, HasMeta b) => Event a -> b -> b
addMetaNote x = applyMeta $ wrapTMeta $ noteToReactive x

fromMetaReactive :: forall b. AttributeClass b => Meta -> Reactive b
fromMetaReactive = fromMaybe mempty . unwrapMeta

metaAt :: AttributeClass b => Time -> Score a -> b
metaAt x = (`atTime` x) . runScoreMeta

metaAtStart :: AttributeClass b => Score a -> b
metaAtStart x = (x ^. onset) `metaAt` x

withMeta :: AttributeClass a => (a -> Score b -> Score b) -> Score b -> Score b
withMeta f x =
  let m = (view meta) x
      r = fromMetaReactive m
   in case splitReactive r of
        Left a -> f a x
        Right ((a, t), bs, (u, c)) ->
          (meta .~) m
            $ mapBefore t (f a)
            $ (composed $ fmap (\(view (from event) -> (s, a)) -> mapDuring s $ f a) $ bs)
            $ mapAfter u (f c)
            $ x

withMetaAtStart :: AttributeClass a => (a -> Score b -> Score b) -> Score b -> Score b
withMetaAtStart f x =
  let m = view meta x
   in f (fromMetaReactive m `atTime` (x ^. onset)) x

-- withSpan :: Score a -> Score (Span, a)
-- withSpan = mapWithSpan (,)

withTime :: Score a -> Score (Time, a)
withTime = mapWithSpan (\s x -> (s ^. onset, x))

inSpan :: Time -> Span -> Bool
inSpan t' (view onsetAndOffset -> (t, u)) = t <= t' && t' < u

mapBefore :: Time -> (Score a -> Score a) -> Score a -> Score a
mapBefore t f x = let (y, n) = (fmap snd `bimap` fmap snd) $ mpartition (\(t2, x) -> t2 < t) (withTime x) in (f y <> n)

mapDuring :: Span -> (Score a -> Score a) -> Score a -> Score a
mapDuring s f x = let (y, n) = (fmap snd `bimap` fmap snd) $ mpartition (\(t, x) -> t `inSpan` s) (withTime x) in (f y <> n)

mapAfter :: Time -> (Score a -> Score a) -> Score a -> Score a
mapAfter t f x = let (y, n) = (fmap snd `bimap` fmap snd) $ mpartition (\(t2, x) -> t2 >= t) (withTime x) in (f y <> n)

-- Transform the score with the current value of some meta-information
-- Each "update chunk" of the meta-info is processed separately

runScoreMeta :: forall a b. AttributeClass b => Score a -> Reactive b
runScoreMeta = fromMetaReactive . (view meta)

noteToReactive :: Monoid a => Event a -> Reactive a
noteToReactive n = (pure <$> n) `activate` pure mempty

activate :: Event (Reactive a) -> Reactive a -> Reactive a
activate (view (from event) -> (view onsetAndOffset -> (start, stop), x)) y = y `turnOn` (x `turnOff` y)
  where
    turnOn = switchR start
    turnOff = switchR stop
