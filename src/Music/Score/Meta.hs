{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Music.Score.Meta
  ( module Music.Time.Meta,

    -- * Meta-events
    addMetaEvent,
    addMetaAt,
    fromMetaReactive,
    metaAtStart,
    withMeta,
  )
where

import Control.Lens hiding (parts)
import Control.Monad.Plus
import Data.Maybe
import Music.Score.Internal.Util (composed)
import Music.Time
import Music.Time.Impulses
import Music.Time.Meta
import qualified Data.Map

addMetaEvent :: forall a b. (AttributeClass a, HasMeta b) => Event a -> b -> b
addMetaEvent x = applyMeta $ wrapTMeta $ eventToReactive x

addMetaAt :: forall a b. (AttributeClass a, HasMeta b) => Time -> a -> b -> b
addMetaAt t x = applyMeta $ wrapTMeta $ timeToImpulses t x

fromMetaReactive :: forall b. AttributeClass b => Meta -> Reactive b
fromMetaReactive = fromMaybe mempty . unwrapMeta

metaAt :: (HasMeta a, AttributeClass b) => Time -> a -> b
metaAt x = (`atTime` x) . runScoreMeta

metaAtStart :: (HasMeta a, HasPosition a) => AttributeClass b => a -> b
metaAtStart x = case _era x of
  Nothing -> mempty
  Just e -> (e ^. onset) `metaAt` x

withMeta :: AttributeClass a => (a -> Score b -> Score b) -> Score b -> Score b
withMeta f x =
  let m = (view meta) x
      r = fromMetaReactive m
   in case splitReactive r of
        Left a -> f a x
        Right ((a, t), bs, (u, c)) ->
          (meta .~) m $
            mapBefore t (f a) $
              (composed $ fmap (\(view (from event) -> (s, a)) -> mapDuring s $ f a) $ bs) $
                mapAfter u (f c) $
                  x

withTime :: Score a -> Score (Time, a)
withTime = mapWithSpan (\s x -> (s ^. onset, x))

inSpan :: Time -> Span -> Bool
inSpan t' (view onsetAndOffset -> (t, u)) = t <= t' && t' < u

mapBefore :: Time -> (Score a -> Score a) -> Score a -> Score a
mapBefore t f x = let (y, n) = (fmap snd `bimap` fmap snd) $ mpartition (\(t2, _) -> t2 < t) (withTime x) in (f y <> n)

mapDuring :: Span -> (Score a -> Score a) -> Score a -> Score a
mapDuring s f x = let (y, n) = (fmap snd `bimap` fmap snd) $ mpartition (\(t, _) -> t `inSpan` s) (withTime x) in (f y <> n)

mapAfter :: Time -> (Score a -> Score a) -> Score a -> Score a
mapAfter t f x = let (y, n) = (fmap snd `bimap` fmap snd) $ mpartition (\(t2, _) -> t2 >= t) (withTime x) in (f y <> n)

runScoreMeta :: forall a b. (HasMeta a, AttributeClass b) => a -> Reactive b
runScoreMeta = fromMetaReactive . view meta

eventToReactive :: Monoid a => Event a -> Reactive a
eventToReactive n = (pure <$> n) `activateDuring` pure mempty

activateDuring :: Event (Reactive a) -> Reactive a -> Reactive a
activateDuring (view (from event) -> (view onsetAndOffset -> (start, stop), x)) y = y `turnOn` (x `turnOff` y)
  where
    turnOn = switchR start
    turnOff = switchR stop

-- |
-- >>> timeToImpulses 2 On `at` 1
-- Off
--
-- >>> timeToImpulses 2 On `at` 2
-- On
--
-- >>> timeToImpulses 2 On `at` 3
-- Off
-- |
-- >>> (timeToImpulses 2 On <> timeToImpulses 4 On) `at` 1
-- Off
--
-- >>> (timeToImpulses 2 On <> timeToImpulses 4 On) `at` 2
-- On
--
-- >>> (timeToImpulses 2 [1] <> timeToImpulses 2 [3]) `at` 2
-- [1,3]
--
timeToImpulses :: Time -> a -> Impulses a
timeToImpulses t n = Impulses $ Data.Map.singleton t n