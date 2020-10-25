{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Score.Meta
  ( module Music.Time.Meta,

    -- * Meta-events
    addMetaNote,
    fromMetaReactive,
    -- metaAt,
    metaAtStart,
    withMeta,
  )
where

import Control.Lens hiding (parts)
import Control.Monad.Plus
import Data.Maybe
import Music.Score.Internal.Util (composed)
import Music.Time
import Music.Time.Meta

addMetaNote :: forall a b. (AttributeClass a, HasMeta b) => Event a -> b -> b
addMetaNote x = applyMeta $ wrapTMeta $ noteToReactive x

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
          (meta .~) m
            $ mapBefore t (f a)
            $ (composed $ fmap (\(view (from event) -> (s, a)) -> mapDuring s $ f a) $ bs)
            $ mapAfter u (f c)
            $ x

-- TODO move
withTime :: Score a -> Score (Time, a)
withTime = mapWithSpan (\s x -> (s ^. onset, x))

-- TODO move
inSpan :: Time -> Span -> Bool
inSpan t' (view onsetAndOffset -> (t, u)) = t <= t' && t' < u

-- TODO move
mapBefore :: Time -> (Score a -> Score a) -> Score a -> Score a
mapBefore t f x = let (y, n) = (fmap snd `bimap` fmap snd) $ mpartition (\(t2, x) -> t2 < t) (withTime x) in (f y <> n)

-- TODO move
mapDuring :: Span -> (Score a -> Score a) -> Score a -> Score a
mapDuring s f x = let (y, n) = (fmap snd `bimap` fmap snd) $ mpartition (\(t, x) -> t `inSpan` s) (withTime x) in (f y <> n)

-- TODO move
mapAfter :: Time -> (Score a -> Score a) -> Score a -> Score a
mapAfter t f x = let (y, n) = (fmap snd `bimap` fmap snd) $ mpartition (\(t2, x) -> t2 >= t) (withTime x) in (f y <> n)

-- Transform the score with the current value of some meta-information
-- Each "update chunk" of the meta-info is processed separately

runScoreMeta :: forall a b. (HasMeta a, AttributeClass b) => a -> Reactive b
runScoreMeta = fromMetaReactive . (view meta)

-- TODO move
noteToReactive :: Monoid a => Event a -> Reactive a
noteToReactive n = (pure <$> n) `activate` pure mempty

-- TODO move
activate :: Event (Reactive a) -> Reactive a -> Reactive a
activate (view (from event) -> (view onsetAndOffset -> (start, stop), x)) y = y `turnOn` (x `turnOff` y)
  where
    turnOn = switchR start
    turnOff = switchR stop
