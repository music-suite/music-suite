
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, ConstraintKinds, FlexibleContexts, FlexibleInstances #-}

module Main where

import Music.Prelude
import Data.Maybe
import qualified Music.Score.Pitch
import Control.Lens (each, _head, _last, nullOf)
import qualified Data.List

{-
Given a melody, decorate it by artificially sustaining certain notes.


NOTE This (trivial, unclean) example could become the basis for a whole piece.
I.e. start out with a Cantus Firmus, sustain certain pitches, these become chords/sounds to which melodic motion is added
and so on.
-}

subj :: Voice StandardNote
subj  = compress 16 $ simplifyPitches $ (!! 3) $ stitchTogether $ ppar $ concat $ replicate 5 [c,g,bb,g,cs,b_]
-- subj2 = compress 16 $ simplifyPitches $ (!! 3) $ stitchTogether $ ppar $ concat $ replicate 5 [c,e,f,e,c,cs]
subj2 = compress 16 $ simplifyPitches $ (!! 3) $ stitchTogether $ ppar $ concat $ replicate 5 [c,e,f,e,c,ds]

otherNotes :: Voice a -> Score a
otherNotes v = ppar $ zipWith (\t n -> delay t (pure n)) ts (fmap (fromMaybe (error "Outside voice") . voiceAtDuration v) ts)
  where
    -- Times to sample
    ts = [1,5.25,7,9.75,15,16.5,18,19]

noteToScore n = [renderAlignedNote $ aligned 0 0 n]^.score

music :: Music
music =
  (set parts' (tutti vibraphone) $ level ff $ octavesAbove 1 $ _8va $ over (events.each) (stretchRelativeOnset 1) (otherNotes subj))
    <>
  (set parts' violins1 $ renderAlignedVoice (aligned 0 0 subj))
    <>
  (set parts' (tutti marimba) $ level ff $ octavesAbove 1 $ _8va $ over (events.each) (stretchRelativeOnset 1) (otherNotes subj2))
    <>
  (set parts' violins2 $ renderAlignedVoice (aligned 0 0 subj2))
    <>
  delay 10 midSect1

midSect1 :: Music
midSect1 =
  (set parts' (tutti glockenspiel) $ level ff $ octavesAbove 0 $ _15va $ over (events.each) (stretchRelativeOnset 1) (otherNotes subj2))
    <>
  (set parts' violas $ renderAlignedVoice (aligned 0 0 subj2))



octavesAbove n x = x <> octavesUp n x
octavesBelow n x = x <> octavesDown n x

{-
>>> :da over (events.each) (stretchRelativeOnset 1) (otherNotes subj) <<> (renderAlignedVoice (aligned 0 0 subj))

-}
-- FIXME unsafe
-- Switchpoint belongs to latter value
voiceAtDuration :: Voice a -> Alignment -> Maybe a
voiceAtDuration v = fmap getLast . getOption . voiceAtDuration' (fmap (Option . Just . Last) v)

voiceAtDuration' :: Monoid a => Voice a -> Alignment -> a
voiceAtDuration' v d = voiceToBehavior (aligned o a v) ! (o .+^ d)
  where
    o = 0 -- Does not matter
    a = 0 -- Align to onset

-- Turn a voice into a behavior by aligning.
-- Value at switchpoint is determined by the monoid (see voiceAtDurationFirst etc).
voiceToBehavior :: Monoid a => Aligned (Voice a) -> Behavior a
voiceToBehavior = scoreToBehavior . renderAlignedVoice


scoreToBehavior :: Monoid a => Score a -> Behavior a
scoreToBehavior = concatB . fmap pure

stitchTogether :: (HasPitches' a, Transposable a) => Voice a -> [Voice a]
stitchTogether = Data.List.unfoldr (\v -> let v2 = stitch v v in Just (v2,v2))

{-
  Note: stitch should really have the constraint (HasPitch' a, Transposable a) and be implemented using ^.pitch instead of ^?!pitches
  Not practical as long as [] is in StandardNote!
-}
-- Join two voices together so that one note overlaps. The second voice is transposed to achieve this.
-- Duration is taken from the first voice.
stitch :: (HasPitches' a, Transposable a) => Voice a -> Voice a -> Voice a
stitch = stitchWith (\a b -> [a]^.voice)

-- Join two voices together so that one note overlaps. The second voice is transposed to achieve this.
-- Duration is taken from the second voice.
stitchLast :: (HasPitches' a, Transposable a) => Voice a -> Voice a -> Voice a
stitchLast = stitchWith (\a b -> [b]^.voice)

stitchWith :: forall a . (HasPitches' a, Transposable a) => (Note a -> Note a -> Voice a) -> Voice a -> Voice a -> Voice a
stitchWith f a b
  | nullOf notes a = b
  | nullOf notes b = a
  -- | otherwise      = a <> up diff b
  | otherwise      = initV a <> f (lastV a) (headV (up diff b)) <> tailV (up diff b)
  where
    headV = (^?!notes._head)
    lastV = (^?!notes._last)
    initV = over notes init
    tailV = over notes tail
    lastPitch a = lastV a^?!pitches
    headPitch b = headV b^?!pitches
    diff = (lastPitch a .-. headPitch b)




main = defaultMain music
