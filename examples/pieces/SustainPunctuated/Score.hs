
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts, FlexibleInstances #-}

module Main where

import Music.Prelude
import Data.Maybe
import Util

{-
Given a melody, decorate it by artificially sustaining certain notes.


NOTE This (trivial, unclean) example could become the basis for a whole piece.
I.e. start out with a Cantus Firmus, sustain certain pitches, these become chords/sounds to which melodic motion is added
and so on.
-}

subj :: Voice StandardNote
subj  = compress 16 $ simplifyPitches $ asVoice $ (!! 3) $ stitchTogether $ pcat $ concat $ replicate 5 [c,g,bb,g,cs,b_]
-- subj2 = compress 16 $ simplifyPitches $ asVoice $ (!! 3) $ stitchTogether $ pcat $ concat $ replicate 5 [c,e,f,e,c,cs]
subj2 = compress 16 $ simplifyPitches $ asVoice $ (!! 3) $ stitchTogether $ pcat $ concat $ replicate 5 [c,e,f,e,c,ds]

otherNotes :: Voice a -> Score a
otherNotes v = pcat $ zipWith (\t n -> delay t (pure n)) ts (fmap (fromMaybe (error "Outside voice") . voiceAtDuration v) ts)
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

 
  
  