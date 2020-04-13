
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts #-}

module Main where

import Control.Lens (_1, _2)
import Music.Prelude
import qualified Data.List
import qualified Data.Ord
import Util

{-
A slight variation on the famous RCS quartet movement.

Medium (MC-sized!) orchestra. Treat instruments *individually*.
(2(2=afl/picc).2(2=ca).2(2=baskl).2(=cbsn) 4.2(=flgh).2(2 IS basstbn).0, 1perc(vib) 1timp 8+7 violins, 6 violas, 6 cellos, 4 basses).

8+8+2+(8+7+6+5+4) = 48


48 individual parts.

A)
  Each plays just a *single* pitch in various articulations.
B)
  Everybody plays 2-3 pitches in various articulations.


C) Possibly the best!
  Fixed set of 48 pitches! *Always* played tutti in different permutations!

-}
main = defaultMain music
music = text "Hello!" c

players =
    [flutes1, flutes2, oboes1,oboes2,clarinets1,clarinets2,bassoons1,bassoons2]
    <> divide 4 horns <> divide 2 trumpets <> divide 2 trombones
    <> [tutti vibraphone] <> [tutti timpani]
    <> divide 8 violins1
    <> divide 7 violins2
    <> divide 6 violas
    <> divide 5 cellos
    <> divide 4 doubleBasses




playersByHighest = Data.List.sortBy (Data.Ord.comparing $ \p -> (playableRange (p^.instrument))^.from ambitus._2) players
playersByLowest  = Data.List.sortBy (Data.Ord.comparing $ \p -> (playableRange (p^.instrument))^.from ambitus._1) players

-- Some example "chords"
-
unison1 = ppar $ zipWith (set parts') players (repeat c)
unison2 = ppar $ zipWith (set parts') players (fmap fromPitch $ cycle $ enumChromaticFromTo c c')
unison3 = ppar $ zipWith (set parts') playersByLowest (fmap fromPitch $ cycle $ enumChromaticFromTo c c')
unison4 = ppar $ zipWith (set parts') playersByHighest (fmap fromPitch $ cycle $ enumChromaticFromTo c c')

unison2a = ppar $ zipWith (set parts') players (fmap fromPitch $ cycle $ enumDiatonicFromTo c c')
unison3a = ppar $ zipWith (set parts') playersByLowest (fmap fromPitch $ cycle $ enumDiatonicFromTo c c')
unison4a = ppar $ zipWith (set parts') playersByHighest (fmap fromPitch $ cycle $ enumDiatonicFromTo c c')

unison2b = ppar $ zipWith (set parts') (reverse players) (fmap fromPitch $ cycle $ enumDiatonicFromTo c c')
unison3b = ppar $ zipWith (set parts') (reverse playersByLowest) (fmap fromPitch $ cycle $ enumDiatonicFromTo c c')
unison4b = ppar $ zipWith (set parts') (reverse playersByHighest) (fmap fromPitch $ cycle $ enumDiatonicFromTo c c')

-----
{-
TODO order by bottom range
Most obvious: arrange a diatonic/chromatic 48-note chord for 48 instruments.

Naive approach: generate all possibilities (fact 48) and filter out all arrangements
that has a pitch out of range. Not good as we would need to generate a list of length (fact 48)!
As we are taking range into account anyway, let's find a heuristic way of doing it!

The full arrangement assigns each pitch of the chord to an instrument. Each such assignment is
a pitch-instrument pair. Given an instrument, we can find a pitch inside the chord that matches its
ambitus. As there are possibly many such matching pitches, let's try a logic monad! We also need
state to keep track of the pitches already allocated!
-}










pl "fl1" = flutes1; pl "fl2" = flutes2; pl "ob1" = oboes1; pl "ob2" = oboes2; pl "cl1" = clarinets1; pl "cl2" = clarinets2; pl "bs1" = bassoons1; pl "bs2" = bassoons2
pl "hn1" = divide 4 horns !! 0; pl "hn2" = divide 4 horns !! 1; pl "hn3" = divide 4 horns !! 2; pl "hn4" = divide 4 horns !! 3
pl "tp1" = trumpets1; pl "tp2" = trumpets2; pl "tb1" = trombones1; pl "tb2" = trombones2
pl "vib" = tutti vibraphone
pl "ti"  = tutti timpani
pl "v1.1" = violins1; pl "v1.2" = violins1; pl "v1.3" = violins1; pl "v1.4" = violins1; pl "v1.5" = violins1; pl "v1.6" = violins1; pl "v1.7" = violins1; pl "v1.8" = violins1
pl "v2.1" = violins2; pl "v2.2" = violins2; pl "v2.3" = violins2; pl "v2.4" = violins2; pl "v2.5" = violins2; pl "v2.6" = violins2; pl "v2.7" = violins2; pl "v2.8" = violins2
pl "vla.1" = violas; pl "vla.2" = violas; pl "vla.3" = violas; pl "vla.4" = violas; pl "vla.5" = violas; pl "vla.6" = violas; pl "vla.7" = violas; pl "vla.8" = violas
pl "vc.1" = cellos; pl "vc.2" = cellos; pl "vc.3" = cellos; pl "vc.4" = cellos; pl "vc.5" = cellos; pl "vc.6" = cellos; pl "vc.7" = cellos; pl "vc.8" = cellos
pl "db.1" = doubleBasses; pl "db.2" = doubleBasses; pl "db.3" = doubleBasses; pl "db.4" = doubleBasses; pl "db.5" = doubleBasses; pl "db.6" = doubleBasses; pl "db.7" = doubleBasses; pl "db.8" = doubleBasses

[bassoons1,bassoons2] = divide 2 bassoons
