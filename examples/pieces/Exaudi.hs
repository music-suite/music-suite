
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, DeriveFunctor, 
  GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses,
  TupleSections, ViewPatterns #-}

{-



Large-scale dissonance
Built on simple textures (stacked fifths, fourths, thirds etc)




Overlap, fade-out, fade in
  This is one key to getting a SMOOTH final texture

Local dissonance, phased
  This is another key to getting a SMOOTH final texture
  
  Phasing: Harmonic frequency ("harmonic rhythm") and phase
  In short, each texture "moves" between different (related) harmonic aspects with various frequency.

  Phase differences counteracts the perception of "points" (as in syncopation).
  (Harmonic) Frequency differences removes the regularity/ostinato sensation.

We do not shy away from extreme registers, dynamics, consonances/dissonances (or even sharp articulations)
But we want to keep all these potentially disorienting aspects IN CHECK.

  Timbre/instrument
  Harmony (obviously)
  Register
  Speed/movability (alteration between notes/scales vs sustain etc)
  Dynamics

------
ORCHESTRA: 3333 4331 1perc(=?) hrp cel timp str(a4/3/2, a4/3/2, a2, a4/3/2, a4/2 + soli)
Perc: 2 piatti (small and large), GC, snare

Title etc?
  The piece is in part an hommage to Britten 4SI (no 1 in particular).
  Duration about 17 minutes (ca 370-500 bars)


Curious fact: I used a related "block-wise" composition technique in several early pieces:
  Awareness (the line/klang technique was a precursor to it)
  Of a Cirque Glacier
  Shadowings (sort-of)
  Layers
  Passages (really a refinement/different direction)
  Imitations
  Interludes

-}
import Music.Prelude hiding (
    flutes1,    flutes2,
    oboes1,     oboes2,
    clarinets1, clarinets2,
    bassoons1,  bassoons2,
    trumpets1,  trumpets2,
    trombones1, trombones2
    )
import Data.Foldable (Foldable)
import qualified Music.Score
import qualified Data.List
import qualified Data.Ord
import qualified Data.Either
import qualified Data.Maybe
import qualified Music.Time.Internal.Convert
import Music.Time.Internal.Util (rotate)
import Util

-- 3333 4331 2perc(=?) timp str(a4/3/2, a4/3/2, a2, a4/3/2, a4/2 + soli)
orchParts :: [Part]
orchParts = [flutes1,flutes2,oboes1,oboes2,{-"tutti"-}tutti corAnglais,clarinets1,clarinets2,bassoons1,bassoons2]
  <> divide 4 horns <> divide 3 trumpets <> divide 3 trombones <> [tutti tuba]
  <> [tutti timpani]
  <> divide 2 violins <> [violas] <> [cellos] <> [doubleBasses]


{-
Harmonically and structurally, the piece is based on juxtaposition of Guido's hexachord (GH1).
We use all 12 transpositions, but c and fs play the role of starting point and ending point.
eb and a are used as a secondary group.

Structure the piece as a series of blocks/notes.
Probably:
  - Use a CF of 24 or 48 blocks, and to each note align a secondary voice (some of which are very short etc).
  - Or use a recursive expansion: 
    2, to each align a voice of 2 or 3, to each align a voice etc.
  - Not sure about this yet
For each block, pick *one* of the 12 hexachord transpositions *and* a register (just an ambitus). Note that
the combination of hexachord and ambitus may yield a finite harmonic field of everything from one to lots of pitches.
  
-----

How to represent the various "blocks"?
  As a Score – of what?
  Each block is simply a note
  
  Maybe just: Score (Score StandardNote)
  No, start with something more simplitic Score (TextureType, Register, Harmomy, HarmFreqAndPhase) etc.
  Render that simple thing into Score StandardNote etc and use monadic bind.
  
  tiles :: Score (Texture...)
  renderTile :: (Texture...) -> Score StandardNote
  music = tiles >>= renderTile


A texture "fills" a certain duration span with pitches using melody, harmony, orchestration and dynamics.
To create various textures, let's build a combinator library. We want to be able to do the following
to textures:
  - Create simplistic ones (i.e. drones, repeated patterns, phased melody segments)
  - Alter (event-level) frequency and phase (i.e. duration and onset of pattern)
  - 

-}


{-
TODO
Develop time structure (sections, overlapping etc)
Develop harmony (subdivisions etc)

See sketch about complimentary 12-note fields
A 12-tone cluster is gradually shifted/filtered/deconstructed
Degrees of dissonance

Modality vs tonality, absolute points vs relative fields

Relationship betweeen *timbre* and *harmony*:
  Dissonances become more noticable if orchestrated *similarly* and in the same *register*
  
Because we use a 12-note field, we are forced to control dissonance by orchestration

>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 0) [1..6])
[7,8,9,10,11,12]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 1) [1..6])
[8,9,10,11,12]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 2) [1..6])
[9,10,11,12]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 3) [1..6])
[10,11,12]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 4) [1..6])
[11,12]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 5) [1..6])
[12]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 6) [1..6])
[]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 7) [1..6])
[7]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 8) [1..6])
[7,8]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 9) [1..6])
[7,8,9]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 10) [1..6])
[7,8,9,10]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 11) [1..6])
[7,8,9,10,11]
>>> (Data.List.\\) [1..12] ([1..6]<>map (+ 12) [1..6])
[7,8,9,10,11,12]


WE NEED SOME FORM OF SHORT SCORE TO INSPECT THIS

-}
hexachord1 :: (IsPitch a, Transposable a) => [a]
hexachord1 = [c,d,e,f,g,a]

hexachord2 :: (IsPitch a, Transposable a) => [a]
hexachord2 = up _A4 [c,d,e,f,g,a]

-- hexachord1 <> hexachord2 is the most dissonant thing we can do
-- Transposing either by some interval makes them *less* dissonant

-- very dissonant
both3 :: Music
both3   = set parts' violins (pcat hexachord1) <> set parts' violins (pcat hexachord2)
both3'  = set parts' horns (pcat hexachord1) <> set parts' trombones (pcat hexachord2)

-- quite dissonant
both1 :: Music
both1   = set parts' violins (pcat hexachord1) <> set parts' horns (pcat hexachord2)
both1'  = set parts' horns (pcat hexachord1)   <> set parts' violins (pcat hexachord2)

-- not so dissonant
both4 :: Music
both4   = set parts' flutes (pcat hexachord1) <> set parts' trombones (pcat hexachord2)
both4'  = set parts' flutes (pcat hexachord1) <> set parts' oboes (pcat hexachord2)

both4'' = (\x -> ((set parts' flutes . level ff . up _P8))x <> (set parts flutes . level ff)x) (pcat hexachord1)
  <> set parts' bassoons (pcat hexachord2)

-- Basic, basic sketch (480 bars at (1/4) = 120)
form = pcat hexachord1^*5 |> (pcat hexachord1 <> pcat hexachord2)^*4 |> pcat hexachord2^*5

-- Dummy to get all orchestral parts
orch = flip doubleParts c $ orchParts


music :: Music
music = asScore $ stretchTo 480 (parts' .~ violins $ form) <> orch

{-
>>> let hexachords n = up _P8 $ set parts' oboes (pcat hexachord1) <> set parts' trumpets (up (i n) $ pcat hexachord2)
>>> :pl pseq $ fmap hexachords [0..12]
-}



{-
Work out some good combinations to *arrive* to (i.e. transposition and distribution of hexachords + instrumentation)
Then attempt to create a large *polyphonic* (overlapping blocks) structure using this

-}

{-
(Registral) distribution?

We need a function to distribute a hexachord into some register, then apply instrumentation

-}

{-
Texture
  I want to have some freedom with this (particularly when it comes to melody!)
  But it would be good to have some "arrival points".
  Preferably very simple, but not just long notes, nor just JCA-style repetition.

- Automatic dovetailing etc
- Dynamic transitions! Don't stay statically in one texture (JCA-style!)  

                                                 
-}

-- -- our staring point
-- foo :: Score (Texture, [Pitch])
-- foo = [
--   (0  >-> 1,  (t1, [c..a]))^.event,
--   (1  >-> 2,  (t0, [c,e,f,g,a,b]))^.event,
--   (1  >-> 2,  (t2, up (i 3) [c,e,g,a]))^.event,
--   (14 >-> 2,  (t1, up (i 4) [c..a]))^.event
--   
--   ]^.score

-- bar :: Music
-- bar = renderHarmonicTexture foo

-- renderHarmonicTexture :: Score (Texture, [Pitch]) -> Music
-- renderHarmonicTexture x = addDuration x >>= uncurry3 (flip renderTexture)

renderTexture = undefined

-----

-- >>> :o fmap fromPitch'' $ mpseqter bachCMaj 

-- Chords for Bach WTC I C major prelude (last 3 bars simplified)
bachCMajChords :: IsPitch a => Score (Chord a)
bachCMajChords = 
  [(0 <-> (1/2),[c,e,g,c',e'])^.event,((1/2) <-> 1,[c,e,g,c',e'])^.event,(1 <-> (3/2),[c,d,a,d',f'])^.event,((3/2) <->
  2,[c,d,a,d',f'])^.event,(2 <-> (5/2),[b_,d,g,d',f'])^.event,((5/2) <-> 3,[b_,d,g,d',f'])^.event,(3 <->
  (7/2),[c,e,g,c',e'])^.event,((7/2) <-> 4,[c,e,g,c',e'])^.event,(4 <-> (9/2),[c,e,a,e',a'])^.event,((9/2) <->
  5,[c,e,a,e',a'])^.event,(5 <-> (11/2),[c,d,fs,a,d'])^.event,((11/2) <-> 6,[c,d,fs,a,d'])^.event,(6 <->
  (13/2),[b_,d,g,d',g'])^.event,((13/2) <-> 7,[b_,d,g,d',g'])^.event,(7 <-> (15/2),[b_,c,e,g,c'])^.event,((15/2) <->
  8,[b_,c,e,g,c'])^.event,(8 <-> (17/2),[a_,c,e,g,c'])^.event,((17/2) <-> 9,[a_,c,e,g,c'])^.event,(9 <->
  (19/2),[d_,a_,d,fs,c'])^.event,((19/2) <-> 10,[d_,a_,d,fs,c'])^.event,(10 <-> (21/2),[g_,b_,d,g,b])^.event,((21/2) <->
  11,[g_,b_,d,g,b])^.event,(11 <-> (23/2),[g_,bb_,e,g,cs'])^.event,((23/2) <-> 12,[g_,bb_,e,g,cs'])^.event,(12 <->
  (25/2),[f_,a_,d,a,d'])^.event,((25/2) <-> 13,[f_,a_,d,a,d'])^.event,(13 <-> (27/2),[f_,ab_,d,f,b])^.event,((27/2) <->
  14,[f_,ab_,d,f,b])^.event,(14 <-> (29/2),[e_,g_,c,g,c'])^.event,((29/2) <-> 15,[e_,g_,c,g,c'])^.event,(15 <->
  (31/2),[e_,f_,a_,c,f])^.event,((31/2) <-> 16,[e_,f_,a_,c,f])^.event,(16 <-> (33/2),[d_,f_,a_,c,f])^.event,((33/2) <->
  17,[d_,f_,a_,c,f])^.event,(17 <-> (35/2),[g__,d_,g_,b_,f])^.event,((35/2) <-> 18,[g__,d_,g_,b_,f])^.event,(18 <->
  (37/2),[c_,e_,g_,c,e])^.event,((37/2) <-> 19,[c_,e_,g_,c,e])^.event,(19 <-> (39/2),[c_,g_,bb_,c,e])^.event,((39/2) <->
  20,[c_,g_,bb_,c,e])^.event,(20 <-> (41/2),[f__,f_,a_,c,e])^.event,((41/2) <-> 21,[f__,f_,a_,c,e])^.event,(21 <->
  (43/2),[fb__,c_,a_,c,eb])^.event,((43/2) <-> 22,[fb__,c_,a_,c,eb])^.event,(22 <-> (45/2),[ab__,f_,b_,c,d])^.event,((45/2)
  <-> 23,[ab__,f_,b_,c,d])^.event,(23 <-> (47/2),[g__,f_,g_,b_,d])^.event,((47/2) <-> 24,[g__,f_,g_,b_,d])^.event,(24 <->
  (49/2),[g__,e_,g_,c,e])^.event,((49/2) <-> 25,[g__,e_,g_,c,e])^.event,(25 <-> (51/2),[g__,d_,g_,c,f])^.event,((51/2) <->
  26,[g__,d_,g_,c,f])^.event,(26 <-> (53/2),[g__,d_,g_,b_,f])^.event,((53/2) <-> 27,[g__,d_,g_,b_,f])^.event,(27 <->
  (55/2),[g__,eb_,a_,c,fs])^.event,((55/2) <-> 28,[g__,eb_,a_,c,fs])^.event,(28 <-> (57/2),[g__,e_,g_,c,g])^.event,((57/2)
  <-> 29,[g__,e_,g_,c,g])^.event,(29 <-> (59/2),[g__,d_,g_,c,f])^.event,((59/2) <-> 30,[g__,d_,g_,c,f])^.event,(30 <->
  (61/2),[g__,d_,g_,b_,f])^.event,((61/2) <-> 31,[g__,d_,g_,b_,f])^.event,(31 <-> (63/2),[c__,c_,g_,bb_,e])^.event,((63/2)
  <-> 32,[c__,c_,g_,bb_,e])^.event,(32 <-> 33,[c__,c_,f_,a_,e])^.event,(33 <-> 34,[c__,b__,f,g,d'])^.event,(34 <->
  35,[c__,c_,e,g,c'])^.event]^.score

-- -- Given an abstract pattern and a chord, return a pattern using the pitches of the chord.
-- renderChordPattern :: Pattern ChordNote -> Chord a -> Pattern a
-- -- renderChordPattern texture chord = fmap (\n -> cycle chord !! n) texture
-- renderChordPattern texture chord = renderChordPatternX texture (chordToPitchMap chord)
-- 
-- -- For each note, render the pattern at (0 <-> 1) and stretch to duration of note.
-- renderChords :: (Reversible a, Splittable a) => Pattern ChordNote -> Score (Chord a) -> Score a
-- -- renderChords pattern chords = renderPattern zeroV =<< fmap (renderChordPattern pattern) chords
-- renderChords pattern chords = join $ fmap (renderPattern zeroV . renderChordPattern pattern) chords
-- 
-- -- Render a pattern at the duration of a note.
-- renderChordsA :: (Reversible a, Splittable a) => Pattern ChordNote -> Score (Chord a) -> Score a
-- renderChordsA pattern chords = join $ mapWithSpan (\s -> transform (negateV s) . renderPattern s . renderChordPattern pattern) chords

{-
Compare:

  >>> let p = (newPattern $ compress 16 $ [0,1,2,3]^.voice)
  >>> let s = [(0 <-> 0.5,[c,e,g,c',e'])^.event,(0.5 <-> 1.5, [ds,fs,a,c',ds'])^.event]^.score
  >>> :o fmap fromPitch'' $ renderChords p s
  >>> :o fmap fromPitch'' $ renderChordsA p s
-}

-- Pattern used in Bach C Major prelude
bachCMajPattern :: (Reversible a, Num a) => Pattern a
bachCMajPattern = newPattern $ stretchTo 1 $ (^.voice) $ fmap pure [0,1,2,3,4,2,3,4]

bachCMaj :: IsPitch b => Score b
bachCMaj = fmap fromPitch'' $ bachCMaj'
bachCMaj' = renderPatternsRel $ fmap (renderChordPattern bachCMajPattern . chordToPitchMap) bachCMajChords
bachCMaj'2 = renderPatternsAbs $ fmap (renderChordPattern (compress 2 bachCMajPattern) . chordToPitchMap) bachCMajChords
-- Note: renderPatternsAbs is generally *much* slower

testBach = ((^.events).tr) bachCMaj' == ((^.events).tr) (bachCMaj'2 :: Score Pitch)
  where
    tr = filterWithSpan (\s x -> s^.onset < 10)

s1 = [(0 <-> (1/2),c)^.event,((1/2) <-> (5/8),c')^.event,((5/8) <-> 1,g)^.event,(1 <-> (9/8),c')^.event,((9/8) <->
  (3/2),e')^.event,((3/2) <-> (13/8),e')^.event,((13/8) <-> (7/4),g')^.event,((7/4) <-> (15/8),a')^.event,((15/8) <->
  2,e')^.event,(2 <-> (9/4),g')^.event,((9/4) <-> 3,e)^.event]^.score



type PitchMap = Pitch -> Pitch
defPitchMap = id

-- Map *diatonic steps* to a chord.
-- Middle c is index 0, d is index 1 and so on.
chordToPitchMap :: Chord Pitch -> PitchMap
chordToPitchMap chord p = chord !! (fromIntegral ((p.-.c)^._steps) `mod` length chord)

renderChordPattern :: (HasPitches' a, PitchOf a ~ Pitch) => Pattern a -> PitchMap -> Pattern a
renderChordPattern texture pitchMap = fmap (over pitches' pitchMap) texture


foo :: Score (Pattern (PartT Part (DynamicT Dynamics Pitch)), Chord Pitch)
foo = pseq 
  -- [ stretch 4    $ (mempty, (pat1<>hrnPat, [c,e,g,c',e',g',c'']))  ^.event.eventScore
  -- , stretch 3.25 $ (mempty, (pat1<>hrnPat, [b_,e,gs,d',f',a',b']))^.event.eventScore
  -- , stretch 5.25 $ (mempty, (pat1<>hrnPat, [c,e,g,c',e',g',c']))  ^.event.eventScore
  -- , stretch 10   $ (mempty, (pat1<>hrnPat, [b_,e,gs,d',f',a',b']))^.event.eventScore
  [ stretch 4    $ (mempty, (pat1<>hrnPat, hexachord1))^.event.eventScore
  , stretch 2    $ (mempty, (pat1<>hrnPat, hexachord2))^.event.eventScore
  , stretch 3.25 $ (mempty, (pat1<>hrnPat, hexachord1))^.event.eventScore
  , stretch 2.25 $ (mempty, (pat1<>hrnPat, hexachord2))^.event.eventScore
  , stretch 2    $ (mempty, (pat1<>hrnPat, hexachord1))^.event.eventScore
  , stretch 3.25 $ (mempty, (pat1<>hrnPat, hexachord2))^.event.eventScore
  , stretch 2.25 $ (mempty, (pat1<>hrnPat, hexachord1))^.event.eventScore
  , stretch 10   $ (mempty, (pat1<>hrnPat, hexachord2))^.event.eventScore
  ]
  where

fooX :: Score (Pattern (PartT Part (DynamicT Dynamics Pitch)), Chord Pitch)
fooX = pseq 
  -- [ stretch 4    $ (mempty, (pat1<>hrnPat, [c,e,g,c',e',g',c'']))  ^.event.eventScore
  -- , stretch 3.25 $ (mempty, (pat1<>hrnPat, [b_,e,gs,d',f',a',b']))^.event.eventScore
  -- , stretch 5.25 $ (mempty, (pat1<>hrnPat, [c,e,g,c',e',g',c']))  ^.event.eventScore
  -- , stretch 10   $ (mempty, (pat1<>hrnPat, [b_,e,gs,d',f',a',b']))^.event.eventScore
  [ stretch 4    $ (mempty, (hrnPat2, hexachord1))^.event.eventScore
  , stretch 2    $ (mempty, (hrnPat2, hexachord2))^.event.eventScore
  , stretch 3.25 $ (mempty, (hrnPat2, hexachord1))^.event.eventScore
  , stretch 2.25 $ (mempty, (hrnPat2, hexachord2))^.event.eventScore
  , stretch 2    $ (mempty, (hrnPat2, hexachord1))^.event.eventScore
  , stretch 3.25 $ (mempty, (hrnPat2, hexachord2))^.event.eventScore
  , stretch 2.25 $ (mempty, (hrnPat2, hexachord1))^.event.eventScore
  , stretch 10   $ (mempty, (hrnPat2, hexachord2))^.event.eventScore
  ]
  where

foo2 :: Score (Pattern (PartT Part (DynamicT Dynamics Pitch)), Chord Pitch)
foo2 = pseq 
  -- [ stretch 4    $ (mempty, (pat1<>hrnPat, [c,e,g,c',e',g',c'']))  ^.event.eventScore
  -- , stretch 3.25 $ (mempty, (pat1<>hrnPat, [b_,e,gs,d',f',a',b']))^.event.eventScore
  -- , stretch 5.25 $ (mempty, (pat1<>hrnPat, [c,e,g,c',e',g',c']))  ^.event.eventScore
  -- , stretch 10   $ (mempty, (pat2, [b_,e,gs,d',f',a',b']))^.event.eventScore
  [ stretch 2    $ (mempty, (pat2<>tptPat,  hexachord2))^.event.eventScore
  , stretch 3.25 $ (mempty, (pat2, hexachord1))^.event.eventScore
  , stretch 2.25 $ (mempty, (pat2, hexachord2))  ^.event.eventScore
  , stretch 2    $ (mempty, (pat2<>tbnPat,  hexachord1))^.event.eventScore
  , stretch 3.25 $ (mempty, (pat2, hexachord2))^.event.eventScore
  , stretch 2.25 $ (mempty, (pat2, hexachord1))  ^.event.eventScore
  , stretch 10   $ (mempty, (pat2<>tptPat, hexachord2))^.event.eventScore
  ]
  where

pat1, hrnPat :: (IsPitch b, HasParts' b, Reversible b, PartOf b ~ Part) => Pattern b
pat1 = (\x -> mempty
    <> (delay (0/16))  (fmap (set parts' violins1)  x)
    <> (delay (4/16))  (fmap (set parts' violins2)  x)
    <> (delay (6/16))  (fmap (set parts' violas)    x)
    <> (delay (10/16)) (fmap (set parts' cellos1a2) x)
    <> (delay (16/16)) (fmap (set parts' cellos2a2) x)
    )
  $ monophonicScoreToPattern $ stretch (1/16) $ pseq [c,e,g,b,b,g,e,c]
pat2 = (\x -> mempty
    <> (delay (0/6)) (fmap (set parts' flutes1)    x)
    <> (delay (1/6)) (fmap (set parts' flutes2)    x)
    <> (delay (2/6)) (fmap (set parts' flutes3)    x)
    <> (delay (3/6)) (fmap (set parts' clarinets1) x)
    <> (delay (4/6)) (fmap (set parts' clarinets2) x)
    <> (delay (5/6)) (fmap (set parts' clarinets3) x)
    -- <> (delay (0/6)) (fmap (set parts' oboes1)     x)
    -- <> (delay (0/6)) (fmap (set parts' oboes2)     x)
    )
  $ monophonicScoreToPattern $ stretch (1/6) $ upDiatonic c 1 $ pseq [c^*4,e^*4,g^*3,b^*1,b^*3,g^*1,e^*4,c^*4]

hrnPat = (\x -> mempty 
    <> delay (1/4) (fmap (set parts' horns1) x) 
    <> delay (3/4) (fmap (set parts' horns2) x) 
    <> delay (5/4) (fmap (set parts' horns3) x) 
    <> delay (6/4) (fmap (set parts' horns4) x) 
    )
  $ monophonicScoreToPattern $ stretchTo 4 $ pseq [c^*3,e,g^*8,e^*4]
hrnPat2 = (\x -> mempty 
    <> delay (1/4)  (fmap (set parts' horns1) x) 
    <> delay (3/4)  (fmap (set parts' horns2) x) 
    <> delay (5/4)  (fmap (set parts' horns3) x) 
    <> delay (6/4)  (fmap (set parts' horns4) x) 
    <> delay (8/4)  (fmap (set parts' bassoons1) x) 
    <> delay (9/4)  (fmap (set parts' bassoons2) x) 
    <> delay (10/4) (fmap (set parts' clarinets1) x) 
    <> delay (11/4) (fmap (set parts' clarinets2) x) 
    )
  $ monophonicScoreToPattern $ stretchTo 4 $ pseq [c^*3,e,g^*8,e^*4]

tptPat = (\x -> mempty 
    <> delay (0/4) (fmap (set parts' trumpets1) x) 
    <> delay (0/4) (fmap (set parts' trumpets2) x) 
    <> delay (0/4) (fmap (set parts' trumpets3) x) 
    )
  $ monophonicScoreToPattern $ stretchTo (1/8) $ pseq [c^*7,c'^*1]

tbnPat = (\x -> mempty 
    <> delay (0/4) (fmap (set parts' trombones1) x) 
    <> delay (0/4) (fmap (set parts' trombones2) x) 
    <> delay (0/4) (fmap (set parts' trombones3) x) 
    )
  $ monophonicScoreToPattern $ stretchTo (1/4) $ pseq [c^*7,c'^*1]


[flutes1,flutes2,flutes3] = divide 3 oboes
[oboes1,oboes2,oboes3] = divide 3 oboes
[clarinets1,clarinets2,clarinets3] = divide 3 clarinets
[bassoons1,bassoons2,bassoons3] = divide 3 bassoons

[horns1,horns2,horns3,horns4] = divide 4 horns
[trumpets1,trumpets2,trumpets3] = divide 3 trumpets
[trombones1,trombones2,trombones3] = divide 3 trombones
[cellos1a2,cellos2a2] = divide 2 cellos
timp = solo timpani
cel = solo celesta
hrp = harp

-- TODO ***register***, dynamics, rests

-- TODO fix HasParts' Pattern etc instances
-- then we could use doubleParts etc.
    
bar = renderPatternsAbs  $ fmap (\(p,c) -> renderChordPattern p (chordToPitchMap c)) foo
bar2 = renderPatternsAbs $ fmap (\(p,c) -> renderChordPattern p (chordToPitchMap c)) foo2
bar3 = renderPatternsAbs $ fmap (\(p,c) -> renderChordPattern p (chordToPitchMap c)) fooX
baz :: Score StandardNote
baz = fmap fromAspects $ bar <> bar2
 
fromAspects :: (
  HasPitch' a,
  HasPart' a,
  -- HasArticulation' a,
  HasDynamic' a,
  -- ArticulationOf a ~ Articulation,
  DynamicOf a ~ Dynamics,
  PartOf a ~ Part,
  PitchOf a ~ Pitch
                
                ) => a -> StandardNote
fromAspects x = {-set articulation' (x^.articulation) $-} set dynamic' (x^.dynamic') $ set part' (x^.part') $ fromPitch'' (x^.pitch')


eventScore = to ((^.score) . pure)

-- TODO how to implement renderChordPattern with other info than pitch (i.e. part, articulation, dynamics) in pattern

-- findCommonOrigin :: [(Time, Duration)]
-- findCommonOrigin

-- bach1 = renderHarmonicTexture $ fmap (droneT,) bachCMaj
-- bach2 = renderHarmonicTexture $ fmap (t1,) bachCMaj


subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)



{-
Classes of hexachords:
  [2,2,2,2,2,2] (whole tone, just 1 permutation)
Those involving 3*1 (one m3)
  permutations [2,2,1,2,2,3] (720)
  Can contain a tritone *only* if the the 3 and 1 are adjacent!
  
Those involving 3*2 (two m3)
  [2,2,1,1,3,3]
  
-}
-- All 12-tone hexachords built on the intervals [2,2,1,2,2,3] (Guido's Hexachord)
-- TODO remove inversions (rotations)

allHexaChords :: [[Pitch]]
allHexaChords = id
  -- Lots of equals, due to the repeated 2 etc
  $ Data.List.nub

  $ fmap (fmap (relative c (spell usingSharps)). offsetPoints (c::Pitch))
  $ fmap (fmap $ spell usingSharps) $ Data.List.permutations [2,2,1,2,2::Semitones]

allHexaChords' = timeSignature (6/4) $ compress 4 $ pseq $ concat $ (fmap.fmap) fromPitch'' allHexaChords


firstChordSketchStr = (firstChordSketch^*20) >>= (\x -> stretchTo 1 $ times (8*10) $ [x]^.score)

-- Stretch by 20 to get full duration
firstChordSketch =
  [(0 <-> (1/2),d_)^.event,(0 <-> (1/2),f_)^.event,(0 <-> (1/2),a_)^.event,(0 <-> (1/2),c)^.event,(0 <-> (1/2),e)^.event,(0
  <-> (1/2),g)^.event,((1/2) <-> 1,f_)^.event,((1/2) <-> 1,a_)^.event,((1/2) <-> 1,c)^.event,((1/2) <-> 1,d)^.event,((1/2)
  <-> 1,e)^.event,((1/2) <-> 1,g)^.event,(1 <-> (3/2),a_)^.event,(1 <-> (3/2),c)^.event,(1 <-> (3/2),d)^.event,(1 <->
  (3/2),e)^.event,(1 <-> (3/2),f)^.event,(1 <-> (3/2),g)^.event,((3/2) <-> 2,a_)^.event,((3/2) <-> 2,c)^.event,((3/2) <->
  2,d)^.event,((3/2) <-> 2,e)^.event,((3/2) <-> 2,f)^.event,((3/2) <-> 2,g)^.event,((3/2) <-> 2,c')^.event,(2 <->
  (5/2),a_)^.event,(2 <-> (5/2),c)^.event,(2 <-> (5/2),d)^.event,(2 <-> (5/2),e)^.event,(2 <-> (5/2),f)^.event,(2 <->
  (5/2),g)^.event,(2 <-> (5/2),c')^.event,(2 <-> (5/2),d')^.event,((5/2) <-> 3,a_)^.event,((5/2) <-> 3,d)^.event,((5/2) <->
  3,e)^.event,((5/2) <-> 3,f)^.event,((5/2) <-> 3,g)^.event,((5/2) <-> 3,c')^.event,((5/2) <-> 3,d')^.event,((5/2) <->
  3,f')^.event,(3 <-> (7/2),c)^.event,(3 <-> (7/2),d)^.event,(3 <-> (7/2),e)^.event,(3 <-> (7/2),f)^.event,(3 <->
  (7/2),g)^.event,(3 <-> (7/2),a)^.event,((7/2) <-> 4,d_)^.event,((7/2) <-> 4,f_)^.event,((7/2) <-> 4,a_)^.event,((7/2) <->
  4,c)^.event,((7/2) <-> 4,e)^.event,((7/2) <-> 4,g)^.event,(4 <-> (9/2),e_)^.event,(4 <-> (9/2),g_)^.event,(4 <->
  (9/2),c)^.event,(4 <-> (9/2),d)^.event,(4 <-> (9/2),f)^.event,(4 <-> (9/2),a)^.event,((9/2) <-> 5,f_)^.event,((9/2) <->
  5,a_)^.event,((9/2) <-> 5,c)^.event,((9/2) <-> 5,d)^.event,((9/2) <-> 5,g)^.event,((9/2) <-> 5,e')^.event,(5 <->
  (11/2),e_)^.event,(5 <-> (11/2),f_)^.event,(5 <-> (11/2),e)^.event,(5 <-> (11/2),f)^.event,(5 <-> (11/2),c'')^.event,(5
  <-> (11/2),d'')^.event,(5 <-> (11/2),g___)^.event,(5 <-> (11/2),a___)^.event,((11/2) <-> 6,e__)^.event,((11/2) <->
  6,f_)^.event,((11/2) <-> 6,f)^.event,((11/2) <-> 6,c')^.event,((11/2) <-> 6,e')^.event,((11/2) <-> 6,d'')^.event,((11/2)
  <-> 6,a___)^.event,((11/2) <-> 6,g__)^.event,(6 <-> (13/2),c)^.event,(6 <-> (13/2),d)^.event,(6 <-> (13/2),e)^.event,(6
  <-> (13/2),f)^.event,(6 <-> (13/2),g)^.event,(6 <-> (13/2),a)^.event,(6 <-> (13/2),eb)^.event,(6 <-> (13/2),f)^.event,(6
  <-> (13/2),g)^.event,(6 <-> (13/2),ab)^.event,(6 <-> (13/2),bb)^.event,(6 <-> (13/2),c')^.event,((13/2) <->
  7,c)^.event,((13/2) <-> 7,d)^.event,((13/2) <-> 7,e)^.event,((13/2) <-> 7,f)^.event,((13/2) <-> 7,g)^.event,((13/2) <->
  7,a)^.event,((13/2) <-> 7,eb)^.event,((13/2) <-> 7,f)^.event,((13/2) <-> 7,g)^.event,((13/2) <-> 7,ab)^.event,((13/2) <->
  7,bb)^.event,((13/2) <-> 7,c')^.event,(7 <-> (15/2),c)^.event,(7 <-> (15/2),d)^.event,(7 <-> (15/2),e)^.event,(7 <->
  (15/2),f)^.event,(7 <-> (15/2),g)^.event,(7 <-> (15/2),a)^.event,(7 <-> (15/2),eb)^.event,(7 <-> (15/2),f)^.event,(7 <->
  (15/2),g)^.event,(7 <-> (15/2),ab)^.event,(7 <-> (15/2),bb)^.event,(7 <-> (15/2),c')^.event,((15/2) <->
  8,c)^.event,((15/2) <-> 8,d)^.event,((15/2) <-> 8,e)^.event,((15/2) <-> 8,f)^.event,((15/2) <-> 8,g)^.event,((15/2) <->
  8,a)^.event,((15/2) <-> 8,eb)^.event,((15/2) <-> 8,f)^.event,((15/2) <-> 8,g)^.event,((15/2) <-> 8,ab)^.event,((15/2) <->
  8,bb)^.event,((15/2) <-> 8,c')^.event,(8 <-> (17/2),c)^.event,(8 <-> (17/2),d)^.event,(8 <-> (17/2),e)^.event,(8 <->
  (17/2),f)^.event,(8 <-> (17/2),g)^.event,(8 <-> (17/2),a)^.event,(8 <-> (17/2),eb)^.event,(8 <-> (17/2),f)^.event,(8 <->
  (17/2),g)^.event,(8 <-> (17/2),ab)^.event,(8 <-> (17/2),bb)^.event,(8 <-> (17/2),c')^.event,((17/2) <->
  9,c)^.event,((17/2) <-> 9,d)^.event,((17/2) <-> 9,e)^.event,((17/2) <-> 9,f)^.event,((17/2) <-> 9,g)^.event,((17/2) <->
  9,a)^.event,((17/2) <-> 9,eb)^.event,((17/2) <-> 9,f)^.event,((17/2) <-> 9,g)^.event,((17/2) <-> 9,ab)^.event,((17/2) <->
  9,bb)^.event,((17/2) <-> 9,c')^.event,(9 <-> (19/2),c)^.event,(9 <-> (19/2),d)^.event,(9 <-> (19/2),e)^.event,(9 <->
  (19/2),f)^.event,(9 <-> (19/2),g)^.event,(9 <-> (19/2),a)^.event,(9 <-> (19/2),eb)^.event,(9 <-> (19/2),f)^.event,(9 <->
  (19/2),g)^.event,(9 <-> (19/2),ab)^.event,(9 <-> (19/2),bb)^.event,(9 <-> (19/2),c')^.event,((19/2) <->
  10,c)^.event,((19/2) <-> 10,d)^.event,((19/2) <-> 10,e)^.event,((19/2) <-> 10,f)^.event,((19/2) <-> 10,g)^.event,((19/2)
  <-> 10,a)^.event,((19/2) <-> 10,eb)^.event,((19/2) <-> 10,f)^.event,((19/2) <-> 10,g)^.event,((19/2) <->
  10,ab)^.event,((19/2) <-> 10,bb)^.event,((19/2) <-> 10,c')^.event,(10 <-> (21/2),c)^.event,(10 <-> (21/2),d)^.event,(10
  <-> (21/2),e)^.event,(10 <-> (21/2),f)^.event,(10 <-> (21/2),g)^.event,(10 <-> (21/2),a)^.event,(10 <->
  (21/2),eb)^.event,(10 <-> (21/2),f)^.event,(10 <-> (21/2),g)^.event,(10 <-> (21/2),ab)^.event,(10 <->
  (21/2),bb)^.event,(10 <-> (21/2),c')^.event,((21/2) <-> 11,c)^.event,((21/2) <-> 11,d)^.event,((21/2) <->
  11,e)^.event,((21/2) <-> 11,f)^.event,((21/2) <-> 11,g)^.event,((21/2) <-> 11,a)^.event,((21/2) <-> 11,eb)^.event,((21/2)
  <-> 11,f)^.event,((21/2) <-> 11,g)^.event,((21/2) <-> 11,ab)^.event,((21/2) <-> 11,bb)^.event,((21/2) <->
  11,c')^.event,(11 <-> (23/2),c)^.event,(11 <-> (23/2),d)^.event,(11 <-> (23/2),e)^.event,(11 <-> (23/2),f)^.event,(11 <->
  (23/2),g)^.event,(11 <-> (23/2),a)^.event,(11 <-> (23/2),eb)^.event,(11 <-> (23/2),f)^.event,(11 <-> (23/2),g)^.event,(11
  <-> (23/2),ab)^.event,(11 <-> (23/2),bb)^.event,(11 <-> (23/2),c')^.event,((23/2) <-> 12,c)^.event,((23/2) <->
  12,d)^.event,((23/2) <-> 12,e)^.event,((23/2) <-> 12,f)^.event,((23/2) <-> 12,g)^.event,((23/2) <-> 12,a)^.event,((23/2)
  <-> 12,eb)^.event,((23/2) <-> 12,f)^.event,((23/2) <-> 12,g)^.event,((23/2) <-> 12,ab)^.event,((23/2) <->
  12,bb)^.event,((23/2) <-> 12,c')^.event,(12 <-> (25/2),fs)^.event,(12 <-> (25/2),gs)^.event,(12 <-> (25/2),as)^.event,(12
  <-> (25/2),b)^.event,(12 <-> (25/2),cs')^.event,(12 <-> (25/2),ds')^.event,(12 <-> (25/2),a_)^.event,(12 <->
  (25/2),b_)^.event,(12 <-> (25/2),cs)^.event,(12 <-> (25/2),d)^.event,(12 <-> (25/2),e)^.event,(12 <->
  (25/2),fs)^.event,((25/2) <-> 13,fs)^.event,((25/2) <-> 13,gs)^.event,((25/2) <-> 13,as)^.event,((25/2) <->
  13,b)^.event,((25/2) <-> 13,cs')^.event,((25/2) <-> 13,ds')^.event,((25/2) <-> 13,a)^.event,((25/2) <->
  13,b)^.event,((25/2) <-> 13,cs')^.event,((25/2) <-> 13,d')^.event,((25/2) <-> 13,e')^.event,((25/2) <-> 13,fs')^.event,(13
  <-> (27/2),fs)^.event,(13 <-> (27/2),gs)^.event,(13 <-> (27/2),as)^.event,(13 <-> (27/2),b)^.event,(13 <->
  (27/2),cs')^.event,(13 <-> (27/2),ds')^.event,(13 <-> (27/2),a)^.event,(13 <-> (27/2),b)^.event,(13 <->
  (27/2),cs')^.event,(13 <-> (27/2),d')^.event,(13 <-> (27/2),e')^.event,(13 <-> (27/2),fs')^.event,((27/2) <->
  14,fs)^.event,((27/2) <-> 14,gs)^.event,((27/2) <-> 14,as)^.event,((27/2) <-> 14,b)^.event,((27/2) <->
  14,cs')^.event,((27/2) <-> 14,ds')^.event,((27/2) <-> 14,a)^.event,((27/2) <-> 14,b)^.event,((27/2) <->
  14,cs')^.event,((27/2) <-> 14,d')^.event,((27/2) <-> 14,e')^.event,((27/2) <-> 14,fs')^.event,(14 <->
  (29/2),fs)^.event,(14 <-> (29/2),gs)^.event,(14 <-> (29/2),as)^.event,(14 <-> (29/2),b)^.event,(14 <->
  (29/2),cs')^.event,(14 <-> (29/2),ds')^.event,(14 <-> (29/2),a)^.event,(14 <-> (29/2),b)^.event,(14 <->
  (29/2),cs')^.event,(14 <-> (29/2),d')^.event,(14 <-> (29/2),e')^.event,(14 <-> (29/2),fs')^.event,((29/2) <->
  15,fs)^.event,((29/2) <-> 15,gs)^.event,((29/2) <-> 15,as)^.event,((29/2) <-> 15,b)^.event,((29/2) <->
  15,cs')^.event,((29/2) <-> 15,ds')^.event,((29/2) <-> 15,a)^.event,((29/2) <-> 15,b)^.event,((29/2) <->
  15,cs')^.event,((29/2) <-> 15,d')^.event,((29/2) <-> 15,e')^.event,((29/2) <-> 15,fs')^.event,(15 <->
  (31/2),fs)^.event,(15 <-> (31/2),gs)^.event,(15 <-> (31/2),as)^.event,(15 <-> (31/2),b)^.event,(15 <->
  (31/2),cs')^.event,(15 <-> (31/2),ds')^.event,(15 <-> (31/2),a)^.event,(15 <-> (31/2),b)^.event,(15 <->
  (31/2),cs')^.event,(15 <-> (31/2),d')^.event,(15 <-> (31/2),e')^.event,(15 <-> (31/2),fs')^.event,((31/2) <->
  16,fs)^.event,((31/2) <-> 16,gs)^.event,((31/2) <-> 16,as)^.event,((31/2) <-> 16,b)^.event,((31/2) <->
  16,cs')^.event,((31/2) <-> 16,ds')^.event,((31/2) <-> 16,a)^.event,((31/2) <-> 16,b)^.event,((31/2) <->
  16,cs')^.event,((31/2) <-> 16,d')^.event,((31/2) <-> 16,e')^.event,((31/2) <-> 16,fs')^.event,(16 <->
  (33/2),fs)^.event,(16 <-> (33/2),gs)^.event,(16 <-> (33/2),as)^.event,(16 <-> (33/2),b)^.event,(16 <->
  (33/2),cs')^.event,(16 <-> (33/2),ds')^.event,(16 <-> (33/2),a)^.event,(16 <-> (33/2),b)^.event,(16 <->
  (33/2),cs')^.event,(16 <-> (33/2),d')^.event,(16 <-> (33/2),e')^.event,(16 <-> (33/2),fs')^.event,((33/2) <->
  17,fs)^.event,((33/2) <-> 17,gs)^.event,((33/2) <-> 17,as)^.event,((33/2) <-> 17,b)^.event,((33/2) <->
  17,cs')^.event,((33/2) <-> 17,ds')^.event,((33/2) <-> 17,a)^.event,((33/2) <-> 17,b)^.event,((33/2) <->
  17,cs')^.event,((33/2) <-> 17,d')^.event,((33/2) <-> 17,e')^.event,((33/2) <-> 17,fs')^.event,(17 <->
  (35/2),fs)^.event,(17 <-> (35/2),gs)^.event,(17 <-> (35/2),as)^.event,(17 <-> (35/2),b)^.event,(17 <->
  (35/2),cs')^.event,(17 <-> (35/2),ds')^.event,(17 <-> (35/2),a)^.event,(17 <-> (35/2),b)^.event,(17 <->
  (35/2),cs')^.event,(17 <-> (35/2),d')^.event,(17 <-> (35/2),e')^.event,(17 <-> (35/2),fs')^.event,((35/2) <->
  18,fs)^.event,((35/2) <-> 18,gs)^.event,((35/2) <-> 18,as)^.event,((35/2) <-> 18,b)^.event,((35/2) <->
  18,cs')^.event,((35/2) <-> 18,ds')^.event,((35/2) <-> 18,a)^.event,((35/2) <-> 18,b)^.event,((35/2) <->
  18,cs')^.event,((35/2) <-> 18,d')^.event,((35/2) <-> 18,e')^.event,((35/2) <-> 18,fs')^.event,(18 <->
  (37/2),fs)^.event,(18 <-> (37/2),gs)^.event,(18 <-> (37/2),as)^.event,(18 <-> (37/2),b)^.event,(18 <->
  (37/2),cs')^.event,(18 <-> (37/2),ds')^.event,((37/2) <-> 19,fs)^.event,((37/2) <-> 19,gs)^.event,((37/2) <->
  19,as)^.event,((37/2) <-> 19,b)^.event,((37/2) <-> 19,cs')^.event,((37/2) <-> 19,ds')^.event,(19 <-> (39/2),fs)^.event,(19
  <-> (39/2),gs)^.event,(19 <-> (39/2),as)^.event,(19 <-> (39/2),b)^.event,(19 <-> (39/2),cs')^.event,(19 <->
  (39/2),ds')^.event,((39/2) <-> 20,as_)^.event,((39/2) <-> 20,fs)^.event,((39/2) <-> 20,cs')^.event,((39/2) <->
  20,gs')^.event,((39/2) <-> 20,ds'')^.event,((39/2) <-> 20,b'')^.event,(20 <-> (41/2),b_)^.event,(20 <->
  (41/2),fs)^.event,(20 <-> (41/2),cs')^.event,(20 <-> (41/2),gs')^.event,(20 <-> (41/2),ds'')^.event,(20 <->
  (41/2),as'')^.event,((41/2) <-> 21,ds)^.event,((41/2) <-> 21,as)^.event,((41/2) <-> 21,cs')^.event,((41/2) <->
  21,gs')^.event,((41/2) <-> 21,b')^.event,((41/2) <-> 21,fs'')^.event,(21 <-> (43/2),b_)^.event,(21 <->
  (43/2),fs)^.event,(21 <-> (43/2),cs')^.event,(21 <-> (43/2),gs')^.event,(21 <-> (43/2),ds'')^.event,(21 <->
  (43/2),as'')^.event,((43/2) <-> 22,fs)^.event,((43/2) <-> 22,gs)^.event,((43/2) <-> 22,as)^.event,((43/2) <->
  22,b)^.event,((43/2) <-> 22,cs')^.event,((43/2) <-> 22,ds')^.event,(22 <-> (45/2),as_)^.event,(22 <->
  (45/2),fs)^.event,(22 <-> (45/2),cs')^.event,(22 <-> (45/2),gs')^.event,(22 <-> (45/2),ds'')^.event,(22 <->
  (45/2),b'')^.event,((45/2) <-> 23,b_)^.event,((45/2) <-> 23,fs)^.event,((45/2) <-> 23,cs')^.event,((45/2) <->
  23,gs')^.event,((45/2) <-> 23,ds'')^.event,((45/2) <-> 23,as'')^.event,(23 <-> (47/2),cs)^.event,(23 <->
  (47/2),as)^.event,(23 <-> (47/2),fs')^.event,(23 <-> (47/2),b')^.event,(23 <-> (47/2),ds'')^.event,(23 <->
  (47/2),gs'')^.event,((47/2) <-> 24,b_)^.event,((47/2) <-> 24,fs)^.event,((47/2) <-> 24,cs')^.event,((47/2) <->
  24,gs')^.event,((47/2) <-> 24,ds'')^.event,((47/2) <-> 24,as'')^.event]^.score



sop  = fmap $ set parts' violins1
mez  = fmap $ set parts' violins2
alto = fmap $ set parts' violas
ten  = fmap $ set parts' $ (!! 0) $ divide 2 cellos
bari = fmap $ set parts' $ (!! 1) $ divide 2 cellos
bass = fmap $ set parts' doubleBasses




spat = newPattern . mconcat . map noteToVoice
ppat = mconcat . map (pureP . noteToVoice)

noteToVoice :: Note a -> Voice a
noteToVoice x = [x]^.voice

{-
Create 6, 12 (or 24?) patterns.
Some for all 6 voices, some for just 2, some for 3 etc.
Combine!
-}



exPat1 :: (IsPitch a, HasParts' a, PartOf a ~ Part, Reversible a, Transposable a, Attenuable a) => Score (Pattern (Maybe a))
exPat1 = transform (0<->5) $ pure $ mempty
    <> sop  (stretch (1/8) $ spat [c,d]) 
    <> mez  (stretch (1/4) $ spat [c,d]) 
    <> alto (stretch (1/2) $ spat [c,d])
    <> ten  (stretch 1     $ spat [c,d])
    <> bari (stretch 2     $ spat [c,d])
    <> bass (stretch 4     $ spat [c,d])

exPat2 :: (IsPitch a, HasParts' a, PartOf a ~ Part, Reversible a, Transposable a, Attenuable a) => Score (Pattern (Maybe a))
exPat2 = transform (0<->5) $ pure $ mempty
    <> sop  (stretch (1/16) $ spat [c,d]) 
    <> mez  (stretch (1/8) $ spat [c,d]) 
    <> alto (stretch (1/4) $ spat [c,d])
    <> ten  (stretch (1/2) $ spat [c,d])
    <> bari (stretch (1/1) $ spat [c,d])
    <> bass (stretch (2/4) $ spat [c,d])
exPat3 :: (IsPitch a, HasParts' a, PartOf a ~ Part, Reversible a, Transposable a, Attenuable a) => Score (Pattern (Maybe a))
exPat3 = transform (0<->10.5) $ pure $ mempty
    <> sop  (stretch (1/2) $ delay 1 $ spat [gs,as^*4]) 
    <> mez  (stretch (1/2) $ delay 1 $ spat [ds,es^*4]) 
    <> alto (stretch (1/2) $ spat [c,d^*4])
    -- <> ten  (stretch (1/2) $ spat [c,d^*4])
    -- <> bari (stretch (1/2) $ spat [c,d^*4])
    -- <> bass (stretch (1/2) $ spat [c,d^*4])

pat6Fin1 :: (IsPitch a, HasParts' a, PartOf a ~ Part, Reversible a, Transposable a, Attenuable a) => Score (Pattern (Maybe a))
pat6Fin1 = transform (0<->20) $ pure $ mempty
    <> sop  (stretch (1/8) $ fmap (level ff . up   _P8) $ spat [c,d,f,e,g,f,e,c,d]) 
    <> mez  (stretch (1/4) $ fmap (level ff . up   _P8) $ spat [c,d,f,e,a_,f,e,c,d]) 
    <> alto (stretch (1/2) $ fmap (level ff . down _P1) $ spat [c,d,f,e,g,f,e,c,d,e])
    <> ten  (stretch 1     $ fmap (level ff . down _P1) $ spat [c,d,f,e,g,f,e,c,d,e])
    <> bari (stretch 2     $ fmap (level ff . down _P4) $ spat [c,d,f,e,g,f,e,c,d,e,a_,c])
    <> bass (stretch 4     $ fmap (level ff . down (_P8^+^_P4)) $ spat [c,d,f,e,g,f,e,c,d,e,a_])

restPat :: (IsPitch a, HasParts' a, PartOf a ~ Part, Reversible a, Transposable a, Attenuable a) => Score (Pattern (Maybe a))
restPat = transform (0<->20) $ pure $ mempty
    <> sop  (stretch (1/16)  $ fmap (level ff . up   _P8) $ spat [g,a,g,a,rest^*4]) 
    <> mez  (stretch (1/16)  $ fmap (level ff . up   _P8) $ spat [g,a,g,a,rest^*4]) 
    <> alto (stretch (1/16)  $ fmap (level ff . down _P1) $ spat [g,a,g,a,rest^*4])
    <> ten  (stretch (1/4)   $ fmap (level ff . down _P1) $ spat [g,a,g,a,rest^*4])
    <> bari (stretch (1/4)   $ fmap (level ff . down _P1) $ spat [g,a,g,a,rest^*4])
    <> bass (stretch (1/4)   $ fmap (level ff . down _P1) $ spat [g,a,g,a,rest^*4])


exaudi :: Music
exaudi = foldr1 catPartWise $ map (mcatMaybes) $ 
  [ delay 0  $ level pp $ renderPatternsAbs $ exPat1
  , delay 0  $ renderPatternsAbs $ exPat2
  , delay 0  $ renderPatternsAbs $ exPat1
  , delay 0  $ renderPatternsAbs $ exPat2

  , delay 0  $ renderPatternsAbs $ exPat3
  , delay 10 $ renderPatternsAbs $ fmap2 (up m3) $ fmap2 (down _P8) 
    $ fmap2 high3ToLow3 exPat3
  , delay 0  $ renderPatternsAbs $ exPat3
  , delay 10 $ renderPatternsAbs $ fmap2 (up m3) $ fmap2 (down _P8) 
    $ fmap2 high3ToLow3 exPat3
  , delay 0  $ renderPatternsAbs $ exPat3
  , delay 10 $ renderPatternsAbs $ fmap2 (up m3) $ fmap2 (down _P8) 
    $ fmap2 high3ToLow3 exPat3

  , delay 20 $ renderPatternsAbs $ pat6Fin1
  , delay 0  $ legato $ renderPatternsAbs $ restPat
  ]




high3ToLow3 = replaceParts [(violins1,cellos1),(violins2,cellos2),(violas,doubleBasses)]
[cellos1,cellos2] = divide 2 cellos
fmap2 = (fmap.fmap)