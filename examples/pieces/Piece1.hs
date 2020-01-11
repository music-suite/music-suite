
{-# LANGUAGE
  ConstraintKinds, TypeFamilies, FlexibleContexts, DeriveFunctor, GeneralizedNewtypeDeriving,
  StandaloneDeriving, MultiParamTypeClasses, TupleSections, ViewPatterns #-}

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
ORCHESTRA: 3333 4331 2perc(=?) hrp cel timp str(a4/3/2, a4/3/2, a2, a4/3/2, a4/2 + soli)
Perc: 3 piatti (small, large, sizzle), gran casa, tenor drum

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
    trombones1, trombones2,
    corAnglaises
    )
import Data.Foldable (Foldable)
import qualified Music.Score
import qualified Data.List
import qualified Data.Ord
import qualified Data.Either
import qualified Data.Maybe
import qualified Debug.Trace
import qualified Music.Time.Internal.Convert
import Music.Time.Internal.Util (rotate)
import Util hiding (doveTail2, doveTail3)

__ORCHESTRA__ :: ()
__ORCHESTRA__ = ()

-- 3333 4331 2perc(=?) timp str(a4/3/2, a4/3/2, a2, a4/3/2, a4/2 + soli)
-- orchParts :: [Part]
-- orchParts = [flutes1,flutes2,flutes3,oboes1,oboes2,{-"tutti"-}tutti corAnglais,clarinets1,clarinets2,clarinets3,bassoons1,bassoons2,bassoons3]
--   <> divide 4 horns <> divide 3 trumpets <> divide 3 trombones <> [tutti tuba]
--   <> [timp,hrp,cel]
--   <> divide 2 violins <> [violas] <> [cellos] <> [doubleBasses]

high3ToLow3 = replaceParts [(violins1,cellos1),(violins2,cellos2),(violas,doubleBasses)]

[flutes1,flutes2,flutes3] = divide 3 flutes
[oboes1,oboes2,oboes3] = divide 3 oboes
[clarinets1,clarinets2,clarinets3] = divide 3 clarinets
[bassoons1,bassoons2,bassoons3] = divide 3 bassoons

[horns1,horns2,horns3,horns4] = divide 4 horns
[trumpets1,trumpets2,trumpets3] = divide 3 trumpets
[trombones1,trombones2,trombones3] = divide 3 trombones

basses = tutti doubleBass
timp = tutti timpani
[timp1,timp2] = divide 2 timp
cel = tutti celesta
vib = tutti vibraphone
hrp = harp
tub = tutti tuba

corAnglaises = tutti corAnglais



-- Dummy to get all orchestral parts in output
-- orch :: Music
-- orch = level mf $ flip doubleParts c $ orchParts



__HEXACHORDS__ :: ()
__HEXACHORDS__ = ()
{-
Harmonically and structurally, the piece is based on juxtaposition of Guido's hexachord (GH1).
We use all 12 transpositions, but c (GH1) and fs (GH2) play the role of starting point and ending point.
eb and a (GH3 and GH4) are used as a secondary group.

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
  Each block is a note indicating hexachord (GHn where 1 < n < 12), range, dynamics and orchestration.
-}


{-
We *only* use Guido's Hexachord and its truncation!

Here is a function to generate other hexachords using the same set of intervals:

allHexaChordTypes :: [[Pitch]]
allHexaChordTypes = id
  -- Lots of equals, due to the repeated 2 etc
  $ Data.List.nub

  $ fmap (fmap (relative c (spell usingSharps)). offsetPoints (c::Pitch))
  $ fmap (fmap $ spell usingSharps) $ Data.List.permutations [2,2,1,2,2::Semitones]

allHexaChordTypes' = timeSignature (6/4) $ compress 4 $ pseq $ concat $ (fmap.fmap) fromPitch'' allHexaChordTypes

-}



{-
TODO
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

-}
hexachord1 :: (IsPitch a, Transposable a) => [a]
hexachord1 = [c,d,e,f,g,a]

hexachord2 :: (IsPitch a, Transposable a) => [a]
hexachord2 = up _A4 [c,d,e,f,g,a]

-- All 4-element subsets of hexachord1
hexachord4Subsets =
  [[c,d,e,f],[c,d,e,g],[c,d,f,g],[c,e,f,g],[d,e,f,g],[c,d,e,a],[c,d,f,a],[c,e,f,a],
  [d,e,f,a],[c,d,g,a],[c,e,g,a],[d,e,g,a],[c,f,g,a],[d,f,g,a],[e,f,g,a]]
{-
>>> Data.List.nub $ fmap (Data.List.sort) $ filter (\x -> length x == 4) $ Data.List.subsequences [c,d,e,f,g,a::Pitch]
[[c,d,e,f],[c,d,e,g],[c,d,f,g],[c,e,f,g],[d,e,f,g],[c,d,e,a],[c,d,f,a],[c,e,f,a],[d,e,f,a],[c,d,g,a],[c,e,g,a],[d,e,g,a],[c,f,g,a],[d,f,g,a],[e,f,g,a]]
-}


{-

-- hexachord1 <> hexachord2 is the most dissonant thing we can do
-- Transposing either by some interval makes them *less* dissonant

-- very dissonant
both3 :: Music
both3   = set parts' violins (ppar hexachord1) <> set parts' violins (ppar hexachord2)
both3'  = set parts' horns (ppar hexachord1) <> set parts' trombones (ppar hexachord2)

-- quite dissonant
both1 :: Music
both1   = set parts' violins (ppar hexachord1) <> set parts' horns (ppar hexachord2)
both1'  = set parts' horns (ppar hexachord1)   <> set parts' violins (ppar hexachord2)

-- not so dissonant
both4 :: Music
both4   = set parts' flutes (ppar hexachord1) <> set parts' trombones (ppar hexachord2)
both4'  = set parts' flutes (ppar hexachord1) <> set parts' oboes (ppar hexachord2)

both4'' = (\x -> ((set parts' flutes . level ff . up _P8))x <> (set parts flutes . level ff)x) (ppar hexachord1)
  <> set parts' bassoons (ppar hexachord2)
-}

form :: Music
-- Basic, basic sketch (480 bars at (1/4) = 120)
form = ppar hexachord1|*5 |> (ppar hexachord1 <> ppar hexachord2)|*4 |> ppar hexachord2|*5



-- music :: Music
-- music = asScore $ stretchTo 480 (parts' .~ violins $ form) <> orch

{-
Voicings

How many voicings of a 6 note chord, assuming each PC can be vocied in 6 octaves?
a^n, where a is the number of octaves and n the number of PCs, in this case 46656

Generate all possible voicings of a 6-note chord into 2 and 3 octaves (64 and 729 respectively!)
>>> voicings' 2
>>> voicings' 3

voicings :: Integer -> [Pitch] -> [[Pitch]]
voicings n pitches = fmap (\(a,b,c,d,e,f) -> zipWith octavesUp (map pred [a,b,c,d,e,f]) pitches) $ combination6 n
voicings' n pitches = pseq $ fmap ppar $ fmap2 fromPitch'' (voicings n pitches)

-- What is the correct Math term?
combination6 n = [(a,b,c,d,e,f)
  | a <- [1..n]
  , b <- [1..n]
  , c <- [1..n]
  , d <- [1..n]
  , e <- [1..n]
  , f <- [1..n]
   ]
-}




__PRELIMINARIES__ :: ()
__PRELIMINARIES__ = ()

{-
Texture
  I want to have some freedom with this (particularly when it comes to melody!)
  But it would be good to have some "arrival points".
  Preferably very simple, but not just long notes, nor just JCA-style repetition.
- Automatic dovetailing etc
- Dynamic transitions! Don't stay statically in one texture (JCA-style!)
-}












{-
>>> :i Pattern
newtype Pattern a
instance Functor Pattern
instance Semigroup (Pattern a)
instance Transformable (Pattern a)
instance (IsPitch a, Reversible a) => IsPitch (Pattern a)
instance Monoid (Pattern a)

pureP      :: Reversible a => a           -> Pattern a
newPattern :: Reversible a => Voice a     -> Pattern a
mconcat    ::                 [Pattern a] -> Pattern a
spat       :: Reversible a => [Note a]    -> Pattern a
ppat       :: Reversible a => [Note a]    -> Pattern (Voice a)

renderPattern     :: (Reversible a, Splittable a) => Pattern a -> Span -> Score a
renderPatternsAbs :: (Reversible a, Splittable a) => Score (Pattern a) -> Score a
renderPatternsRel :: (Reversible a, Splittable a) => Score (Pattern a) -> Score a
-}



-- |
-- A behavior describing dynamics for the 2 main block voices.
-- *Generally* the first voice is used by strings and the second by winds.
--
dyns :: Behavior (Dynamics, Dynamics)
dyns = voiceToBehavior $ aligned 0 0 $
  [ pure (ppp,  ppp)
  , pure (pp,   pp)
  , pure (_p,   _p)
  , pure (mp,   mp)
  , pure (mf,   _p)
  , pure (mf,   _p)
  , pure (mp,   pp)
  , pure (_p,   pp)

  , pure (pp,   _p)
  , pure (_p,   pp)
  , pure (mf,   _p)
  , pure (_f,   mp)
  , pure (mf,   mf)
  , pure (pp,  _f)
  -- 14

  , pure (_f,   mf)
  , pure (mp,   _f)
  , pure (_f,   mf)
  , pure (_p,   _f)
  , pure (mf,   _f)
  , pure (ff,   fff)

  , pure (mf,   mf)
  , pure (_p,   _p)
  , pure (_p,   _p)
  , pure (ppp,  ppp)
  , pure (_p,   ppp)
  , pure (ppp,  ppp)

  ]^.voice


-- |
-- A behavior describing the bumber of subdivisions for the 2 main block voices.
--
beats :: Behavior Duration
beats = voiceToBehaviorLastDef 0 0 (1/4) $
  [ (pure (12/8))|*1
  , (pure (12/8))|*1
  , (pure (12/16))|*1
  , (pure (12/8))|*1
  , (pure (12/16))|*1
  , (pure (12/8))|*1
  , (pure (12/8))|*1
  , (pure (12/8))|*1

  , (pure (1/16))|*1
  , (pure (1/8))|*1
  , (pure (1/16))|*1
  , (pure (1/8))|*1
  , (pure (1/16))|*1
  , (pure (1/8))|*1

  , (pure (1/16))|*1
  , (pure (1/8))|*1
  , (pure (1/16))|*1
  , (pure (1/8))|*1
  , (pure (1/16))|*1
  , (pure (1/8))|*1

  , (pure (1/16))|*1
  , (pure (1/8))|*1
  , (pure (1/16))|*1
  , (pure (1/8))|*1
  , (pure (1/16))|*1
  , (pure (1/8))|*1

  ]^.voice
-- Watch out for the Monoid! We don't want to use the Duration instance, hence the use of ..LastDef



{-
TODO add more textures!
TODO think about doubling/leaving out notes when using choruses with a number of voices /= 6
TODO think about melodic patterns/sequentiation


Ideas:
    Interesting combinations (number of voice):
      low flutes+high oboes (6)
      high flutes + low oboes (6)
      high flutes and clarinets interleaved (6)
      low clarinets and bassoons interleaved (6)

      high flutes and clarinets, low oboes and bassoons (12)
      low flutes and clarinets, high oboes and bassoons (12) ??

      trombones (3)
      bassoons (3)
      bassoons+horns+bass cl (8)
      trumpets+trombones (6)
      horns+2 bassoons (6)

      horns+tuba+english horn (6)
      horns+tuba+english horn+bassoons (9)
      horns+tuba+english horn+bassoons+trombones (12)

      stopped horns, muted trumpets and trombones (10)


    Odd wind combination (sord trumpet, low cor anglais, low bassoon etc)

    Soloistic strings
    Big string chord, occassionally breaking out soloistic strings

  Tutti ideas

    Big cluster orchestrated in different ways (8 pitches in strings, 12+11 in winds etc)
      Timbral dissonance (dissonant notes in similar sounding parts)
      Timbral consonance (dissonant notes in different sounding parts)

  In general, think about solo vs. chamber vs. tutti orchestrations

  Also: something based on resonance, i.e. attack in strings sustained in horns or vice versa


----

TODO work out the basic melodies/subjects

TODO work the spacing sketches into the orchestral textures

TODO make a general plan for how to place these textures in the general timeline

TODO start working on the final short score
  Bass line vs top line (with local highs and lows)
  Technically:
    What number of staves (max 8)
    Which sequence of textures should we map to each stave?
    Alternatively: use a relatively small number of textures (about 16) and give each a single staff
      We can't, we need more!
    So how do we decide which textures are non-overlapping (i.e. they can be sequenced on a single SS stave?)



-}


-- Ensemble shortcuts

-- Oboe-souding, strong low-r vs. strong high-r
oboeClarinetDiv6IL  = [oboes1,oboes2,clarinets1,clarinets2,clarinets3,corAnglaises]
oboeFluteDiv6IL     = [oboes1,oboes2,flutes1,flutes2,flutes3,corAnglaises]

fluteClarinetDiv6   = [flutes1,flutes2,flutes3,clarinets1,clarinets2,clarinets3]
fluteClarinetDiv6IL = [flutes1,flutes2,clarinets1,flutes3,clarinets2,clarinets3]
fluteOboeDiv6       = [flutes1,flutes2,flutes3,oboes1,oboes2,corAnglaises]
fluteOboeDiv6IL     = [flutes1,flutes2,oboes1,flutes3,oboes2,corAnglaises]
fluteOboeDiv6IL'    = [flutes1,oboes1,flutes2,oboes2,flutes3,corAnglaises]

clarinetsDiv3 = [clarinets1,clarinets2,clarinets3]
bassoonsDiv3 = [bassoons1,bassoons2,bassoons3]

fluteOboeClarinetDiv9 = [flutes1,flutes2,flutes3,oboes1,oboes2,corAnglaises,clarinets1,clarinets2,clarinets3]
woodwindsDiv12 = [flutes1,flutes2,flutes3,oboes1,oboes2,corAnglaises,clarinets1,clarinets2,clarinets3,bassoons1,bassoons2,bassoons3]

woodwindsDiv12Loud = [
  flutes1,flutes2,clarinets1,flutes3,oboes1,oboes2,
  clarinets2,corAnglaises,clarinets3,bassoons1,bassoons2,bassoons3
  ]
woodwindsDiv12Soft = [
  oboes1,oboes2,flutes1,flutes2,flutes3,corAnglaises,
  clarinets1,clarinets2,bassoons1,clarinets3,bassoons2,bassoons3
  ]

trumpetsDiv3        = [trumpets1,trumpets2,trumpets3]
trombonesDiv3       = [trombones1,trombones2,trombones3]
trombsDiv6           = trumpetsDiv3 <> trombonesDiv3
trombsDiv5 = trumpetsDiv3 <> [trombones1,trombones2]
hornsBsnDiv6        = [horns1,horns2,bassoons1,bassoons2,horns3,horns4]
lowBrassDiv4        = trombonesDiv3 <> [tub]

hornsDiv4 = [horns1,horns2,horns3,horns4]


-- String divisions

[violins1_1, violins1_2] = divide 2 violins1
[violins2_1, violins2_2] = divide 2 violins2
[violas1,violas2] = divide 2 violas
[cellos1,cellos2] = divide 2 cellos
-- [cellos1a2,cellos2a2] = divide 2 cellos
[cellos1of4,cellos2of4,cellos3of4,cellos4of4] = divide 4 cellos


highStringsDiv4 = [violins1_1,violins1_2,violins2_1,violins2_2]
lowStringsDiv4 = [violas1,violas2,cellos1,cellos2] -- also consider cellos a4!

stringsDiv5        = [violins1,violins2,violas1,cellos1,doubleBasses]
stringsDiv6        = [violins1,violins2,violas1,cellos1,cellos2,doubleBasses]

stringsDiv5NoBass  = [violins1,violins2,violas1,cellos1,cellos2]
-- These are our workhourse 6 part string tuttis:
stringsDiv6NoBass    = [violins1,violins2,violas1,violas2,cellos1,cellos2]
stringsDiv6NoBassHi  = [violins1_1,violins1_2,violins2,violas,cellos1,cellos2]
stringsDiv6NoBassHi' = [violins1_1,violins1_2,violins2_1,violins2_2,violas,cellos1]  -- No cello II
stringsDiv6NoBassHi''= [violins1_1,violins1_2,violins2_1,violins2_2,violas1,violas2] -- No cello
stringsDiv6NoBassLo  = [violins2,violas,cellos1of4,cellos2of4,cellos3of4,cellos4of4] -- No violin I
stringsDiv6NoBassLo' = [violas1,violas2,cellos1of4,cellos2of4,cellos3of4,cellos4of4] -- No violins

-- vI vI vlaI vlaII vcI vcII
-- vI.1 vI.2 vII vla vcI vcII
-- vI.1 vI.2 vII vla vcI vcII
-- vI.1 vI.2 vII.1 vII.2 vla vcI

{-
18 wind instruments in upper-mid?
  3fl+2ob+3cl+2tr
-}
flutesDiv3 = [flutes1,flutes2,flutes3]
oboesAndCAs = [oboes1,oboes2,corAnglaises]
trumpDiv2 = [trumpets1,trumpets2]

stringsDiv8       = [violins1_1,violins1_2,violins2_1,violins2_2,violas,cellos1,cellos2,basses]
stringsDiv8NoBass = [violins1_1,violins1_2,violins2_1,violins2_2,violas1,violas2,cellos1,cellos2]


stringsDiv10       = [violins1_1,violins1_2,violins2_1,violins2_2,violas,cellos1of4,cellos2of4,cellos3of4,cellos4of4,basses]
stringsDiv10NoBass = [violins1_1,violins1_2,violins2_1,violins2_2,violas1,violas2,cellos1of4,cellos2of4,cellos3of4,cellos4of4]
-- Note: consider colour differences: low violins vs. high cellos, just using violas and basses a la Pathetique, etc

{-
-- 6 voices
oboeClarinetDiv6IL
oboeFluteDiv6IL
fluteClarinetDiv6
fluteClarinetDiv6IL
fluteOboeDiv6
fluteOboeDiv6IL
fluteOboeDiv6IL'
trombsDiv6
hornsBsnDiv6

stringsDiv6
stringsDiv6NoBass
-}




__TEXTURE__ :: ()
__TEXTURE__ = ()

------------------------------------------------------------------------------------------
{-
\f y xs -> zipWith f xs (repeat y)
  :: (a -> b -> c) -> b -> [a] -> [c]
The only way [a] can be converted to [c] is by (a -> c)
The only way to obtain (a -> c) is to flip (a -> b -> c) to get (b -> (a -> c)) and apply that to the b
I.e. this is equivalent to
\f y xs -> fmap ((flip f) y) xs
I.e.
  map (flip f y) xs === zipWith f xs (repeat y)
I.e.
  map (`f` y) xs === zipWith f xs (repeat y)
------
\f g a b c -> zipWith f a (zipWith g b c)
  :: (a -> b1 -> d) -> (b -> c -> b1) -> [a] -> [b] -> [c] -> [d]
\f g a b c -> zipWith3 (\a b c -> f a (g b c)) a b c
  :: (a -> b1 -> d) -> (b -> c -> b1) -> [a] -> [b] -> [c] -> [d]
I.e.
  zipWith3 (\a b c -> f a (g b c)) a b c == zipWith f a (zipWith g b c)
I.e.
  zipWith3 (\a b c -> f a (g b c)) a b c == zipWith f a (zipWith g b c)
-}



-- |
-- Build a pattern by applying the different transformations in different parts.
--
-- This is the canonical "phasing" effect, as a span/transformation can be used to
-- manipulate both (pattern-level) frequency and phase.
--
phasePattern :: (Aspects a, Functor f, Monoid (f a), Transformable (f a))
  => [Part] -> [Span] -> f a -> Behavior (f a)
phasePattern x ts pat = phasePatterns x ts (repeat pat)

phasePatterns :: (Aspects a, Functor f, Monoid (f a), Transformable (f a))
  => [Part] -> [Span] -> [f a] -> Behavior (f a)
phasePatterns x y z = pure $ mconcat $ zipWith3 j x y z
  where
    j thePart theTransf thePat = set (mapped . parts') thePart $ transform theTransf thePat

straightPattern x z = phasePattern x (repeat mempty) z
straightPatterns x z = phasePatterns x (repeat mempty) z

droneAndRestPattern :: Aspects a
  => [Part]
  -> [Span]
  -> [Note (Maybe a)]
  -> [Duration]
  -> Behavior (Pattern (Maybe a))
droneAndRestPattern x x' y z = phasePatterns x x' (zipWith (\p d -> spat [p |* d,rest]) y z)

{- How to dovetail?

Essence of DT: more than one instrument share a single repeated figure.
There are 2 ways: standard 2-part DT and Stravinskian 3-part DT

For simplicity, assume all notes in the pattern have the same duration and ignore rhytmical spelling of last note
in pattern (which would typically be changed to a large value with stacc to prevent an ugly buildup of rests afterwards).

## 2-part DT
If a figure has n notes (not counting final repeated)
  A: Play n+1 notes. Rest n-1 notes. Etc
  B: Same, delayed by n.

## 3-part DT
If a figure has 2n notes (not counting final repeated)
  A: Play n+1 (first bit), rest 2n-1, play n+1 (second bit), rest 2n-1 etc.
  B: Same, delayed by -2n.
  C: Same, delayed by 2n.
-}
doveTail2 :: Reversible a => Voice a -> [Pattern (Maybe a)]
doveTail2 xs =
  [ pat
  , delay (xs^.duration) pat
  ]
  where
    n = length (xs^.notes)
    playP = (^.voice) $ fmap2 Just $ take (n+1) (cycle (xs^.notes))
    restP = rest|*(xs^._tail.duration) -- or duration of xs - duration of (head x)
    pat = newPattern $ playP <> restP

{-
>>>
:o mcatMaybes $ flip renderPattern (0<->2) $ mconcat $ zipWith (set (mapped.parts')) fluteClarinetDiv6 $ doveTail2 (mconcat [c,d,e,d,e]^/16)

>>>
:o mcatMaybes $ flip renderPattern (0<->3) $ mconcat $ zipWith (set (mapped.parts')) fluteClarinetDiv6 $ doveTail3 (mconcat [c,d,e,f]^/16) (mconcat [g,f,e,d]^/16)-}

-- Assumes duration of xs and ys to be equal!
doveTail3 :: Reversible a => Voice a -> Voice a -> [Pattern (Maybe a)]
doveTail3 xs ys =
  [ pat
  , delay ((-2) *^ (xs^.duration + ys^.duration)) pat
  , delay (  2  *^(xs^.duration + ys^.duration)) pat
  ]
  where
    n = length (xs^.notes) -- or length ys

    playP1 = (^.voice) $ fmap2 Just $ take (n+1) $ (cycle $ (xs^.notes)<>(ys^.notes))
    restP1 = rest|*(sum $ fmap (^.duration) $ tail (ys^.notes) <> (xs^.notes))

    playP2 = (^.voice) $ fmap2 Just $ take (n+1) $ (cycle $ (ys^.notes)<>(xs^.notes))
    restP2 = rest|*(sum $ fmap (^.duration) $ tail (xs^.notes) <> (ys^.notes))

    pat = newPattern $ playP1 <> restP1 <> playP2 <> restP2


{-

------
Tangent: If dealing with a pitch material of n notes, an up-down pattern has length 2n-2.
This is because we "loose" a note at each turning point.
-}
----------


------------------------------------------------------------------------------------------
-- Woodwind
------------------------------------------------------------------------------------------

{-
Note: Textures should *generally* only use the mid-octave, as the incoming pitch material
is expeted to provide a suitable spacing.

Acceptable uses of octaves in the *patterns* is octave doubling, or transposing the *whole* texture upwards one
or two octaves. Case such as [c,d_,e,f',g] should be avoided as this arbitrarily changes the spacing of the
incoming pitches (which are supposed to be well-spaced already!).
-}

{-
Handy indentities:
  delay   a . stretch  b = transform (a     >-> b)
  delay   a . compress b = transform (a     >-> (1/b))
  undelay a . stretch  b = transform ((1/a) >-> b)
  undelay a . compress b = transform ((1/a) >-> (1/b))

-}

{-
TODO use Data.List functions to generate more insteresting patterns!
  I.e. start with something like hexachord1 and explore permutations, choose etc

    intersperse :: a -> [a] -> [a]
    >>> intersperse ',' "abcde" == "a,b,c,d,e"

    intercalate :: [a] -> [[a]] -> [a]
    transpose
    subsequences
    permutations
    \\
    union
    intersect

-}

{-
Based on rising fourhts in the tetrachord!
  c f
  d g
  e a

  g c
  a d

Other pattern based on fifths?
-}


risingFourthsInHC =
  [ (c,f)
  , (d,g)
  , (e,a)
  , (g,c)
  , (a,d)
  ]

-- getRisingFourth :: IsPitch a => Pitch -> a
-- getRisingFourth n = Data.Maybe.fromJust $ lookup n (cycle risingFourthsInHC)
risingFourthPair n = cycle risingFourthsInHC !! n

windAgile :: Aspects a => Behavior (Pattern (Maybe a))
windAgile = phasePatterns fluteOboeDiv6 y z
  where
    y = fmap (\r -> delaying (r/16) <> compressing 16) [0,0,0, 3,3,3]
    z = fmap spat $ fmap (up _P8)
        [ [c,f,f,f,rest]
        , [d,g,g,g,rest|*2]
        , [e,a,a,a,a,rest|*2]
        , [c,f,f,f,rest,c,f,f,rest]
        , [d,g,g,g,rest,d,g,g,rest|*2]
        , [e,a,a,a,rest,e,a,a,rest|*3]
        ]
{-
TODO use (g c) and (a d) as well?
-}


-- Not bad-looking, need DT!
windScales :: Aspects a => Behavior (Pattern (Maybe a))
windScales = phasePatterns fluteOboeClarinetDiv9 y z
  where
    y = fmap (\r -> (r/16) >-> (1/16)) [0,3,6, 1,4,7, 2,5,8]
    z = repeat $ spat $ palindr [c',d',e',f',g',a']
    -- TODO reverse order (or more sophisticated distribution!)

-- Good with 3-6
windScalesDT :: Aspects a => Int -> Behavior (Pattern (Maybe a))
windScalesDT n = phasePatterns (flutesDiv3<>oboesAndCAs<>trumpDiv2) y z
  where
    y = fmap (\r -> (r/16) >-> (1/16)) [0,0,3,3,6,6,1,1]
    z = cycle $ (\x -> doveTail2 (x^.voice)) $ palindr $ take n $ cycle [c',d',e',f',g',a']

    -- TODO reverse order (or more sophisticated distribution!)

cl2Pitches :: Aspects a => Behavior (Pattern (Maybe a))
cl2Pitches = pure $ mconcat $ zipWith (set (mapped . parts')) clarinetsDiv3
  $ doveTail2
  $ compress 16 $ mconcat $ mconcat $ replicate 2 [c,d,c,d]
cl2PitchesA3 :: Aspects a => Behavior (Pattern (Maybe a))
cl2PitchesA3 = pure $ mconcat $ zipWith (set (mapped . parts')) clarinetsDiv3
  $ (\x -> doveTail3 x x)
  $ compress 16 $ mconcat $ mconcat $ replicate 2 [c,d,c,d]

bsn2PitchesA3 :: Aspects a => Behavior (Pattern (Maybe a))
bsn2PitchesA3 = pure $ mconcat $ zipWith (set (mapped . parts')) bassoonsDiv3
  $ (\x -> doveTail3 x x)
  $ compress 16 $ mconcat $ mconcat $ replicate 2 [c,d,c,d]

-- Using different pattern freqs, a la melodien
-- TODO Think about timbre vs which parts is the fastest etc
-- TODO Connect certain parts in parallel/contrary motion etc
windScales2 :: Aspects a => Behavior (Pattern (Maybe a))
windScales2 = phasePatterns fluteClarinetDiv6 (zipWith (\r f -> (r >-> (1/f))) y z) m
  where
    y = [0,0,0, 0,0,0]
    z = cycle [24, 20, 16]
    m = cycle [p,p,p,p,p,p]

    p = spat (palindr [c,d,e,f,g,a,c',d',e',f',g',a',rest])
    q = p -- TODO inversion does not work:
    -- q = spat (palindr $ over pitches' (invertDiatonicallyP c') $ [c'|*4,d',e',d',e'] <> [rest])

-- TODO instrumentation!
-- Try 12 woodwind chorus!
windScales3 :: Aspects a => Behavior (Pattern (Maybe a))
windScales3 = phasePatterns woodwindsDiv12 (zipWith (\r f -> (r >-> (1/f))) y z) m
  where
    y = cycle [0,0,0]
    -- z = cycle [24,24,24, 20,20,20, 16,16,16, 12,12,12]
    z = cycle [8]
    m = cycle $ [p1,p2,p3,p1,p2,p3] <> [p1,p2,p3,p1,p2,p3]

    [p1,p2,p3] = doveTail3 (upP^.voice) (downP^.voice)
    (upP,downP) = palindr2 [c,d,e,f,g,a,c',d',e',f',g',a']

windMixtureLow :: Aspects a => Behavior (Pattern (Maybe a))
windMixtureLow = straightPatterns woodwindsDiv12Soft $ concat $ fmap (replicate 2) $ reverse [c,d,e,f,g,a]

windMixtureLowPhased :: Aspects a => Behavior (Pattern (Maybe a))
windMixtureLowPhased = fmap (compress 2) $ droneAndRestPattern woodwindsDiv12Soft (repeat mempty)
 (concat $ fmap (replicate 2) $ reverse [c,d,e,f,g,a]) (cycle [5,6,7,5.5,6.5,7.5,4,5,6,4.5,5.5,6.5])

-- -- TODO strange
-- windMixture :: Aspects a => Behavior (Pattern (Maybe a))
-- windMixture = straightPatterns woodwindsDiv12Loud $ reverse [c,d,e, f,g,a, c',d',e', f',g',a']

-- TODO

------------------------------------------------------------------------------------------
-- Brass
------------------------------------------------------------------------------------------

-- Related phasing motives

-- LOCKED (old version)
hornsPhased' :: Aspects a => Behavior (Pattern (Maybe a))
hornsPhased' = fmap (compress 4) $ phasePattern hornsDiv4 [0>->1,1>->2,8>->2,3>->4]
  $ spat [c |* 5,f|*3,e|*1,g_|*4,d|*3]

hornsPhased :: Aspects a => Behavior (Pattern (Maybe a))
hornsPhased = fmap (compress 4) $ phasePattern hornsDiv4 [0>->1,1>->2,8>->2,3>->4]
  $ spat [c |*5,f|*3,e|*1,g_|*4,d|*3,a|*3,rest]

hornsPhasedNoTransp :: Aspects a => Behavior (Pattern (Maybe a))
hornsPhasedNoTransp = fmap (compress 4) $ phasePattern hornsDiv4 [0>->1,1>->2,8>->2,3>->4]
  $ spat [c |*5,f|*3,e|*1,g_|*4,d|*3,a|*3,rest]

-- | Horn pedal on c (TODO generalize!).
-- Only uses horn 2 and 4.
hornsPedal :: Aspects a => Behavior (Pattern (Maybe a))
hornsPedal = straightPattern [horns2,horns4] $ stretch 8 $ spat [c]

hornsPedalSt :: (Aspects a, HasText a) => Behavior (Pattern (Maybe a))
hornsPedalSt = straightPattern [horns2,horns4] $ stretch 8 $ spat [fmap2 (addText "XX stopped") c]

-- Note can be augmented with pre-onset attacks, gliss etc
hornsChord :: Aspects a => Int -> Behavior (Pattern (Maybe a))
hornsChord n = droneAndRestPattern hornsDiv4 (repeat mempty)
  (stretch 4 $ reverse (cycle hexachord4Subsets !! n))
  (repeat 1)

-- Note: Doesn't use trombone 3 as that one generally has a bass function along with the tuba!
trombChord :: Aspects a => Int -> Behavior (Pattern (Maybe a))
trombChord n = droneAndRestPattern trombsDiv5 (repeat mempty)
  (stretch 4 $ reverse (cycle hexachord4Subsets !! n))
  (repeat 1)

-- Each number generates a different (1-st octave) voicing of the hexachord
hornsChords = (mconcat $ replicate 50 [
  (2,hornsChord 9),
  (4, hornsChord 8)])^.behaviors

trombChords = (mconcat $ replicate 50 [
  (2,trombChord 9),
  (4, trombChord 8)])^.behaviors
-- TODO horn simple drones stopped (chord or lows only)
-- TODO Brass rhytmised horn pedals (off-beat rhythm, a la Thaikovsky: Pathetique 4th movement)




trumpetsPhased :: Aspects a => Behavior (Pattern (Maybe a))
trumpetsPhased = trumpetsOrTrombonesPhased True

trombonesPhased :: Aspects a => Behavior (Pattern (Maybe a))
trombonesPhased = trumpetsOrTrombonesPhased False

trumpetsOrTrombonesPhased :: Aspects a => Bool -> Behavior (Pattern (Maybe a))
trumpetsOrTrombonesPhased tptsOrTbns = fmap (compress 4) $ phasePattern par [0>->1,1>->1,8>->2] pat
  where
    par = if tptsOrTbns then trumpetsDiv3 else trombonesDiv3
    pat = newPattern $ (if tptsOrTbns then id else down _P8) $ trombPattern

trombPattern = [(1,c')^.note,((1/2),c')^.note,((1/16),c')^.note,((1/16),c')^.note,((1/16),c')^.note,((1/16),c')^.note,
      ((1/16),c')^.note,((1/16),c')^.note,((1/8),g)^.note,((1/16),c')^.note,((1/16),c')^.note,((1/8),g)^.note,
      ((1/16),d')^.note,((1/16),d')^.note,((1/8),g)^.note,((1/16),d')^.note,((1/16),d')^.note,((1/8),g)^.note,
      ((1/16),a)^.note,((1/16),a)^.note,((1/16),g)^.note,((1/16),g)^.note,rest]^.voice

lowBrassDrones4 :: Aspects a => Behavior (Pattern (Maybe a))
lowBrassDrones4 = droneAndRestPattern lowBrassDiv4 (repeat mempty) y z
  where
    y = [f,e,d,c]
    z = [5,3,4,5] -- TODO correct?

bassoonDrones3 :: Aspects a => Behavior (Pattern (Maybe a))
bassoonDrones3 = droneAndRestPattern (divide 3 bassoons) (repeat mempty) y z
  where
    y = [e,d,c]
    z = [3,4,5] -- TODO correct?


-- Trumpets long notes (a la Melodien)
-- Trumpets upward fanfare-like motion (a la Muliebris)
-- Trumpets playing short notes
-- Trumpets mp as part of medium-loud texture

------------------------------------------------------------------------------------------
-- Percussion, harp and celesta
------------------------------------------------------------------------------------------

hrpGliss :: Aspects a => Behavior (Pattern a)
hrpGliss = pure $ set (mapped . parts') hrp $ compress 32 $ spat $ fmap fromPitch'' $ palindr $ enumDiatonicFromTo c_ c''

-- TODO hrp high phased harmonics (2-3 of them, simple!!)

hrpBroken :: Aspects a => Behavior (Pattern a)
hrpBroken = pure $ set (mapped . parts') hrp $ compress 8 $ spat [d_,f_,a_,c,e,g,e,c,a_,f_]
-- TODO bad spacing! see also celBroken

hrp2Pitches :: Aspects a => Behavior (Pattern a)
hrp2Pitches = pure $ set (mapped . parts') hrp $ compress 16 $ fmap (octavesUp 1) $ spat [c,d]


hrpBisb :: Aspects a => Behavior (Pattern a)
hrpBisb = pure $ set (mapped . parts') hrp $ compress 16 $ fmap (octavesUp 1) $ spat [c]

vibBroken :: Aspects a => Behavior (Pattern a)
vibBroken = pure $ set (mapped . parts') vib $ compress 8 $ fmap (octavesUp 1) $ spat [d_,f_,a_,c,e,g]
-- TODO bad spacing!

vib2Pitches :: Aspects a => Behavior (Pattern a)
vib2Pitches = pure $ set (mapped . parts') vib $ compress 16 $ fmap (octavesUp 1) $ spat [c,d]

-- TODO add
crot = tutti glockenspiel
crot2Pitches :: Aspects a => Behavior (Pattern a)
crot2Pitches = pure $ set (mapped . parts') crot $ compress 16 $ fmap (octavesUp 1) $ spat [c,d]

{-
celBroken :: Aspects a => Behavior (Pattern a)
celBroken = pure $ set (mapped . parts') cel $ compress 8 $ fmap (octavesUp 1) $ spat [d_,f_,a_,c,e,g]
-- TODO bad spacing!

cel2Pitches :: Aspects a => Behavior (Pattern a)
cel2Pitches = pure $ set (mapped . parts') cel $ compress 16 $ fmap (octavesUp 1) $ spat [c,d]
-}

-- TODO "free" tremolando
-- TODO unison harp melody (a la Mahler 9 opening)
-- TODO harp non-linear rising motive, and retrogrades (c e d f e g ...)

-- TODO timp?
-- Timp alternating through the 6 pitches (3 or 4 at a time)
-- Timp playing 2 pitches
-- Timp tremolo on 1 pitch

timpaniRoll :: Aspects a => Behavior (Pattern a)
timpaniRoll = pure $ set (mapped . parts') timp $ spat [fmap (tremolo 2) c]

------------------------------------------------------------------------------------------
-- Strings
------------------------------------------------------------------------------------------

stringArp :: Aspects a => Int -> Behavior (Pattern (Maybe a))
stringArp m = straightPatterns (reverse stringsDiv6NoBass)
  $ fmap spat $ fmap (\n -> take m $ drop n $ stringArpPitches) [0..]

stringArpIn8 :: Aspects a => Int -> Behavior (Pattern (Maybe a))
stringArpIn8 m = straightPatterns (reverse stringsDiv8NoBass)
  $ fmap spat $ fmap (\n -> take m $ drop n $ stringArpPitches) [0..]

stringArpPitches = [a_,c,d,e,f,g,a,c',d',e',f']

stringSustain = straightPatterns stringsDiv6NoBass [a,g,f,e,d,c]

-- LOCKED
stringDrones :: Aspects a => Behavior (Pattern (Maybe a))
stringDrones = droneAndRestPattern stringsDiv6NoBass (repeat mempty) y z
  where
    y = [a,g,f,e,d,c]
    z = [3,4,5,3,4,5]

stringDronesMixed :: Aspects a => Behavior (Pattern (Maybe a))
stringDronesMixed = droneAndRestPattern stringsDiv6NoBass (repeat mempty) y z
  where
    y = [a,g,f,e,d,c]
    z = [3/2,4/2,5/2,3,4,5]

stringDronesMixed3 :: Aspects a => Behavior (Pattern (Maybe a))
stringDronesMixed3 = droneAndRestPattern stringsDiv6NoBass (repeat mempty) y z
  where
    y = [a,g,f,e,d,c]
    z = [3/3,4/3,5/3,(6/3),(7/3),(8/3)]

-- TODO placeholder
doubleBassDrones :: Aspects a => Behavior (Pattern (Maybe a))
doubleBassDrones = pure $ mconcat $ zipWith (set (mapped.parts')) (divide 6 doubleBasses) (reverse [c,d,e,f,g,a])

-- TODO string scales
-- TODO strings accomp figures
{-
  Accomps:
    2-note repeat (c c a a)
    Alberti
    Bach C maj
    Style brise
    Arp up and down (compare strindberg!)

-}

-- TODO strings leading note accomp figures

-- TODO more variants of the flourish idea!

-- TODO other parts as well
-- TODO other pitches
stringsStrindberg4 :: Aspects a => [Part] -> Behavior (Pattern (Maybe a))
stringsStrindberg4 ps = fmap (compress 16) $ straightPatterns ps
  (repeat $ spat $ (\x->x<>reverse x)[g_,e,c',a'])

stringsStrindberg3 :: Aspects a => [Part] -> Behavior (Pattern (Maybe a))
stringsStrindberg3 ps = fmap (compress 12) $ straightPatterns ps
  (repeat $ spat $ (\x->x<>reverse x)[g_,e,c',a'])

stringDronesWithFlourish4 :: Aspects a => Behavior (Pattern (Maybe a))
stringDronesWithFlourish4 = stringDronesWithFlourish' 1 0

stringDronesWithFlourish8 :: Aspects a => Behavior (Pattern (Maybe a))
stringDronesWithFlourish8 = stringDronesWithFlourish' 2 0

stringDronesWithFlourish16 :: Aspects a => Behavior (Pattern (Maybe a))
stringDronesWithFlourish16 = stringDronesWithFlourish' 4 0

stringDronesWithFlourishBetter4 = stringDronesWithFlourish' 1 (-1/4)
stringDronesWithFlourishBetter8 = stringDronesWithFlourish' 2 (-1/4)
stringDronesWithFlourishBetter16 = stringDronesWithFlourish' 4 (-1/4)

stringDronesWithFlourish' :: Aspects a => Int -> Duration -> Behavior (Pattern (Maybe a))
stringDronesWithFlourish' compr extraDur = straightPatterns x y
  where
    x = stringsDiv6NoBass
    y =
      [ spat $ [ a|*(3+extraDur)]   <> (compress (fromIntegral compr) $ concat $ replicate compr $ flourish 1)
      , spat $ [ g|*(4+extraDur)]   <> (compress (fromIntegral compr) $ concat $ replicate compr $ flourish 2)
      , spat $ [ f|*(5+extraDur)]   <> (compress (fromIntegral compr) $ concat $ replicate compr $ flourish 3)

      , spat $ [ e|*(3.5+extraDur)] <> (compress (fromIntegral compr) $ concat $ replicate compr $ flourish 4)
      , spat $ [ d|*(4.5+extraDur)] <> (compress (fromIntegral compr) $ concat $ replicate compr $ flourish 5)
      , spat $ [ c|*(5.5+extraDur)] <> (compress (fromIntegral compr) $ concat $ replicate compr $ flourish 6)
      ]

flourish :: (AdditiveGroup a, Fractional (Scalar a), IsPitch a, VectorSpace a) => Int -> [a]
flourish 1 = [g,f,g,a]^/4     <> [c',d',c',a]^/4
flourish 2 = [f,e,f,g]^/4     <> [a,c',a,g]^/4
flourish 3 = [e,d,e,f]^/4     <> [g,a,g,f]^/4
flourish 4 = [d,c,d,e]^/4     <> [f,g,f,e]^/4
flourish 5 = [c,a_,c,d]^/4    <> [e,f,e,d]^/4
flourish 6 = [a_,g_,a_,c]^/4  <> [d,e,d,c]^/4


-- LOCKED
stringsLargeTuplets :: Aspects a => Duration -> Behavior (Pattern (Maybe a))
stringsLargeTuplets n = fmap (compress n) $ straightPatterns x y
  where
    x = stringsDiv6NoBass
    y = fmap spat [ [g,a], [e,f], [c,d], [c,d], [c_,d_], [c_,d_] ]

-- LOCKED!
-- Works with just high or low strings
-- Use filters if only one is desired
stringTrem :: Aspects a => Behavior (Pattern (Maybe a))
stringTrem = phasePatterns x y z
  where
    x = stringsDiv10
    y = fmap (\r -> ((r*2.5)>->2.5)) $ cycle [0..5]
    z = replicate 5 (fmap (tremolo 3) $ spat mel) <> replicate 5 (fmap (tremolo 3 . down _P8) $ spat mel)
    mel = reverse hexachord1


-- TODO many, many more, see above!








{-
Remember:
  (1/20) is a bar
  (1/2)  is 10 bars

-}

------
-- Combinations etc.
------

percAndCl2Pitches = vib2Pitches <> fmap (stretch 2) hrp2Pitches <> cl2Pitches



patterns :: Aspects a => Int -> Behavior (Pattern (Maybe a))
patterns pn = fixDynamics2 pn $ case pn of
      -- 0 -> pure (stretch 4 $ newPattern (lutoslawTrillToStacc <> rest^/4))

      -- Keep these empty to contain the sketches
      0  -> mempty
      1  -> mempty
      2  -> mempty
      3  -> mempty

      ------------------------------------------------------------------------------------------
      -- Main subjects
      4  ->   [ (7.5,   mempty)
              , (1,     mainSubWinds)
              , (1.5,   mainSubWinds2)
              -- 7.5+1+1.5 = 10
              , (6,     mempty)
              , (5,     mainSubWindTutti)
              , (5,     mainSubWindTutti2)
              ]^.behaviors
              -- 6+5+5 = 16
      5  ->   [ (19,    mainSubStrings)
              , (1,     mainSubStringsLoud)
              , (6,     mainSubStrings)
              ]^.behaviors
        -- TODO vary!


      -- Percussion staff/2-note pattern
      6  ->   [ (5,     mempty)
              , (2,     fmap (stretch 2) hrp2Pitches <> vib2Pitches {-<> cl2PitchesA3-}) -- add clarinet?
              , (2+1/4, hrp2Pitches <> fmap (stretch 2) vib2Pitches)
              , (3/4,   bsn2PitchesA3)
              -- 10
              , (4, mempty)
              -- 14
              , (2,     hrpBisb <> fmap (stretch 2) crot2Pitches)
              , (10,    fmap (stretch 2) hrp2Pitches <> vib2Pitches) -- ff vib (hard mallets!)
              ]^.behaviors
        -- TODO vary!

      -- Extra trumpet staff
      7  ->   [ (26,    fmap (stretch 4) trumpetsPhased)]^.behaviors
      -- TODO


      -- Timp etc
      8  -> timpaniRoll

      -- DO NOT USE
      9  -> mempty

      ------------------------------------------------------------------------------------------
      -- TODO use differnet instrument combination for agile
      -- TODO final mixture has pitches in reverse order
      10 ->   [ (4,     mempty)
              , (1/2,   windAgile)
              , (1/2,   windAgile)
              , (1/2,   windAgile)
              , (1/2,   windAgile)
              -- 6
              , (1/4,   windScalesDT 3)
              , (1/4,   windScalesDT 5)
              , (1/4,   windScalesDT 6)
              -- 6+3/4
              , (1/4,   windScalesDT 4)
              , (1/4,   windScalesDT 6)
              , (1/4,   mempty)
              , (1/2,   mempty)
              -- 8
              , (15,    windAgile)
              , (3,     windMixtureLowPhased)
              ]^.behaviors
      11 -> windScales2 -- TODO

      ------------------------------------------------------------------------------------------
      12 ->   [ (2,     mempty)
              , (4,     hornsPhasedNoTransp) -- TODO slower rhythm!
              , (3,     mempty)
              -- 9
              -- TODO try phasing in 3!
              , (0.5,   hornsChords)
              , (3.5,   hornsPhasedNoTransp)
              , (2,     trombChords)
              -- 15
              , (4,     hornsPhasedNoTransp)
              , (2,     fmap2 (down _P8) hornsChords <> trombChords)
              -- 21
              , (45,    hornsPhasedNoTransp) -- TODO slower rhythm!
              ]^.behaviors

      13 ->   [ (6,     bassoonDrones3), -- TODO basssoon + clarinet
                (20,    lowBrassDrones4) ]^.behaviors

      ------------------------------------------------------------------------------------------
      -- TODO add flourish, replace with moving texture etc
      14 ->   [ (2,     stringDronesMixed)
              , (2,     stringDronesMixed3)
              , (1.5,   stringSustain)
              -- 5.5

              , (1,     stringDronesWithFlourishBetter4)
              , (1,     stringDronesWithFlourishBetter8)
              , (1.25,  stringDronesWithFlourishBetter8)
              , (1.25,  stringDronesWithFlourishBetter4)

                        -- TODO what to do here?
                        -- back to drones?
                        -- introduce stringberg figure (briefly?)
                        -- trills?
                        -- quicker version of drones?
                        -- scales?
              --10
              , (1,     stringTrem)
              , (1,     stringsStrindberg4 [violins2,violas,cellos1]) -- or flourish here (again)
              , (1,     stringsStrindberg3 [violas])
              , (1,     mempty)

              , (3.5,     stringDronesWithFlourishBetter16)
              --17.5
              , (0.25,    fmap (compress 8) (stringArpIn8 3))
              , (0.25,    fmap (compress 8) (stringArpIn8 3))
              , (0.25,    fmap (compress 8) (stringArpIn8 4))
              , (0.25,    fmap (compress 8) (stringArpIn8 4))
              , (0.25,    fmap (compress 8) (stringArpIn8 5))
              , (0.25,    fmap (compress 8) (stringArpIn8 6))

              -- 19
              , (1.5,    mempty) -- string subject!
              , (3,      stringDrones) -- TODO replace
              , (2.5,    mempty) -- string subject!
              ]^.behaviors
      15 ->   [ (26, doubleBassDrones) -- TODO
              ]^.behaviors
      ------------------------------------------------------------------------------------------

      -- n -> error $ "patterns: Unknown fake part " ++ show n
      n -> mempty

stringD = fst
windD   = snd
fixDynamics2 pn x =  fmap ([
  stringD,stringD,windD,windD,
  windD,stringD,windD,windD,
  stringD,stringD,windD,windD,
  windD,windD,stringD,stringD
  ] !! pn) (undelay 0.5 dyns) >>= flip fmap2 x . level



export4 :: Aspects a => Int -> Behavior (Pattern (Maybe a))
export4 pn = fixDynamics pn $ case pn of
      0 -> lutoslaw <> hrp2Pitches <> hornsPhased' <> (fmap2 (softer 2) stringDrones)
      -- n -> error $ "patterns: Unknown fake part " ++ show n
      n -> mempty

export3 :: Aspects a => Int -> Behavior (Pattern (Maybe a))
export3 pn = fixDynamics pn $ case pn of
      4 -> lutoslaw <> (fmap2 (softer 2) stringDrones)
      n -> mempty

export2 :: Aspects a => Int -> Behavior (Pattern (Maybe a))
export2 pn = fixDynamics pn $ case pn of
      8 -> stringTrem
      n -> mempty

export1 :: Aspects a => Int -> Behavior (Pattern (Maybe a))
export1 pn = fixDynamics pn $ case pn of
      7 ->
        [ (2  , stringDrones)
        , (2.5, (fmap (compress 2) stringDronesWithFlourish4))
        , (2.5, (fmap (compress 4) stringDronesWithFlourish16))
        , (2  , stringsLargeTuplets 3)
        ]^.behaviors
      8 -> hornsPhased
      n -> mempty

fixDynamics pn x =  fmap ([fst,fst,snd,snd, snd,fst,snd,snd, fst,fst,snd,snd, snd,snd,fst,fst] !! pn) (undelay 0.5 dyns) >>= flip fmap2 x . level








--------
-- Miscellaneous
--------

-- rivers :: Aspects a => Behavior (Pattern (Maybe a))
-- rivers = pure $ mconcat $ zipWith3 j x y z
--   where
--     j thePart thePhase thePat = set (mapped . parts') thePart $ transform ((thePhase*2.5)>->2.5) $ thePat
--     x = [trumpets1,corAnglaises,horns1]
--     y = [0,1,2, 3,3,3]
--     z = [ newPattern (river1 <> (rest|*3))
--         , newPattern (river2 <> (rest|*4))
--         , newPattern (river3 <> (rest|*5))
--         ]
--
-- -- Melodic material
-- -- "River" subjects from LMJB
-- river1 :: IsPitch t => Voice t
-- river1 = [((1/8),c)^.note,((1/8),d)^.note,((1/4),e)^.note,((1/2),g)^.note,(1,g)^.note]^.voice
-- river2 = [((1/8),d)^.note,((1/8),e)^.note,((1/4),c)^.note,((1/2),a_)^.note,(1,a_)^.note]^.voice
-- river3 = [((1/8),a_)^.note,((1/8),c)^.note,((1/4),d)^.note,((1/2),e)^.note,(1,e)^.note]^.voice
--
-- lutoslaw
lutoslaw :: Aspects a => Behavior (Pattern (Maybe a))
lutoslaw = fmap (compress 4) $ phasePattern x [0>->1,0.25>->1,5>->2,6>->2] pat
  where
    x = [oboes1,clarinets1,oboes2,clarinets2,clarinets3]
    pat = newPattern (up _P8 $ lutoslawFalling <> rest|*3)

-- Misc imported voices

-- Just f,e, can be replaced by [d,c], [e,d], [g,f] or [a,g]
lutoslawTrillToStacc :: IsPitch a => Voice a
lutoslawTrillToStacc =
  [((1/32),f)^.note,((1/32),e)^.note,((1/32),f)^.note,((1/32),e)^.note,((1/32),f)^.note,((1/32),e)^.note,
  ((1/32),f)^.note,((1/32),e)^.note,((1/32),f)^.note,((1/32),e)^.note,((1/32),f)^.note,((1/32),e)^.note,
  ((1/16),f)^.note,((1/16),f)^.note,((1/16),f)^.note,((1/16),f)^.note,((1/16),f)^.note,((1/16),f)^.note]^.voice

lutoslawFalling :: IsPitch a => Voice a
lutoslawFalling =
  [((1/2),d)^.note,((1/8),d)^.note,((1/32),d)^.note,((1/32),e)^.note,((1/32),fs)^.note,((1/32),d)^.note,((1/4),cs)^.note,
  ((1/4),cs)^.note]^.voice


-- TODO the main subject (f e c d)
mainSubject1 :: IsPitch a => Voice a
mainSubject1 = mconcat [f',e',c',d']
mainSubject2 = mconcat [a,a,g,g]
mainSubject3 = mconcat [d,d,c,c]
mainSubject4 = mconcat [g_,g_,a_,a_]
mainSubject5 = mconcat [c_,c_,d_,d_]
mainSubject6 = mconcat [e__,f__,g__,f__]

mainSubjects = [mainSubject1,mainSubject2,mainSubject3,mainSubject4,mainSubject5,mainSubject6]

-- | Given 6 parts, pattern duration and and padding (before scaling), return main subject presentation.
mainSub :: Aspects a => [Part] -> Behavior (Pattern (Maybe a))
mainSub x = pure $ stretch (5/4) $ (mconcat $ fmap newPattern $ zipWith (set parts') x (fmap (<> rest) mainSubjects))

mainSubPatterns = fmap newPattern $ (fmap (<> rest) mainSubjects)

mainSub' :: Aspects a => [[Part]] -> Behavior (Pattern (Maybe a))
mainSub' pss = pure $ stretch (5/4) $ (mconcat $ zipWith doublePartsF pss $ mainSubPatterns)

mainSub'' :: Aspects a => [[(Part,Int)]] -> Behavior (Pattern (Maybe a))
mainSub'' pss = pure $ stretch (5/4) $ (mconcat $ zipWith doublePartsInOctaveF pss $ mainSubPatterns)

mainSubWinds   = mainSub'' $ fmap pure $ zip oboeFluteDiv6IL (repeat 1)
mainSubWinds2  = mainSub'' $ fmap pure $ zip fluteClarinetDiv6IL (repeat 1)
mainSubTroms   = mainSub'' $ fmap pure $ zip trombsDiv6 (repeat 0)
mainSubHrnBsn  = mainSub'' $ fmap pure $ zip ([corAnglaises]<>hornsDiv4<>[bassoons3]) [-1,-1,-1,0,0,0]

mainSubStrings = mainSub stringsDiv6NoBass -- TODO many alternatives, see above!
mainSubStringsLoud = mainSub'' $ fmap pure $ [(p1,1),(p2,0),(p3,0),(p4,0),(p5,0),(p6,-1)]
  where
    [p1,p2,p3,p4,p5,p6] = stringsDiv6
mainSubWindTutti = mainSub''
  [[(flutes1,1),(flutes2,1),(flutes3,1),(oboes1,0),(oboes2,0),(clarinets1,1),(clarinets2,1),(clarinets3,1),(trumpets1,0)],
   [(horns1,0),(horns3,0)],
   [(horns2,0),(horns4,0)],
   [(bassoons1,0)],
   [(bassoons2,0)],
   [(bassoons3,0)]
   ]
-- mainSubWindTutti2 = mainSub'' [[(flutes1,1),(oboes1,0),    (trumpets1,0)],
--                         [(flutes2,1),(oboes2,0),    (trumpets2,0)],
--                         [(flutes3,1),(clarinets1,0),(trumpets3,0)],
--
--                         [(corAnglaises,1),(trombones1,0)],
--                         [(clarinets2,1),  (trombones2,0)],
--                         [(clarinets3,1),  (trombones3,0),(tub,0)]
--                         ]

-- TODO fix this
mainSubWindTutti2 = mainSub'' [[(flutes1,1),(oboes1,0)],
                        [(flutes2,1),(oboes2,0)],
                        [(flutes3,1),(clarinets1,0)],

                        [(corAnglaises,1){-,(horns1,0)-}],
                        [(clarinets2,1){-, (horns2,0)-}],
                        [(clarinets3,1)]
                        ]





__UTIL__ :: ()
__UTIL__ = ()

run :: String -> IO ()
run q = makeScoreFromSketchJust q >>= openMusicXml

runR :: String -> IO ()
runR q = makeScoreFromSketchJust q >>= writeMusicXml "/Users/hans/Dropbox/Studio/Interludes/Export/test2.xml"

runL :: IO ()
runL = makeScoreFromSketch >>= openLilypond

runD :: IO ()
runD = makeScoreFromSketch >>= openDiagram'

runA :: IO ()
runA = makeScoreFromSketch >>= openAudacity . upwardCompressor _p 0.4

runS :: IO ()
runS = makeScoreFromSketch >>= writeMidi "/Users/hans/Dropbox/Studio/Interludes/Export/test2.mid"

runCS = makeScoreFromSketch >>= putStrLn . md5 . show

testP :: Behavior (Pattern (Maybe StandardNote)) -> IO ()
testP x = openLilypond $ asScore $ mcatMaybes $ flip renderPattern (0<->17) $ (! 0) $ x

testP' x = asScore $ mcatMaybes $ flip renderPattern (0<->4) $ (! 0) $ x




makeScoreFromSketchJust "winds"   = fmap (mfilter (\x -> isWoodwindInstr (x^.part._instrument))) makeScoreFromSketch
makeScoreFromSketchJust "brass"   = fmap (mfilter (\x -> isBrassInstr (x^.part._instrument))) makeScoreFromSketch
makeScoreFromSketchJust "strings" = fmap (mfilter (\x -> isStringInstr (x^.part._instrument))) makeScoreFromSketch

makeScoreFromSketchJust "fl+ob"    = fmap (mfilter (\x -> (x^.part._instrument) `elem` [flute,oboe])) makeScoreFromSketch
makeScoreFromSketchJust "fl+ob+kl" = fmap (mfilter (\x -> (x^.part._instrument) `elem` [flute,oboe,clarinet])) makeScoreFromSketch

makeScoreFromSketchJust "hrp" = fmap (mfilter (\x -> (x^.part._instrument) `elem` [harp^._instrument])) makeScoreFromSketch

makeScoreFromSketchJust ""        = makeScoreFromSketch
makeScoreFromSketchJust _         = error "Unknown query, try \"\""




__MECHANICS__ :: ()
__MECHANICS__ = ()

-- doTrace = Debug.Trace.trace
doTrace = flip const

patterns' :: Part -> Span -> Pattern (Maybe StandardNote)
patterns' p s =
  doTrace ("Fetching pattern for sketch part '" ++ show (fakePartToInt p) ++ "' in bars " ++ show (toDouble (s^.onset)))
    $ ((patterns (fakePartToInt p))^.from behavior) (s^.onset)

-- The fake parts (each representing a line in the harmony sketch)
fakeParts :: [Part]
fakeParts = [ (!! 0) $ divide 8 violins
            , (!! 0) $ divide 8 cellos
            , (!! 1) $ divide 8 violins
            , (!! 1) $ divide 8 cellos
            , (!! 2) $ divide 8 violins
            , (!! 2) $ divide 8 cellos
            , (!! 3) $ divide 8 violins
            , (!! 3) $ divide 8 cellos
            , (!! 4) $ divide 8 violins
            , (!! 4) $ divide 8 cellos
            , (!! 5) $ divide 8 violins
            , (!! 5) $ divide 8 cellos
            , (!! 6) $ divide 8 violins
            , (!! 6) $ divide 8 cellos
            , (!! 7) $ divide 8 violins
            , (!! 7) $ divide 8 cellos ]

fakePartToInt :: Part -> Int
fakePartToInt p = Data.Maybe.fromMaybe 0 $ Data.List.findIndex (== p) fakeParts

intToFakePart :: Int -> Part
intToFakePart = (fakeParts !!)

-- | Get all minims from the harmony sketch.
captureChords :: IO (Score ([] Pitch))
captureChords = do
  s <- captureSibelius'
  return
    $ simultaneous
    $ toChords
    $ filterOnsetDurationDivisibleBy (1/4) s

-- | Get all minims and their parts from the harmony sketch.
captureChordsWithParts :: IO [(Part, Score ([] Pitch))]
captureChordsWithParts = do
  s <- captureSibelius'
  return
    $ (mapped._2) %~ simultaneous
    $ (mapped.mapped)  %~ toChords
    $ extractPartsWithInfo
    $ filterOnsetDurationDivisibleBy (1/4)
    $ (s::Music)

showChords :: IO ()
showChords =
  captureChordsWithParts
    >>= pure . ppar
      . zipWith (set parts') stringsDiv10
      . fmap (fromChords . snd)
    >>= openLilypond

-- | Load the harmony sketch and use patterns' to assemble the full score.
makeScoreFromSketch :: IO Music
makeScoreFromSketch = do
  cs' <- captureChordsWithParts
  let cs = fmap2 (filterWithSpan (\s _ -> (s^.onset) >= start && (s^.offset) <= stop)) cs'
  res <- return
    -- $ (|> orch) -- FIXME notes out of range
    $ ppar
    $ fmap snd
    $ fmap2 (mcatMaybes . renderPatternsAbs)
    $ stretch 20
    $ fmap (fmap2 (uncurry applyChordPitchesWithOctavesAndRests2) . addPatterns)
    $ cs
  -- printNotesOutOfRange res
  return res
    where
      addPatterns :: (Part, Score a) -> (Part, Score (a, Pattern (Maybe StandardNote)))
      addPatterns = (\(p,x) -> (p, mapWithSpan (\s x -> (x, patterns' p s)) x))

      start = 0
      stop  = 27










-- hornsPhased = pure $ compress 4 $ mconcat $ zipWith j x y
--   where
--     j = set (mapped . parts')
--     x = hornsDiv4
--     y = zipWith transform [0>->1,1>->2,8>->2,3>->4] (repeat pat)
--     pat = spat [c|*5,f|*3,e|*1,g_|*4,d|*3]


-- ccf71f40b1b97266202d99544bc39b64
