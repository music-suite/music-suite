{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, ConstraintKinds, FlexibleContexts, FlexibleInstances, TypeFamilies, DeriveFunctor,
  GeneralizedNewtypeDeriving, ViewPatterns, MultiParamTypeClasses, RankNTypes, ConstraintKinds, StandaloneDeriving,
  DeriveTraversable, DeriveDataTypeable, TupleSections #-}

{-
Voice combinators (create basic voices, combine, transform)
  Monoid
  Time and pitch tranformations
  Notably: split/chunks/mconcat

  Infinite voice
  Sequence, parallel and random
    Maybe create a big tree of "alternative" and traverse in various ways to get seq/par structure
-}
import Music.Prelude
import qualified Music.Score.Pitch
import Data.AffineSpace
import Music.Time.Internal.Util (rotate)
import Data.Tree(Tree)
import qualified Data.Tree
import qualified Data.List
import Control.Lens (Iso, Lens', lens, element, iso, _1, _2, preview, each, _head, _last, nullOf)





type Rhythm = Voice ()
type Melody = Voice (Maybe Pitch)
type Accompaniment = Score Pitch

rhythm = durationsAsVoice



-- Some melodies
fiskeskar :: Melody
fiskeskar = [((1/8),Just d)^.note,((1/8),Just a)^.note,((1/8),Just a)^.note,((1/8),Just g)^.note,((1/8),Just a)^.note,((1/8),Just f)^.note,((1/4),Just e)^.note,((1/8),Just e)^.note,((1/8),Just g)^.note,((1/8),Just a)^.note,((1/8),Just f)^.note,((1/8),Just e)^.note,((1/16),Just e)^.note,((1/16),Just f)^.note,((1/4),Just d)^.note,((1/8),Just d)^.note,((1/8),Just a)^.note,((1/8),Just a)^.note,((1/8),Just g)^.note,((1/8),Just a)^.note,((1/8),Just f)^.note,((1/4),Just e)^.note,((1/8),Just e)^.note,((1/8),Just g)^.note,((1/8),Just a)^.note,((1/8),Just f)^.note,((1/8),Just e)^.note,((1/16),Just e)^.note,((1/16),Just f)^.note,((1/4),Just d)^.note,((1/8),Just f)^.note,((1/8),Just d)^.note,((1/8),Just e)^.note,((1/8),Just f)^.note,((1/8),Just g)^.note,((1/8),Just f)^.note,((1/4),Just e)^.note,((1/8),Just f)^.note,((1/8),Just d)^.note,((1/8),Just e)^.note,((1/8),Just f)^.note,((1/8),Just g)^.note,((1/32),Just g)^.note,((1/32),Just f)^.note,((1/32),Just g)^.note,((1/32),Just f)^.note,((1/4),Just e)^.note,((1/8),Just e)^.note,((1/8),Just g)^.note,((1/8),Just a)^.note,((1/8),Just f)^.note,((1/4),Just e)^.note,((1/4),Just d)^.note]^.voice

silent :: Melody
silent     = [((3/16),Just g)^.note,((1/16),Just a)^.note,((1/8),Just g)^.note,((3/8),Just e)^.note,((3/16),Just g)^.note,((1/16),Just a)^.note,((1/8),Just g)^.note,((3/8),Just e)^.note,((1/4),Just d')^.note,((1/8),Just d')^.note,((3/8),Just b)^.note,((1/4),Just c')^.note,((1/8),Just c')^.note,((3/8),Just g)^.note,((1/4),Just a)^.note,((1/8),Just a)^.note,((3/16),Just c')^.note,((1/16),Just b)^.note,((1/8),Just a)^.note,((3/16),Just g)^.note,((1/16),Just a)^.note,((1/8),Just g)^.note,((3/8),Just e)^.note,((1/4),Just a)^.note,((1/8),Just a)^.note,((3/16),Just c')^.note,((1/16),Just b)^.note,((1/8),Just a)^.note,((3/16),Just g)^.note,((1/16),Just a)^.note,((1/8),Just g)^.note,((3/8),Just e)^.note,((1/4),Just d')^.note,((1/8),Just d')^.note,((3/16),Just f')^.note,((1/16),Just d')^.note,((1/8),Just b)^.note,((3/8),Just c')^.note,((3/8),Just e')^.note,((3/16),Just c')^.note,((1/16),Just g)^.note,((1/8),Just e)^.note,((3/16),Just g)^.note,((1/16),Just f)^.note,((1/8),Just d)^.note,((3/4),Just c)^.note]^.voice

silentBass :: Melody
silentBass = [((3/4),Just c_)^.note,((3/4),Just c_)^.note,((3/4),Just g_)^.note,((3/4),Just c_)^.note,((3/4),Just f__)^.note,((3/4),Just c_)^.note,((3/4),Just f__)^.note,((3/4),Just c_)^.note,((3/4),Just g_)^.note,((3/4),Just c_)^.note,((3/4),Just g_)^.note,((3/4),Just c_)^.note]^.voice

-- Just repeats with a strange offset (as we begin and end on tonic)
-- Example of why melodies as sequences doesn't really make sense (we can't just move the pickup to the position of the final tonic)
fs1 = beginning 20 $ mconcat $ stitchTogether fiskeskar
-- Split off part at beginning, transform before stitching back together
fs2 = (retr $ beginning 2.5 $ fiskeskar) `stitch` fiskeskar
-- Expand chunks, gradually increasing
fs3 = simplifyPitches $ beginning 40 $ mconcat $ zipWith (\n -> over pitches (relative d  (^*n))) [1..] (chunks 4.25 $ cycleV fiskeskar)
fs4 = simplifyPitches $ beginning 40 $ mconcat $ zipWith (\n -> over pitches (relative a  (^*n))) [1..] (chunks 4.25 $ cycleV fiskeskar)
fs5 = simplifyPitches $ beginning 40 $ mconcat $ zipWith (\n -> over pitches (relative f  (^*n))) [1..] (chunks 4.25 $ cycleV fiskeskar)
fs6 = simplifyPitches $ beginning 40 $ mconcat $ zipWith (\n -> over pitches (relative fs (^*n))) [1..] (chunks 4.25 $ cycleV fiskeskar)

-- Expand into itself at various points
fs7  = expandInto (4+3/8) fiskeskar (fiskeskar)
fs8  = expandInto (5+2/4) fiskeskar (fiskeskar|/3) -- mad!
fs9  = expandInto (2+1/4) fiskeskar (between (2/4) (5/4) fiskeskar)
fs10 = expandInto (5+1/4) fiskeskar (between (0/4) (3/4) fiskeskar)

music :: Music
music =
  (silentNight |* (4/3)) |> fiskeskarChorale
  where
  silentNight = fmap (fmap fromPitch) $
    renderAlignedVoice (aligned 0 0 silent)
    <>
    renderAlignedVoice (aligned 0 0 silentBass)
  fiskeskarChorale = rcat $ fmap (fmap $ fmap fromPitch)
    $ fmap (renderAlignedVoice . aligned 0 0) [ fs1, fs2, fs3, fs4, fs5, fs6, fs7, fs8, fs9, fs10 ]

-- TODO FIXME add?
-- See $splitSemantics in TODO.md

-- TODO bad?
instance HasDuration a => HasDuration (Maybe a) where
  _duration Nothing = error "No duration"
  _duration (Just x) = _duration x

{-
All rather chaotic! Stitch is interesting, but not that intuitive!
How does join happen? Pitches are aligned, but durations are not
-}


interleave :: Voice a -> Voice a -> Voice a
interleave v1 v2 = ((v1^.notes) `merge` (v2^.notes))^.voice
  where
    merge [] xs = []
    merge xs [] = []
    merge (x:xs) (y:ys) = x:y:merge xs ys

ausfaltung :: Interval -> Melody -> Melody
ausfaltung i v = interleave v (up i v)

-- Uses of stichTogether
v1 :: Melody
v1 =
  simplifyPitches $ 
  (!! 4) $ -- generations grow quickly
  (stitchTogether $ octavesDown 2 [c,d,fs,b_,cs]^.voice |/4)



{-
TODO generalization of Pattern, new Scales/Chords and the harmonic Fields found in previous sketches
-}
type Points a = ([Diff a], a, Diff a, [Diff a])



{-
>>> [_M2,_M2,_M2]^.vectorsPoints  (c::Pitch)
[d,e,fs]

>>> [d,e,fs]^.pointsVectors (c::Pitch)
[_M2,_M2,_M2]

>>> [1,2,2]^.vectorsPoints  (2::Time)
[3,5,7]

>>> [3,5,7]^.pointsVectors (2::Time)
[1,2,2]
-}
vectorsPoints :: AffineSpace a => a -> Iso [Diff a] [Diff a] [a] [a]
vectorsPoints o = iso (points o) (vectors o)
  where
    points o  = tail . offsetPoints o
    vectors o = tail . pointOffsets o

pointsVectors :: AffineSpace a => a -> Iso [a] [a] [Diff a] [Diff a]
pointsVectors o = from (vectorsPoints o)


type Mel2 = (Pitch, Voice Interval)

-- http://people.bu.edu/jyust/mcm09_revised.pdf

-- Trees!
-- Notion of "stress" points that can be moved around or affixed to a bar hierharchy
type MelT = Tree Interval

{-
Represent melody as a tree of aligned voices phrases/groups etc.
When "rendered", each phrase is aligned to some grid, offset to avoid overlaps.
I.e. it becomes possible to cut out notes.
  (More general operations here: cut notes with stretch etc)
-}
-- (((((eb d) d)
--    ((eb d) d))
--    ((eb d) (d bb2)3))
--  ((((bb a) g)
--    ((g  f) eb))
--   (((eb d) (c c2)3))))
-- ==>
-- (((((-m2)P1)m2
--    ((-m2)P1))m2
--    ((-m2)P1(M6)))P1
--  ((((-m2)-M2)P1
--    ((-M2)-m2))P1
--   (((-m2)-M2(P1)))))
--
--
-- (1   (2 3))
-- ==>
--    1 1 (1)
--
-- ((0 1) (2 3))
-- ==>
--   (1) 1 (1)
--
--
-- ((1 2) (((3 4) (5 6)) (7 (8 9))))
--  ( (1) 1 ( ((1)1(1))1     (1(1))))
-- renderMelT :: MelT -> Melody
-- renderMelT ()


--  Schenkerian stuff

-- Contours

-- Overlapping scales (i.e. diatonic/chromatic) with inflictions
-- TODO proper scale support!

-- Phrases, question/answer etc
















-- Melody as vector
melodicAmbitus :: (HasPitch' a, Music.Score.Pitch.Pitch a ~ Pitch) => Voice a -> Ambitus Interval Pitch
melodicAmbitus ys =
  let xs = fmap (^.pitch) ys in case (headV xs, lastV xs) of
  (Just a, Just b) -> Ambitus (a `min` b) (a `max` b)
  _                -> error "melodicAmbitus: Empty voice"










-- Les Miz encodings

-- 3. The Docks
llRh1  = [2,2,     2,2,     1,1,1,1, 4]^.rhythm |/ 16
llRh2  = [1,1,1,1, 1,1,1,1, 1,1,1,1, 4]^.rhythm |/ 16
llPat1 = [0,5,0,5,0,1,2,4,5]
llPat2 = [0,1,2,3,0,1,2,3,0,1,2,4,5]
llPitches1 = [c',bb,a,  g, g,g]
llPitches2 = [c',bb,ab, g, f,eb]
llPitches3 = [c',db',c',db',c',db']

-- Repeat twice
ll :: Melody
ll =  mconcat $ fmap (\(a,b,c) -> mel a b c) [
  (llRh1, llPat1, llPitches1),
  (llRh2, llPat2, llPitches1),
  (llRh1, llPat1, llPitches2),
  (llRh2, llPat2, llPitches2),
  (llRh1, llPat1, llPitches3)
  ]











stitchTogether :: (HasPitches' a, Transposable a) => Voice a -> [Voice a]
stitchTogether = Data.List.unfoldr (\v -> let v2 = stitch v v in Just (v2,v2))
{-
stitchItselfN :: Transposable a => Int -> Voice a -> Voice a
stitchItselfN 0 v = v
stitchItselfN n v = stitch v (stitchItselfN (n - 1) v)

-- TODO blocks
stitchItself :: Transposable a => Voice a -> Voice a
stitchItself v = stitch v (stitchItself v)
-}

-- subj = [c,eb,d,g,fs]



main = defaultMain music


type VS a = [Placed (Voice a)]

-- Render each phrase at the first time that does not overlap with a previous phrase
-- renderVS :: [Time] -> VS a -> Placed (Voice a)

-- renderVS _ []      = mempty
-- renderVS [] _      = mempty
-- renderVS (t:ts) (v:vs) = delay (t.-.0) v

{-
  Functor, Applicative, Monad
  Monoid
  Transformable
  HasDuration
  TODO HasMeta
-}
nullV           :: Voice a -> Bool
headV           :: Voice a -> Maybe a             -- Bad?
lastV           :: Voice a -> Maybe a             -- Bad?
cycleV          :: Voice a -> Voice a
takeV           :: Int     -> Voice a -> Voice a
dropV           :: Int     -> Voice a -> Voice a
rotateR         :: Int     -> Rhythm  -> Rhythm
melodyAsRhythm  :: Lens' Melody Rhythm

cycleV          = over notes cycle
nullV           = nullOf notes
headV           = preview (notes._head.from note._2)
lastV           = preview (notes._last.from note._2)
takeV n         = over notes (take n)
dropV n         = over notes (drop n)
rotateR n       = over notes (rotate n)
setR            = zipVoiceWith' (const) (flip const)
getR            = fmap (const ())
melodyAsRhythm  = lens getR (flip setR)

rotateRhythm :: Int -> Melody -> Melody
rotateRhythm n = over melodyAsRhythm (rotateR n)

transposeSingleNote :: Int -> Interval -> Melody -> Melody
transposeSingleNote n i = over (notes . element n) (up i)

retr :: Voice a -> Voice a
retr = over notes reverse

{-
Surprisingly effective for a "stupid" combinator.
-}
addLeading :: Interval -> Melody -> Melody
addLeading i = over notes (>>= \n -> [0.75*|n,0.25*|down i n])
addLeadingD :: Int -> Melody -> Melody
addLeadingD i = over notes (>>= \n -> [0.75*|n,0.25*|downDiatonic c (fromIntegral i) n])


{-
  Variants of intersperse/intercalate (related to phrasewise model as per above)
-}

-- Expand notes/adjacent notes to longer sequences
expandInto :: (HasPitches' a, HasDuration a, Transposable a, Splittable a, Transformable a) => Duration -> Voice a -> Voice a -> Voice a
expandInto d v w = va `stitch` w `stitch` vb
  where
    (va,vb) = split d v

between a b = beginning (b-a) . ending a


-- TODO actually add these
instance HasMeta a => HasMeta (Aligned a) where
  meta = undefined
instance HasMeta (Voice a) where
  meta = undefined




mel :: Rhythm -> [Int] -> [Pitch] -> Melody
mel rh ns ps = fmap (\(i,_) -> Just $ ps!!(ns!!i)) $ withIndexV rh
-- TODO

withIndexV :: Voice a -> Voice (Int, a)
withIndexV v = fmap swap $ zipVoiceNoScale v (fmap pure [0..]^.voice)
  where swap (a,b) = (b,a)
