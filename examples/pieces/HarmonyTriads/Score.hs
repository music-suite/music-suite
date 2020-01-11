
module Main where

import Music.Prelude
import Util

{-
Triadic harmony

Leading notes against root, third and fifth
Call these -I, +I, -III, +III, -V, +V (leading up to or coming down to)
All exist in major and minor forms (M2 vs m2)

Outline the piece as a succession of triadic chords with 2 (or more/less) LN


>>> Data.List.permutations ["-i","-I","+_iii","+III"]
(24)
[["-i","-I","+_iii","+III"],["-I","-i","+_iii","+III"],["+_iii","-I","-i","+III"],["-I","+_iii","-i","+III"],
["+_iii","-i","-I","+III"],["-i","+_iii","-I","+III"],["+III","+_iii","-I","-i"],["+_iii","+III","-I","-i"],
["+_iii","-I","+III","-i"],["+III","-I","+_iii","-i"],["-I","+III","+_iii","-i"],["-I","+_iii","+III","-i"],
["+III","-i","-I","+_iii"],["-i","+III","-I","+_iii"],["-i","-I","+III","+_iii"],["+III","-I","-i","+_iii"],
["-I","+III","-i","+_iii"],["-I","-i","+III","+_iii"],["+III","-i","+_iii","-I"],["-i","+III","+_iii","-I"],
["-i","+_iii","+III","-I"],["+III","+_iii","-i","-I"],["+_iii","+III","-i","-I"],["+_iii","-i","+III","-I"]]


-- Using all 24 combinations  of -i -I +_iii +III along with all 12 PCs gives us
>>> length $ liftA2 (,) [1..24] [1..12]
288

-- Using it with both major and minor triads gives us
>>> length $ liftA2 (,) [1..24] [1..24]
576

-}


-- returns a voice of duration 1
type Degree = Int
_i   = 0
_iii = 1
_v   = 2
type Direction = Bool
fromBelow = True
fromAbove = False

-- |
-- Construct a two-note voice of duration one, consisting of (last) a note in the given chord
-- and (first) a step-wise approach to the resolving note.
--
-- @ >>> leadingNote [c,e,g] fromBelow _i MajorMode
-- [((1/2),bb_)^.note,((1/2),c)^.note]^.voice
-- 
-- >>> leadingNote [c,e,g] fromBelow _i MinorMode
-- [((1/2),b_)^.note,((1/2),c)^.note]^.voice
-- 
-- >>> leadingNote [e,g,b] fromBelow _iii MinorMode
-- [((1/2),fs)^.note,((1/2),g)^.note]^.voice
-- 
-- >>> leadingNote [e,g,b] fromAbove _v MinorMode
-- [((1/2),c')^.note,((1/2),b)^.note]^.voice
--
leadingNote :: Chord Pitch -> Direction -> Degree -> Mode -> Voice Pitch
leadingNote chord direction degree mode = stretchTo 1 $ pure nonChordNote <> pure chordNote
  where
    chordNote = (chord !! (degree `mod` length chord))
    leadingInterval
      | mode == MajorMode = _M2
      | mode == MinorMode = m2
    nonChordNote
      | direction == fromAbove = chordNote .+^ leadingInterval
      | direction == fromBelow = chordNote .-^ leadingInterval

leadingNote' :: Chord Pitch -> Direction -> Degree -> Mode -> Music
leadingNote' c n u m = fmap fromPitch'' . renderAlignedVoice . aligned 0 0 $ leadingNote c n u m

{-
>>> leadingNote (triad c) fromBelow v MajorMode
[((1/2),a)^.note,((1/2),g)^.note]^.voice

>>> :o leadingNote' (triad c) fromAbove _iii MinorMode
>>> :o leadingNote' (triad c) fromAbove _iii MajorMode

>>> :o leadingNote' (triad c) fromBelow i MinorMode
>>> :o leadingNote' (triad c) fromBelow i MajorMode
-}

type Resolution = (Direction, Degree, Mode)
reses :: [Resolution]
reses =
  [ (fromAbove,_iii,MinorMode)
  , (fromAbove,_iii,MajorMode)
  , (fromBelow,_i,MinorMode)
  , (fromBelow,_i,MajorMode)
  ]

type Cadenze = [Resolution]

{-
>>> :di fmap (leadingNoteFromResolution c) cad1

-}
cad1 = [ (fromBelow,_i,MinorMode)
       , (fromAbove,_iii,MinorMode)
       , (fromAbove,_v,MinorMode)]
cad2 = [ (fromBelow,_i,MajorMode)
       , (fromAbove,_iii,MinorMode)
       , (fromAbove,_v,MinorMode)]
cad3 = [ (fromBelow,_i,MajorMode)
       , (fromAbove,_iii,MinorMode)
       , (fromAbove,_v,MajorMode)]


leadingNoteFromResolution :: Pitch -> Resolution -> Voice Pitch
leadingNoteFromResolution z (di,de,m) = leadingNote (triad z) di de m

leadingNoteFromResolution' :: Pitch -> Resolution -> Music
leadingNoteFromResolution' z (dir,degree,mode) = leadingNote' (triad z) dir degree mode


-- All possible groups of 2 Resolutions
resPairs :: [(Resolution,Resolution)]
resPairs = over (mapped.both) (reses !!) $ liftA2 (,) [0..3] [0..3]

resPairs' :: [Music]
resPairs' = compress 2 $ fmap (uncurry (|>)) $ over (mapped.both) (leadingNoteFromResolution' c) resPairs

resPairScore :: Music
resPairScore = pseq $ compress 2 $ fmap (uncurry (|>)) $ over (mapped.both) (leadingNoteFromResolution' c) resPairs

-- Realisation of all resPairs
foo :: [Music]
foo =  zipWith (<<>) (repeat $ fmap fromPitch'' $ ppar $ fmap pure $ triad c) (up _P8 resPairs')

indexFoo :: Int -> Music
indexFoo n = cycle foo !! n

{-
>>> :pl (!! 7) $ zipWith (<<>) (repeat $ fmap fromPitch'' $ ppar $ fmap pure $ triad c) (up _P8 resPairs')

View all resolutions
>>> :o pseq $ fmap indexFoo [1..16]

-}

chordWithResolutionPairs =
  [ (c, 5)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (c, 2)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (c, 2)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (c, 2)
  , (eb,4)
  , (a, 5)
  ]

allChordWithResolutionPairs = liftA2 (,) [c::Behavior Pitch,cs,d,ds,e,f,fs,g,gs,a,as,b] resPairs
-- >>> length $ permutations $ allChordWithResolutionPairs
-- fact 192 -- enormous!


chordWithResolutionPairs' = showCWRP chordWithResolutionPairs
showCWRP x = pseq $ fmap (\(root,res2) -> up (root .-. asPitch c) $ indexFoo res2) x



music = c














-- Old test version
chordWithResolutionPairsOLD1 =
  [ (c, 5)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (c, 2)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (c, 2)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (a, 5)
  , (eb,4)
  , (c, 2)
  , (eb,4)
  , (a, 5)
  ]



