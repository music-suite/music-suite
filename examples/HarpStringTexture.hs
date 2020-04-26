{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

import Music.Prelude
import qualified Music.Score.Articulation as S
import qualified Music.Score.Pitch as S
import qualified Music.Score.Part as S

-- TODO this is a fork of examples/piano2.hs. Minimize!
main =
  -- TODO workaround for the fact that the REPL can only show
  -- the top-level definition.

  -- defaultMain music
  defaultMain $
    renderPattern
      (stringArpIn8 20 <> hrpGliss)
      (0 <-> 40)

music :: Music
music =
  fmap Just $ renderPatternsAbs
    $ fmap renderSimple
    $ topLevelScore

-- TODO add info
--  Harmony
--  Texture
--  Random seed?
--  Time (for indexing a global Behavior)?
data Block
  = Block
      { col :: Col,
        range :: Range,
        texture :: Texture
      }

data Col = Blue | Brown

data Range = Hi | Lo

data Texture = Chord | Repeat

topLevelScore :: Score Block
topLevelScore =
  mempty
    |> stretch
      3.5
      ( ( ( mempty
              |> (pure (Block Brown Hi Chord))
              |> (pure (Block Blue Hi Chord) |* 2)
          )
            <> ( delay 2
                   . stretch 2
               )
              ( mempty
                  |> (pure (Block Brown Lo Chord))
                  |> ( pure
                         (Block Blue Lo Chord)
                         |* 2
                     )
              )
        )
          <> delay
            4
            ( ( pure
                  (Block Brown Hi Repeat)
                  |> (pure (Block Blue Hi Repeat) |* 2)
              )
                <> ( delay 2
                       . stretch 2
                   )
                  (pure (Block Brown Lo Repeat) |> (pure (Block Blue Lo Repeat) |* 0.5))
            )
      )
    |> stretch
      2
      ( ( ( mempty
              |> (pure (Block Brown Hi Chord))
              |> (pure (Block Blue Hi Chord) |* 2)
          )
            <> ( delay 2
                   . stretch 2
               )
              ( mempty
                  |> (pure (Block Brown Lo Chord))
                  |> ( pure
                         (Block Blue Lo Chord)
                         |* 2
                     )
              )
        )
          <> delay
            4
            ( ( pure
                  (Block Brown Hi Repeat)
                  |> (pure (Block Blue Hi Repeat) |* 2)
              )
                <> ( delay 2
                       . stretch 2
                   )
                  (pure (Block Brown Lo Repeat) |> (pure (Block Blue Lo Repeat) |* 0.5))
            )
      )
    |> stretch
      5.5
      ( ( ( mempty
              |> (pure (Block Brown Hi Chord))
              |> (pure (Block Blue Hi Chord) |* 2)
          )
            <> ( delay 2
                   . stretch 2
               )
              ( mempty
                  |> (pure (Block Brown Lo Chord))
                  |> ( pure
                         (Block Blue Lo Chord)
                         |* 2
                     )
              )
        )
          <> delay
            4
            ( ( pure
                  (Block Brown Hi Repeat)
                  |> (pure (Block Blue Hi Repeat) |* 2)
              )
                <> ( delay 2
                       . stretch 2
                   )
                  (pure (Block Brown Lo Repeat) |> (pure (Block Blue Lo Repeat) |* 0.5))
            )
      )
    |> stretch
      3
      ( ( ( mempty
              |> (pure (Block Brown Hi Chord))
              |> (pure (Block Blue Hi Chord) |* 2)
          )
            <> ( delay 2
                   . stretch 2
               )
              ( mempty
                  |> (pure (Block Brown Lo Chord))
                  |> ( pure
                         (Block Blue Lo Chord)
                         |* 2
                     )
              )
        )
          <> delay
            4
            ( ( pure
                  (Block Brown Hi Repeat)
                  |> (pure (Block Blue Hi Repeat) |* 2)
              )
                <> ( delay 2
                       . stretch 2
                   )
                  (pure (Block Brown Lo Repeat) |> (pure (Block Blue Lo Repeat) |* 0.5))
            )
      )

-- |
-- Alternative to 'render' just to get a sense of 'topLevelScore'.
renderSimple ::
  (IsPitch a, Transposable a, HasArticulations' a, Articulated (S.Articulation a)) =>
  Block ->
  Pattern a
renderSimple Block {col, range, texture} =
  stretch 20 $
    let h =
          case range of
            Hi -> up _P15
            Lo -> id
        i =
          case texture of
            Chord -> accentAll
            Repeat -> id
     in h $ i $ case col of
          Blue -> g
          Brown -> c

render ::
  (IsPitch a, Transposable a, HasArticulations' a) => Block -> Pattern a
render Block {col, range, texture = Chord} = case col of
  Blue ->
    mconcat [c_, a_, fs]
  Brown ->
    mconcat [g_, d, a]
render Block {col, range, texture = Repeat} =
  let transp =
        case range of
          Hi -> up _P8
          Lo -> id
   in transp $
        case col of
          Blue ->
            newPattern ([a |* 3, d, e]^.voice) |/ 8
          Brown ->
            newPattern ([e, fs |* 2]^.voice) |/ 6

p1 :: IsPitch a => Pattern a
p1 = newPattern ([a |* 3, d, e]^.voice) |/ 8

p1_1 :: IsPitch a => Pattern a
p1_1 = newPattern ([f, e, f, d, e, c, d |* 2, d |* 2]^.voice) |/ 8

p1_1i :: (IsPitch a, HasPitches' a, S.Pitch a ~ Pitch) => Pattern a
p1_1i = invertDiatonic e p1_1

p2 :: IsPitch a => Pattern a
p2 = newPattern ([e, fs |* 2]^.voice) |/ 3

p4 :: IsPitch a => Pattern a
p4 = newPattern $ view voice $ [a, g, d, e] |/ 8

p4_0 :: IsPitch a => Pattern a
p4_0 = p4 <> delay (1 / 4) p4

p4_1 :: IsPitch a => Pattern a
p4_1 = newPattern $ view voice $ [a, g, d, e, b_, c, d, e] |/ 8

p4_11 :: IsPitch a => Pattern a
p4_11 = newPattern $ view voice $ [b, a, b, c', b, a, d', c', b, a, c', b, a, g, a, b, a, b, c', b] |/ 16

p4_2 :: IsPitch a => Pattern a
p4_2 = newPattern $ view voice $ [f, g, a, b, a, b, c', d', e', d', c', b] |/ 16

p5 :: IsPitch a => Pattern a
p5 = newPattern $ view voice $ [c__, g__, e_, c, e_, g__] |/ 12

p5_1 :: IsPitch a => Pattern a
p5_1 = newPattern $ view voice $ [c__, g__, c_, e_, e, c, g_, c_] |/ 16

type Aspects a = (IsPitch a, HasParts' a, S.Part a ~ Part, Transposable a)

windAgile :: (Aspects a) => Pattern (Maybe a)
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

lutoslaw :: (Aspects a, Transformable a) => Pattern (Maybe a)
lutoslaw = fmap (compress 4) $ phasePattern x [0>->1,0.25>->1,5>->2,6>->2] pat
  where
    x = [oboes1,clarinets1,oboes2,clarinets2,clarinets3]
    pat = newPattern (up _P8 $ lutoslawFalling <> rest|*3)

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



hrpGliss :: Aspects a => (Pattern a)
hrpGliss = set (mapped . parts') hrp $ compress 32 $ spat $ fmap fromPitch $ palindr $ enumDiatonicFromTo c_ c''

-- TODO hrp high phased harmonics (2-3 of them, simple!!)

hrpBroken :: Aspects a => (Pattern a)
hrpBroken = set (mapped . parts') hrp $ compress 8 $ spat [d_,f_,a_,c,e,g,e,c,a_,f_]
-- TODO bad spacing! see also celBroken

hrp2Pitches :: Aspects a => (Pattern a)
hrp2Pitches = set (mapped . parts') hrp $ compress 16 $ fmap (octavesUp 1) $ spat [c,d]


hrpBisb :: Aspects a => (Pattern a)
hrpBisb = set (mapped . parts') hrp $ compress 16 $ fmap (octavesUp 1) $ spat [c]

vibBroken :: Aspects a => (Pattern a)
vibBroken = set (mapped . parts') vib $ compress 8 $ fmap (octavesUp 1) $ spat [d_,f_,a_,c,e,g]
-- TODO bad spacing!

vib2Pitches :: Aspects a => (Pattern a)
vib2Pitches = set (mapped . parts') vib $ compress 16 $ fmap (octavesUp 1) $ spat [c,d]

-- TODO add crotales to library
crot = tutti glockenspiel

crot2Pitches :: Aspects a => (Pattern a)
crot2Pitches = set (mapped . parts') crot $ compress 16 $ fmap (octavesUp 1) $ spat [c,d]

stringArp :: Aspects a => Int -> (Pattern (Maybe a))
stringArp m = straightPatterns (reverse stringsDiv6NoBass)
  $ fmap spat $ fmap (\n -> take m $ drop n $ stringArpPitches) ([0..] :: [Int])

stringArpIn8 :: Aspects a => Int -> (Pattern (Maybe a))
stringArpIn8 m = straightPatterns (reverse stringsDiv8NoBass)
  $ fmap spat $ fmap (\n -> take m $ drop n $ stringArpPitches) [0..]

stringArpPitches :: IsPitch a => [Note a]
stringArpPitches = [a_,c,d,e,f,g,a,c',d',e',f']

stringSustain :: Aspects a => Pattern a
stringSustain = straightPatterns stringsDiv6NoBass [a,g,f,e,d,c]


phasePattern :: (Aspects a, Monoid a, Transformable a, S.Part a ~ Part)
  => [Part] -> [Span] -> a -> a
phasePattern ps s pat = phasePatterns ps s (repeat pat)

-- |
-- Build a pattern by applying the different transformations in different parts.
--
-- This is the canonical "phasing" effect, as a span/transformation can be used to
-- manipulate both (pattern-level) frequency and phase.
--
phasePatterns :: (Aspects a, Monoid (a), Transformable (a))
  => [Part] -> [Span] -> [a] -> a
phasePatterns x y z = mconcat $ zipWith3 j x y z
  where
    j p s pat = set (parts') p $ transform s pat

straightPattern :: (Aspects a, Monoid (a), Transformable (a))
  => [Part] -> a -> a
straightPattern x z = phasePattern x (repeat mempty) z

straightPatterns :: (Aspects a, Monoid (a), Transformable (a))
  => [Part] -> [a] -> a
straightPatterns x z = phasePatterns x (repeat mempty) z


fluteOboeDiv6 :: [Part]
fluteOboeDiv6       = [flutes1,flutes2,flutes3,oboes1,oboes2,corAnglaises]

oboeClarinetDiv6IL :: [Part]
oboeClarinetDiv6IL  = [oboes1,oboes2,clarinets1,clarinets2,clarinets3,corAnglaises]

oboeFluteDiv6IL :: [Part]
oboeFluteDiv6IL     = [oboes1,oboes2,flutes1,flutes2,flutes3,corAnglaises]

-- fluteClarinetDiv6   = [flutes1,flutes2,flutes3,clarinets1,clarinets2,clarinets3]
-- fluteClarinetDiv6IL = [flutes1,flutes2,clarinets1,flutes3,clarinets2,clarinets3]
-- fluteOboeDiv6IL     = [flutes1,flutes2,oboes1,flutes3,oboes2,corAnglaises]
-- fluteOboeDiv6IL'    = [flutes1,oboes1,flutes2,oboes2,flutes3,corAnglaises]

-- clarinetsDiv3 = [clarinets1,clarinets2,clarinets3]
-- bassoonsDiv3 = [bassoons1,bassoons2,bassoons3]

-- fluteOboeClarinetDiv9 = [flutes1,flutes2,flutes3,oboes1,oboes2,corAnglaises,clarinets1,clarinets2,clarinets3]
-- woodwindsDiv12 = [flutes1,flutes2,flutes3,oboes1,oboes2,corAnglaises,clarinets1,clarinets2,clarinets3,bassoons1,bassoons2,bassoons3]

-- woodwindsDiv12Loud = [
--   flutes1,flutes2,clarinets1,flutes3,oboes1,oboes2,
--   clarinets2,corAnglaises,clarinets3,bassoons1,bassoons2,bassoons3
--   ]
-- woodwindsDiv12Soft = [
--   oboes1,oboes2,flutes1,flutes2,flutes3,corAnglaises,
--   clarinets1,clarinets2,bassoons1,clarinets3,bassoons2,bassoons3
--   ]

trumpetsDiv3 :: [Part]
trumpetsDiv3        = [trumpets1,trumpets2,trumpets3]

trombonesDiv3 :: [Part]
trombonesDiv3       = [trombones1,trombones2,trombones3]

trombsDiv6 :: [Part]
trombsDiv6           = trumpetsDiv3 <> trombonesDiv3

trombsDiv5 :: [Part]
trombsDiv5 = trumpetsDiv3 <> [trombones1,trombones2]

hornsBsnDiv6 :: [Part]
hornsBsnDiv6        = [horns1,horns2,bassoons1,bassoons2,horns3,horns4]

lowBrassDiv4 :: [Part]
lowBrassDiv4        = trombonesDiv3 <> [tub]

hornsDiv4 :: [Part]
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
stringsDiv6NoBass :: [Part]
stringsDiv6NoBass    = [violins1,violins2,violas1,violas2,cellos1,cellos2]
stringsDiv6NoBassHi  = [violins1_1,violins1_2,violins2,violas,cellos1,cellos2]
stringsDiv6NoBassHi' = [violins1_1,violins1_2,violins2_1,violins2_2,violas,cellos1]  -- No cello II
stringsDiv6NoBassHi''= [violins1_1,violins1_2,violins2_1,violins2_2,violas1,violas2] -- No cello
stringsDiv6NoBassLo  = [violins2,violas,cellos1of4,cellos2of4,cellos3of4,cellos4of4] -- No violin I
stringsDiv6NoBassLo' = [violas1,violas2,cellos1of4,cellos2of4,cellos3of4,cellos4of4] -- No violins

{-
-- vI vI vlaI vlaII vcI vcII
-- vI.1 vI.2 vII vla vcI vcII
-- vI.1 vI.2 vII vla vcI vcII
-- vI.1 vI.2 vII.1 vII.2 vla vcI

{-
18 wind instruments in upper-mid?
  3fl+2ob+3cl+2tr
-}
flutesDiv3, oboesAndCAs, trumpDiv2 :: [Part]
flutesDiv3 = [flutes1,flutes2,flutes3]
oboesAndCAs = [oboes1,oboes2,corAnglaises]
trumpDiv2 = [trumpets1,trumpets2]


stringsDiv10 :: [Part]
stringsDiv10       = [violins1_1,violins1_2,violins2_1,violins2_2,violas,cellos1of4,cellos2of4,cellos3of4,cellos4of4,basses]

stringsDiv10NoBass :: [Part]
stringsDiv10NoBass = [violins1_1,violins1_2,violins2_1,violins2_2,violas1,violas2,cellos1of4,cellos2of4,cellos3of4,cellos4of4]
-- Note: consider colour differences: low violins vs. high cellos, just using violas and basses a la Pathetique, etc
-}
stringsDiv8, stringsDiv8NoBass :: [Part]
stringsDiv8       = [violins1_1,violins1_2,violins2_1,violins2_2,violas,cellos1,cellos2,basses]
stringsDiv8NoBass = [violins1_1,violins1_2,violins2_1,violins2_2,violas1,violas2,cellos1,cellos2]

high3ToLow3 :: (HasParts' a, S.Part a ~ Part) => a -> a
high3ToLow3 = replaceParts [(violins1,cellos1),(violins2,cellos2),(violas,doubleBasses)]

[_flutes1,_flutes2,flutes3] = divide 3 flutes
[_oboes1,_oboes2,oboes3] = divide 3 oboes
[_clarinets1,_clarinets2,clarinets3] = divide 3 clarinets
[bassoons1,bassoons2,bassoons3] = divide 3 bassoons

[horns1,horns2,horns3,horns4] = divide 4 horns
[_trumpets1,_trumpets2,trumpets3] = divide 3 trumpets
[_trombones1,_trombones2,trombones3] = divide 3 trombones

basses = tutti doubleBass
timp = tutti timpani
[timp1,timp2] = divide 2 timp
cel = tutti celesta
vib = tutti vibraphone
hrp = harp
tub = tutti tuba

_corAnglaises = tutti corAnglais

spat :: [Note a] -> Pattern a
spat = newPattern . mconcat . map noteToVoice

ppat :: [Note a] -> Pattern (Voice a)
ppat = mconcat . map (pureP . noteToVoice)

pureP :: a -> Pattern a
pureP = newPattern . pure

palindr :: [a] -> [a]
palindr []  = error "palindromeL: empty list"
palindr [x] = [x]
palindr xs@(_:_)  = begin <> middle <> end <> reverse middle
  where
    begin  = pure (head xs)
    middle = init (tail xs)
    end    = pure (last xs)
