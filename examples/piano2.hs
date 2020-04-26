{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

import Music.Prelude
import qualified Music.Score.Articulation as S
import qualified Music.Score.Pitch as S

{- TODO basic idea here is:
 -
 -  * Have a topLevelScore of some custom type Block
 -  * Use a render function (Block -> Pattern a) and renderPatternsAbs
 -
 - This is nice, but a basic problem is the difficulty of
 - visualizing/understanding the topLevelScore and render function separately.
 - We could use a graphical backend for topLevelScore.
 -
 - How about the render function? It might be hard to test all possible
 - inputs. Could we draw some representative samples? OTOH if the type
 - is finite and small, maybe just enumerate?
 -
 - We need to build a library of *values* such as:
 -
 -    - Melodies, or fragments thereof
 -    - Scales, chords, chord progressions
 -    - Rhythms, patterns
 -    - Predicates on dissonance, voice leading etc
 -
 - Try not to *add more types*! Focus on the values.
 -
 - Also a solution to the above: simpler patterns and simpler topLevelScore!
 -}
main =
  -- TODO workaround for the fact that the REPL can only show
  -- the top-level definition.

  -- defaultMain music
  defaultMain $
    inspectableToMusic @(Pattern Pitch)
      p5_1

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
            newPattern [a |* 3, d, e] |/ 8
          Brown ->
            newPattern [e, fs |* 2] |/ 6

p1 :: IsPitch a => Pattern a
p1 = newPattern [a |* 3, d, e] |/ 8

p1_1 :: IsPitch a => Pattern a
p1_1 = newPattern [f, e, f, d, e, c, d |* 2, d |* 2] |/ 8

p1_1i :: (IsPitch a, HasPitches' a, S.Pitch a ~ Pitch) => Pattern a
p1_1i = invertDiatonic e p1_1

p2 :: IsPitch a => Pattern a
p2 = newPattern [e, fs |* 2] |/ 3

p4 :: IsPitch a => Pattern a
p4 = newPattern [a, g, d, e] |/ 8

p4_0 :: IsPitch a => Pattern a
p4_0 = p4 <> delay (1 / 4) p4

p4_1 :: IsPitch a => Pattern a
p4_1 = newPattern [a, g, d, e, b_, c, d, e] |/ 8

p4_2 :: IsPitch a => Pattern a
p4_2 = newPattern [f,g,a,b,a,b,c',d',e',d',c',b] |/ 16

p5 :: IsPitch a => Pattern a
p5 = newPattern [c__,g__,e_,c,e_,g__] |/ 12

p5_1 :: IsPitch a => Pattern a
p5_1 = newPattern [c__,g__,c_,e_,e,c,g_,c_] |/ 16
