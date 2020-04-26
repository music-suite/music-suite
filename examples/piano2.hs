{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

import Music.Prelude

{- TODO basic idea here is:
 -
 -  * Have a topLevelScore of some custom type Block
 -  * Use a render function (Block -> Pattern a) and renderPatternsAbs
 -
 - This is nice, but a basic problem is the difficulty of
 - visualizing/understanding the topLevelScore and render function separately.
 - We could use a graphical backend for topLevelScore.
 -
 - How about the render function?
 -
 -}
main = defaultMain music

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
    |> stretch 2 ( ( ( mempty
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
    |> stretch 3 ( ( ( mempty
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
  (IsPitch a, Transposable a) => Block -> Pattern a
renderSimple Block {col, range, texture} = stretch 20 $
  let transp =
        case range of
          Hi -> up _P15
          Lo -> id
   in transp $ case col of
        Blue -> g
        Brown -> c

render ::
  (IsPitch a, Transposable a) => Block -> Pattern a
render Block {col, range, texture = Chord} = case col of
  Blue ->
    mconcat [c_, a_,fs]
  Brown ->
    mconcat [g_,d,a]
render Block {col, range, texture = Repeat} =
  let transp =
        case range of
          Hi -> up _P8
          Lo -> id
   in transp $
        case col of
          Blue ->
            newPattern [a|*3, d, e] |/ 8
          Brown ->
            newPattern [e,fs|*2] |/ 6
