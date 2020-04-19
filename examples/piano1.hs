{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

import Music.Prelude

main = defaultMain music

music :: Music
music =
  fmap Just $ renderPatternsAbs
    -- $ fmap renderSimple
    $ fmap render
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
  ( ( pure
        (Block Brown Hi Chord)
        |> (pure (Block Blue Hi Chord) |* 2)
    )
      <> ( delay 2
             . stretch 2
         )
        (pure (Block Brown Lo Chord) |> (pure (Block Blue Lo Chord) |* 2))
  )
    <>  delay 4 ( ( pure
             (Block Brown Hi Repeat)
             |> (pure (Block Blue Hi Repeat) |* 2)
         )
           <> ( delay 2
                  . stretch 2
              )
             (pure (Block Brown Lo Repeat) |> (pure (Block Blue Lo Repeat) |* 2))
       )

-- |
-- Alternative to 'render' just to get a sense of 'topLevelScore'.
renderSimple
  :: (IsPitch a, Transposable a) => Block -> Pattern a
renderSimple Block{col,range,texture} = case (col,range,texture) of
  (Blue,_,_) -> c
  (Brown,_,_) -> c_

render
  :: (IsPitch a, Transposable a) => Block -> Pattern a
render Block {col, range, texture = Chord} = case col of
  Blue ->
    mconcat [c_, g_]
  Brown ->
    mconcat [d_, b_]
render Block {col, range, texture = Repeat} =
  let transp =
        case range of
          Hi -> up _P8
          Lo -> id
   in transp $
        case col of
          Blue ->
            newPattern [c, d, e] |/ 8
          Brown ->
            newPattern [e, fs] |/ 6
