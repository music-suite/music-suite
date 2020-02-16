
{-# LANGUAGE OverloadedStrings #-}

import Music.Prelude
import Control.Lens (set, over)

-- A simple subject
subj :: Score (Maybe StandardNote)
subj  = times 2 $ pseq [c,c,d,b_,e,e]|/16

-- Each voice differ slighly in onset etc
voca :: Duration -> Music
voca v = delay ((4/8)|*v) $ removeRests $ pseq $ fmap (\i -> (id $ up (_M2^*i) subj) |> rest|*(15/8))
  $ [0..3]

music = id
  -- Set meta-informtion
  $ title "Phrases"
  $ composer "Anonymous"
  -- Change time signature at bar 15
  $ timeSignatureDuring ((14*3/8) <-> 200) (4/8)
  $ timeSignature (3/8)
  -- Use phrase traversal to fuse equal consecutive pitches
  $ over phrases fuse
  $ rcat
  $
    -- Use phrase traversals to rotate the pitches (but not the durations)
    -- in each phrase.
    [ over phrases (rotateValues 2) $ voca 0
    , voca 1
    , over phrases (rotateValues 3) $ voca 2
    , over phrases (rotateValues 4) $ voca 3
    ]

main  = defaultMain music

