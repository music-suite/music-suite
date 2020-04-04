
{-# LANGUAGE OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

import Music.Prelude
import Control.Lens (over)

-- TODO move?
instance Transformable Amplitude where
  transform _ = id
instance AffineSpace Amplitude where
instance VectorSpace Amplitude where

-- A simple subject
subj :: Score (PartT Part (DynamicT (Behavior Amplitude) Pitch))
subj  = times 20 $ pseq [c,d,e,f]|/8 |> pseq [g,fs]|/2

music = id
  $ title "Dynamics"
  $ composer "Anonymous"
  $ fmap (over dynamics (! 0))
  $ 
    rcat $ map (\phase -> level (stretch phase sine*fff) $ subj) [5.0,5.2..6.0]

-- FIXME does not compile
main = defaultMain music
