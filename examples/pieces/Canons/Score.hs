
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts #-}

module Main where

import Music.Prelude
import Util

main = defaultMain music

{-
48 (or 64) canons for 2 or 2-4 voices.

Each is built using Data.List.unfold and sounds good with itself (counterpointally) offset by a fixed number of bars (i.e. 2, 6 etc
depending on the) number of voices.

Possibly, we could orchestrate all the canons.
-}
music = text "Hello!" c
