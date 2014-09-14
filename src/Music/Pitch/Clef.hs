
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Music.Pitch.Clef where

import Music.Pitch.StaffLines
import Data.Typeable

data ClefSymbol = GClef | CClef | FClef | PercClef | NeutralClef
    deriving (Eq, Ord, Show, Typeable)

type OctaveAdjustment = Integer

type Clef = (ClefSymbol, OctaveAdjustment, StaffLines)

{-
TODO

IsPitch instance?
  What to do with the non-standard pitches, i.e.
  we know what (g :: ClefSymbol) is, but what about (cs :: ClefSymbol)?
  I think 1) error or 2) default to g.

Map this to Pitch.Common
  Do we make this module 1) depend on Pitch.Common or 2) the other way around?
  If 1), do we need to separate G/C/F in ClefSymbol, maybe just put a single constructor for common pitch.
  In that case, maybe a function isStandardClefPitch (i.e. c/f/g) would be appropriate.
-}


-- isModernClef :: Clef -> Bool
-- isHistoricalClef :: Clef -> Bool

-- i.e. a c clef on some staff
-- isVoiceClef :: Clef -> Bool

