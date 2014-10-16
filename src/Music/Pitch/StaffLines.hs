
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

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund, Edward Lilley 2012–2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Lines and staff positions.
--
-------------------------------------------------------------------------------------

module Music.Pitch.StaffLines (
        StaffLines,
        HalfSpaces,
  ) where

import Numeric.Natural
import Data.Typeable

-- TODO maybe parameterize stafflines on number of staves

-- |
-- For notation systems using staff lines (i.e. CMN), this type represents staff number.
--
-- Rules:
--
-- * Incrementing staff lines means moving upward.--
--
-- * For staff systems with an odd number of lines, 0 is the middle staff.
--
-- * For staff systems with an even number of lines, 0 is the staff below the middle space.
--
-- I.e. in CMN the staff lines are @[-2..2]@, with negative numbers representing the lines
-- below the middle line and positions numbers representing the lines above it.
--
newtype StaffLines = StaffLines { getStaffLines :: Integer }
  deriving (Eq, Ord, Read, Show, Enum,
           Num, Real, Integral, Typeable)

-- |
-- For notation systems using staff lines, this type represents the difference betwee two
-- staff positions (i.e. one diatonic step in CMN).
--
-- Rules:
--
-- * Incrementing half spaces means moving upward.
--
-- * For staff systems with an odd number of lines, 0 is the middle staff.
--
-- * For staff systems with an even number of lines, 0 is the middle space.
--
-- I.e. in CMN, the non-ledger positions are @[-5..5]@ with the even numbers representing
-- lines and the odd representing spaces and with positive numbers representing positions
-- above the middle line and negative numbers representing positions below.
--
newtype HalfSpaces = HalfSpaces { getHalfSpaces :: Integer }
  deriving (Eq, Ord, Read, Show, Enum,
           Num, Real, Integral, Typeable)


-- staffLinesForNumber :: Int -> StaffLines
-- staffLinesForNumber