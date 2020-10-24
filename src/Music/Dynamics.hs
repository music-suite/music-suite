-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides various representation of musical dynamics.
module Music.Dynamics
  ( module Music.Dynamics.Absolute,
    module Music.Dynamics.Common,
    module Music.Dynamics.Literal,
  )
where

import Music.Dynamics.Absolute
import Music.Dynamics.Common
import Music.Dynamics.Literal

{-
    ppp -42    -36
    pp  -36    -36
    p   -30    -24
    mp  ?
    mf  -24    -18
    f   -18    -12
    ff  -18    -12




    Midi velocities according to Apple:
        16  32  48  64  80  96  112  127
        ppp pp  p   mp  mf  f   ff   fff

    Nakamura (1987) The communication of dynamics between musicians and listeners through musical performance
-}
