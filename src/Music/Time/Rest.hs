{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-- |
-- Many time structures such as 'Score' allows for rests between notes. Generally rests
-- are simply treated as blank space, and thus have no duration. Sometimes it is useful
-- to represent rests explicitly, so this module provides an alias for 'pure' 'Nothing' that
-- can be used to that end.
--
-- @
-- pseq [c,d,rest^*2,e]^/8
-- @
module Music.Time.Rest
  ( -- * Rests
    rest,
  )
where

import Control.Applicative
import Music.Time.Juxtapose

rest :: Applicative f => f (Maybe a)
rest = pure Nothing
-- TODO overload a la IsPitch (bottom instances for ()/Maybe, transformed like IsPitch)
