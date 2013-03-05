
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
-------------------------------------------------------------------------------------

module Music.MusicXml.Write (
    WriteMusicXml(..)

  ) where

import Text.XML.Light (Element)

class WriteMusicXml a where
    write :: a -> [Element]

