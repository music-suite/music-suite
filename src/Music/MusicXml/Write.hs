
module Music.MusicXml.Write (
    WriteMusicXml(..)

  ) where

import Text.XML.Light (Element)

class WriteMusicXml a where
    write :: a -> [Element]

