
module Music.Score.Ties where

import Music.Score.Score

-- |
-- Class of types that can be tied.
--
class Tiable a where
    -- | Split elements into beginning and end and add tie.
    --   Begin properties goes to the first tied note, and end properties to the latter.
    toTie :: a -> (a, a)

-- | 
-- Split all notes that cross a barlines into a pair of tied notes.
-- 
splitTies :: Tiable a => Score a -> Score a
splitTies = undefined