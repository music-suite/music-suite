
module Music.Time.Rest (
        -- * Rests
        rest,
  ) where

import           Control.Applicative
import           Music.Time.Juxtapose

rest :: Applicative f => f (Maybe a)
rest = pure Nothing
