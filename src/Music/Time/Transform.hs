
module Music.Time.Transform (

        module Music.Time.Types,

        -- * The Transformable class
        Transformable(..),
        transformed,

        -- * Specific transformations
        -- ** Transformations
        delaying,
        undelaying,
        stretching,
        compressing,

        -- ** Transforming values
        delay,
        undelay,
        stretch,
        compress,
        (|*),
        (*|),
        (|/),

  ) where

import           Music.Time.Internal.Transform
import           Music.Time.Types

infixl 7 |*
infixr 7 *|
infixr 7 |/

-- | Infix version of 'stretch'.
x |* d = stretch d x
-- | Infix version of 'stretch'.
d *| x = stretch d x
-- | Infix version of 'compress'.
x |/ d = compress d x 
