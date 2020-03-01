{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  #-}
module Music.Time.Transform
  ( module Music.Time.Types,

    -- * The Transformable class
    Transformable (..),
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
  )
where

import Music.Time.Internal.Transform
import Music.Time.Types

infixl 7 |*

infixr 7 *|

infixr 7 |/

-- |  Infix version of 'stretch'.
(|*) :: forall a. Transformable a => a -> Duration -> a
x |* d = stretch d x

-- |  Infix version of 'stretch'.
(*|) :: forall a. Transformable a => Duration -> a -> a
d *| x = stretch d x

-- |  Infix version of 'compress'.
(|/) :: forall a. Transformable a => a -> Duration -> a
x |/ d = compress d x
