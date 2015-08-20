
-- {-# LANGUAGE ConstraintKinds           #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE FlexibleInstances         #-}
-- {-# LANGUAGE MultiParamTypeClasses     #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE RankNTypes                #-}
-- {-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE TupleSections             #-}
-- {-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE UndecidableInstances      #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Many time structures such as 'Score' allows for rests between notes. Generally rests
-- are simply treated as blank space, and thus have no duration. Sometimes it is useful
-- to represent rests explicitly, so this module provides an alias for 'pure' 'Nothing' that
-- can be used to that end.
--
-- To remove rests from a score, use 'mcatMaybes', for example:
--
-- > open $ mcatMaybes $ scat [c,d,rest^*2,e]^/8
--
-------------------------------------------------------------------------------------

module Music.Time.Rest (
        -- * Rests
        rest,
  ) where

import           Control.Applicative
import           Music.Time.Juxtapose

rest :: Applicative f => f (Maybe a)
rest = pure Nothing
