
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,     
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    TypeOperators,    
    OverloadedStrings,
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides time representations for use with scores, events etc.
--
-------------------------------------------------------------------------------------

module Music.Time (
        module Music.Time.Time,
        module Music.Time.Duration,
        module Music.Time.Delayable,
        module Music.Time.Stretchable,
        module Music.Time.Era,
        module Music.Time.Performable,
        module Music.Time.Pos,
  ) where

import Music.Time.Time
import Music.Time.Duration
import Music.Time.Delayable
import Music.Time.Stretchable
import Music.Time.Era
import Music.Time.Performable
import Music.Time.Pos
