
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
        module Music.Time.Absolute,
        module Music.Time.Relative,
        module Music.Time.Era,
        module Music.Time.Performable,
  ) where

import Music.Time.Absolute
import Music.Time.Relative
import Music.Time.Era
import Music.Time.Performable
