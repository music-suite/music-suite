
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Music.Time.Transform (
      module Music.Time.Types,

      -- * The Transformable class
      Transformable(..),

      -- ** Apply under a transformation
      whilst,
      -- whilstStretch,
      -- whilstDelay,
      on,
      conjugate,

      -- ** Specific transformations
      delay,
      undelay,
      stretch,
      compress,

      -- *** Applied transformations
      delaying,
      undelaying,
      stretching,
      compressing,

      -- *** Utility
      delayTime,
  ) where

import           Music.Time.Internal.Transform
import           Music.Time.Types

