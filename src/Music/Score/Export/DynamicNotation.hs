
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Music.Score.Export.DynamicNotation (
    CrescDim(..),
    DynamicNotation(..),
    notateDynamic,
  ) where

-- import           Music.Dynamics.Literal
-- import           Music.Pitch.Literal
-- 
-- import qualified Codec.Midi                    as Midi
-- import           Control.Arrow                 ((***))
-- import           Control.Comonad               (Comonad (..), extract)
-- import           Data.Colour.Names             as Color
-- import           Data.Default
-- import           Data.Foldable                 (Foldable)
-- import qualified Data.Foldable
-- import           Data.Functor.Couple
-- import           Data.Maybe
-- import           Data.Ratio
-- import           Data.Traversable              (Traversable, sequenceA)
-- import qualified Music.Lilypond                as Lilypond
-- import           Music.Score.Internal.Export   hiding (MVoice)
-- import           Music.Time.Internal.Transform (whilstLD)
-- import           Music.Score.Internal.Util     (composed, unRatio)
-- import           System.Process
-- import           Music.Time.Internal.Quantize
-- import qualified Text.Pretty                   as Pretty
-- import qualified Data.List
-- import Music.Score.Convert (reactiveToVoice') -- TODO
-- import Music.Score.Internal.Util (swap, retainUpdates)
import Data.Semigroup
import Control.Lens -- ()
import Music.Score.Dynamics
import Music.Score.Ties
import Music.Time


data CrescDim = NoCrescDim | BeginCresc | EndCresc | BeginDim | EndDim

instance Monoid CrescDim where
  mempty = NoCrescDim
  mappend a _ = a

newtype DynamicNotation 
  = DynamicNotation { getDynamicNotation :: ([CrescDim], Maybe Double) }

instance Wrapped DynamicNotation where
  type Unwrapped DynamicNotation = ([CrescDim], Maybe Double)
  _Wrapped' = iso getDynamicNotation DynamicNotation

instance Rewrapped DynamicNotation DynamicNotation

type instance Dynamic DynamicNotation = DynamicNotation

instance Transformable DynamicNotation where
  transform _ = id

instance Tiable DynamicNotation where
  toTied (DynamicNotation (beginEnd, marks)) 
    = (DynamicNotation (beginEnd, marks), 
       DynamicNotation (mempty, Nothing))

instance Monoid DynamicNotation where
  mempty = DynamicNotation ([], Nothing)
  DynamicNotation ([], Nothing) `mappend` y = y
  x `mappend` DynamicNotation ([], Nothing) = x
  x `mappend` y = x

-- Given a dynamic value and its context, decide:
--
--   1) Whether we should begin or end a crescendo or diminuendo
--   2) Whether we should display the current dynamic value
--
notateDynamic :: (Ord a, Real a) => Ctxt a -> DynamicNotation
notateDynamic x = DynamicNotation $ over _2 (\t -> if t then Just (realToFrac $ extractCtxt x) else Nothing) $ case x of
  (Nothing, y, Nothing) -> ([], True)
  (Nothing, y, Just z ) -> case (y `compare` z) of
    LT      -> ([BeginCresc], True)
    EQ      -> ([],           True)
    GT      -> ([BeginDim],   True)
  (Just x,  y, Just z ) -> case (x `compare` y, y `compare` z) of
    (LT,LT) -> ([NoCrescDim], False)
    (LT,EQ) -> ([EndCresc],   True)
    (EQ,LT) -> ([BeginCresc], False{-True-})

    (GT,GT) -> ([NoCrescDim], False)
    (GT,EQ) -> ([EndDim],     True)
    (EQ,GT) -> ([BeginDim],   False{-True-})

    (EQ,EQ) -> ([],                   False)
    (LT,GT) -> ([EndCresc, BeginDim], True)
    (GT,LT) -> ([EndDim, BeginCresc], True)


  (Just x,  y, Nothing) -> case (x `compare` y) of
    LT      -> ([EndCresc],   True)
    EQ      -> ([],           False)
    GT      -> ([EndDim],     True)


-- TODO consolidate
mapCtxt :: (a -> b) -> Ctxt a -> Ctxt b
mapCtxt f (a,b,c) = (fmap f a, f b, fmap f c)

extractCtxt :: Ctxt a -> a
extractCtxt (_,x,_) = x
