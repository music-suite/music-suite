{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
module Music.Score.Export.TechniqueNotation
  ( TechniqueNotation (..),
    textualNotations,
    notateTechnique,
  )
where

import Control.Lens -- ()
import Data.AffineSpace
import Data.Functor.Context
import Data.Semigroup
import Music.Score.Dynamics
import Music.Score.Phrases
import Music.Score.Technique
import Music.Score.Ties
import Music.Time

-- TODO this works for pizz/arco, col legno etc, but not for all techniques...
newtype TechniqueNotation = TechniqueNotation [String]
  deriving (Semigroup, Monoid)

textualNotations :: TechniqueNotation -> [String]
textualNotations (TechniqueNotation xs) = xs



notateTechnique :: Ctxt SomeTechnique -> TechniqueNotation
notateTechnique t = TechniqueNotation $ case getCtxt t of
  (Just prev, cur, _) ->
    if prev^.pizzicato /= cur^.pizzicato
      then showPizz (cur^.pizzicato) else []
    ++
    if prev^.legno /= cur^.legno
      then showLegno (cur^.legno) else []
    ++
    if prev^.stringPos /= cur^.stringPos
      then showStringPos (cur^.stringPos) else []
    ++
    if prev^.stringMute /= cur^.stringMute
      then showStringMute (cur^.stringMute) else []
    -- TODO add legno etc
  (_, cur, _) ->
       maybe [] showPizz (viewNotEmpty pizzicato cur)
    ++ maybe [] showLegno (viewNotEmpty legno cur)
    ++ maybe [] showStringPos (viewNotEmpty stringPos cur)
    ++ maybe [] showStringMute (viewNotEmpty stringMute cur)
  where
    viewNotEmpty l x = if v == mempty then Nothing else Just v
      where v = view l x

    -- TODO show lowercase "pizz" etc
    -- These should not use Show but a custom function to render e.g. ColLegnoBatt correctly
    showPizz = pure . show
    showLegno = pure . show
    showStringPos = pure . show
    showStringMute = pure . show
