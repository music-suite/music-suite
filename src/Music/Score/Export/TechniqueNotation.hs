{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

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

import Control.Lens
import Data.AffineSpace
import Data.Functor.Context
import Data.Semigroup
import Music.Score.Dynamics
import Music.Score.Phrases
import Music.Score.Technique
import Music.Score.Ties
import Music.Time

newtype TechniqueNotation = TechniqueNotation [String]
  deriving (Semigroup, Monoid)

textualNotations :: TechniqueNotation -> [String]
textualNotations (TechniqueNotation xs) = xs

notateTechnique :: Ctxt Technique -> TechniqueNotation
notateTechnique t = TechniqueNotation $ case getCtxt t of
  (Just prev, cur, _) ->
    if prev ^. pizzicato /= cur ^. pizzicato
      then showPizz (cur ^. pizzicato)
      else
        []
          ++ if prev ^. legno /= cur ^. legno
            then showLegno (cur ^. legno)
            else
              []
                ++ if prev ^. stringPos /= cur ^. stringPos
                  then showStringPos (cur ^. stringPos)
                  else
                    []
                      ++ if prev ^. stringMute /= cur ^. stringMute
                        then showStringMute (cur ^. stringMute)
                        else []
  (_, cur, _) ->
    maybe [] showPizz (viewNotEmpty pizzicato cur)
      ++ maybe [] showLegno (viewNotEmpty legno cur)
      ++ maybe [] showStringPos (viewNotEmpty stringPos cur)
      ++ maybe [] showStringMute (viewNotEmpty stringMute cur)
  where
    viewNotEmpty l x = if v == mempty then Nothing else Just v
      where
        v = view l x

showPizz :: PizzArco -> [String]
showPizz Pizz = ["pizz."]
showPizz Arco = ["arco"]

showLegno :: Legno -> [String]
showLegno NonLegno = ["senza legno"]
showLegno ColLegnoBatt = ["col legno batt."]
showLegno ColLegnoTratto = ["col legno tratto"]

showStringPos :: StringPos -> [String]
showStringPos PosNat = ["pos nat."]
showStringPos MoltoSulTasto = ["molto sul tasto"]
showStringPos MoltoSulPont = ["molto sul pont."]
showStringPos SulTasto = ["sul tasto"]
showStringPos SulPont = ["sul pont."]

showStringMute :: StringMute -> [String]
showStringMute NoStringMute = ["senza sord."]
showStringMute StringMute = ["con sord."]
