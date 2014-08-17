
-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides abstract musical articulations.
--
-------------------------------------------------------------------------------------

module Music.Articulation (
  Articulation,
)
where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Control.Monad
import Control.Applicative
import Data.Monoid.Average

type Articulation = (Average Double, Average Double)

{-

  References
    
    Keller: Phrasing and Articulation: A Contribution to a Rhetoric of Music

      Keller distinguishes between articulation and phrasing:
        - Phrasing is related to the structure or grammar of the music,
          how hierarchical relationsship emerges.
        - Articulation is "everything else", the individual interpretation of
          the melodic line.
        - Keller consider phrasing objective. (It is at least non-deterministic!)
    
    http://www.speech.kth.se/publications/masterprojects/2004/Jerkert.pdf
    
      - Articulation is a *local* alteration of other properties
        - There are multiple interacting hierarchical relationships
        - Articulation has to do with *emphasis*: alteration of properties leads
          to more or less emphasis (compare Laws of Perception)
      - Most common:
        - Time
          - "articulation ratio", i.e. duration/IOI
          - "relative IOI", i.e. prolongation of a note
        - Dynamics
          - fp/sharp attacks etc (i.e. in wind, string)
          - Relative level (i.e. in piano music)
        - Pitch
          - Vibrato, local adjustments (i.e. brighter notes)
        - Timbre
          - Dryness/spectral richness (i.e. more overtones)
    
    http://www.jbiomech.com/article/S0021-9290%2898%2900113-4/abstract
-}

{-

From music21:
  Accent
  Bowing
  BrassIndication
  BreathMark
  Caesura
  DetachedLegato
  Doit
  DoubleTongue
  DownBow
  DynamicArticulation
  Falloff
  FretBend
  FretIndication
  FretTap
  FrettedPluck
  HammerOn
  Harmonic
  HarpFingerNails
  HarpIndication
  IndeterminantSlide
  LengthArticulation
  NailPizzicato
  OpenString
  OrganHeel
  OrganIndication
  OrganToe
  PitchArticulation
  Pizzicato
  Plop
  PullOff
  Scoop
  SnapPizzicato
  Spiccato
  Staccatissimo
  Staccato
  Stopped
  Stress
  StringFingering
  StringHarmonic
  StringIndication
  StringThumbPosition
  StrongAccent
  TechnicalIndication
  Tenuto
  TimbreArticulation
  TonguingIndication
  TripleTongue
  Unstress
  UpBow
  WindIndication
  WoodwindIndication 






----------------


  Stress
  Unstress
  Accent
  StrongAccent

  Tenuto
  Spiccato
  Staccato
  Staccatissimo

  Harmonic
  OpenString
  Stopped
  HammerOn
  PullOff

  OrganIndication
  OrganHeel
  OrganToe
  UpBow
  DownBow

  DetachedLegato (laisser vibrer)
  IndeterminantSlide


  
  Doit (grace?)
  Falloff?
  StringFingering
  HarpFingerNails

  NailPizzicato
  Pizzicato
  SnapPizzicato

  Bowing?

  TonguingIndication
  DoubleTongue
  TripleTongue


  HarpIndication
  FretIndication
  FrettedPluck
  FretTap
  LengthArticulation
  StringIndication
  StringThumbPosition


  Plop
  Scoop

  FretBend

  WindIndication
  BrassIndication
  WoodwindIndication
  TechnicalIndication
  TimbreArticulation

  BreathMark
  Caesura
  
-}