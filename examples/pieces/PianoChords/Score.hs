
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Music.Prelude
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Util

{-
[Difficulty: 1] Feldman-style etude for piano or harp. Just chords with various spacing and attack order (from arp up, arp down etc). Any permitation of 4 or 5. Always 16-th notes, sustained.

>>> length $ Data.List.permutations [1..5]
120
>>> length $ Data.List.permutations [1..4]
24


TODO
  Come up with a way to select permuatations (how many to use?)
  Harmonic material?
    No octave doublings!
    Pitch content
    Ambitus

-}
music = 
  title "Study 1" $ composer "Hans Hoeglund 2014" $ _8vb rh </> set parts' cellos (c^/8)
  where
    rh = scatPaused (fmap (\x -> fromInteger $ floor $ x*7) rands) $ compress 4 $ map (\(a,b,c) -> realize a b c) material


pitchClassMaterial :: [PitchClassSet]
pitchClassMaterial = [
  pitchClassSet [c,e,f,g],
  pitchClassSet [c,e,f,g],
  pitchClassSet [c,e,fs,g],
  pitchClassSet [c,e,f,g],
  pitchClassSet [c,e,g,fs,g],
  pitchClassSet [c,e,f,g,bb],
  pitchClassSet [c,e,g,fs,gs],
  pitchClassSet $ map (pitchToPitchClass . down (_M2::Interval)) [c,e,g,f,gs],
  pitchClassSet $ map (pitchToPitchClass . down (_M2::Interval)) [c,e,g,f,gs],
  pitchClassSet [c,e,g,fs,g],
  pitchClassSet [c,e,g,a,gs],
  pitchClassSet [c,e,g,a,bb],
  pitchClassSet [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,a,gs],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,fs,gs],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,fs,gs],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,fs,gs],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,fs,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,fs,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,fs,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,fs,bb],
   pitchClassSet [c,e,g,fs,gs],
  pitchClassSet [c,e,g,fs,bb],
  pitchClassSet [c,e,g,a,bb],
  pitchClassSet [c,e,g,f,bb],
  pitchClassSet $ map (pitchToPitchClass . down (_M2::Interval)) [c,e,g,f,bb],
  pitchClassSet $ map (pitchToPitchClass . down (_M2::Interval)) [c,e,g,f,bb],
  pitchClassSet $ map (pitchToPitchClass . down (_M2::Interval)) [c,e,g,f,gs],
  pitchClassSet [c,e,g,f,gs],
  pitchClassSet [c,e,gs,f,gs],
  pitchClassSet [c,e,gs,a,gs],
  pitchClassSet [c,e,gs,a,gs],

  
  pitchClassSet [c,e,g,a,gs],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,gs],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,gs],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,eb,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
  pitchClassSet $ map (pitchToPitchClass . up (_M2::Interval)) [c,e,g,a,bb],
   pitchClassSet [c,e,g,a,bb],
  pitchClassSet [c,e,g,a,bb],
  pitchClassSet [c,e,g,a,bb],
  pitchClassSet [c,e,g,f,bb],
  pitchClassSet $ map (pitchToPitchClass . down (_M2::Interval)) [c,e,g,f,bb],
  pitchClassSet $ map (pitchToPitchClass . down (_M2::Interval)) [c,e,g,f,bb],
  pitchClassSet $ map (pitchToPitchClass . down (_M2::Interval)) [c,e,g,f,bb],
  pitchClassSet [c,e,g,f,bb],
  pitchClassSet [c,e,bb,f,gs],
  pitchClassSet [c,e,gs,a,gs],
  pitchClassSet [c,e,gs,a,gs],
  pitchClassSet [c,e,g,f,gs],
  pitchClassSet [c,e,gs,f,gs],
  pitchClassSet [c,e,gs,fs,gs],
  pitchClassSet [c,e,gs,fs,gs]
  ]

registerMaterial :: [Ambitus Pitch]
registerMaterial = [
  (c,c')^.ambitus,
  (c,c')^.ambitus,
  (c,c')^.ambitus,
  (c,c')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c_,_8va c')^.ambitus,
  (c_,_8va c')^.ambitus,
  (c_,_8va c')^.ambitus,
  (c_,_8va c')^.ambitus,
  (c_,_8va c'')^.ambitus,
  (c_,_8va c'')^.ambitus,
  (c_,_8va c'')^.ambitus,
  (c_,_8va c'')^.ambitus,
  (c_,_8va c'')^.ambitus,
  (c_,c'')^.ambitus,
  (c_,c')^.ambitus,
  (c_,c')^.ambitus,
  (c,c'')^.ambitus,
  (c,c')^.ambitus,
  (c,c')^.ambitus,
  (c,c')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,

  (c,_8va c')^.ambitus,
  (c,_8va c')^.ambitus,
  (c,_8va c')^.ambitus,
  (c,_8va c')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (_8vb c_, _8vb c')^.ambitus,
  (_8vb c_, _8vb c')^.ambitus,
  (_8vb c_, _8vb c')^.ambitus,
  (_8vb c_, _8vb c')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c')^.ambitus,
  (c_, c')^.ambitus,
  (c,c'')^.ambitus,
  (c,c')^.ambitus,
  (c,c')^.ambitus,
  (c,c')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  

  (c,c')^.ambitus,
  (c,c')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,
  (_8va c,_8va c'')^.ambitus,

  (c,_8va c')^.ambitus,
  (c,_8va c')^.ambitus,
  (c,_8va c')^.ambitus,
  (c,_8va c')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (c,c'')^.ambitus,
  (_8vb c_, _8vb c')^.ambitus,
  (_8vb c_, _8vb c')^.ambitus,
  (_8vb c_, _8vb c')^.ambitus,
  (_8vb c_, _8vb c')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus,
  (_8vb c_, _8vb c'')^.ambitus
  ]

permMaterial :: [Permutation]
permMaterial = [
  Perm4 0,
  Perm4 0,
  Perm4 0,
  Perm4 1,
  Perm4 0,
  Perm4 0,
  Perm4 6,
  Perm5 2,
  Perm5 2,
  Perm5 6,
  Perm5 6,
  Perm5 6,
  Perm4 2,
  Perm5 1,
  Perm5 2,
  Perm5 6,
  Perm4 2,
  Perm5 6,
  Perm5 6,
  Perm5 6,
  Perm4 6,
  Perm4 7,
  Perm4 6,
  Perm5 6,
  Perm5 6,
  Perm4 1,
  Perm5 6,
  Perm5 6,
  Perm5 (-5),
  Perm4 (-2),
  Perm4 (-1),
  Perm4 (-1),
  Perm4 (-1),

  Perm4 $ 1+0,
  Perm4 $ 1+0,
  Perm4 $ 1+0,
  Perm4 $ 1+1,
  Perm4 $ 1+0,
  Perm4 $ 1+0,
  Perm4 $ 1+6,
  Perm5 $ 1+2,
  Perm5 $ 1+2,
  Perm5 $ 1+6,
  Perm5 $ 1+6,
  Perm5 $ 1+6,
  Perm4 $ 1+2,
  Perm5 $ 1+1,
  Perm5 $ 1+2,
  Perm5 $ 1+6,
  Perm4 $ 1+2,
  Perm5 $ 1+6,
  Perm5 $ 1+6,
  Perm5 $ 1+6,
  Perm4 $ 1+6,
  Perm4 $ 1+7,
  Perm4 $ 1+6,
  Perm5 $ 1+6,
  Perm5 $ 1+6,
  Perm4 $ 1+1,
  Perm5 $ 1+6,
  Perm5 $ 1+6,
  Perm5 $ (-5),
  Perm4 $ (-2),
  Perm4 $ (-1),
  Perm4 $ (-1),
  Perm4 $ (-1),

  Perm4 0,
  Perm4 0,
  Perm4 0,
  Perm4 1,
  Perm4 0,
  Perm4 0,
  Perm4 6,
  Perm5 2,
  Perm5 2,
  Perm5 6,
  Perm5 6,
  Perm5 6,
  Perm4 2,
  Perm5 1,
  Perm5 2,
  Perm5 6,
  Perm4 2,
  Perm5 6,
  Perm5 6,
  Perm5 6,
  Perm4 6,
  Perm4 7,
  Perm4 6,
  Perm5 6,
  Perm5 6,
  Perm4 1,
  Perm5 6,
  Perm5 6,
  Perm5 (-5),
  Perm4 (-2),
  Perm4 (-1),
  Perm4 (-1),
  Perm4 (-1)
  ]

material :: [(PitchClassSet, Ambitus Pitch, Permutation)]
material = zip3 pitchClassMaterial registerMaterial permMaterial

showMaterial m = pcm' </> rm' </> pm'
  where
    (pcm, rm, pm) = unzip3 m
    pcm' = scat $ fmap fromPitchClassSet pcm
    rm'  = scat $ fmap showAmbitus rm
    pm'  = scat $ fmap showPermutation pm
    

realize :: PitchClassSet -> Ambitus Pitch -> Permutation -> Score StandardNote
realize pcs reg perm = music
  where
    pitches = Set.toList $ Set.map pitchClassToPitch pcs :: [Pitch]
    transposedPitches       = spreadPitchesIntoAmbitus reg {-$ List.sort-} pitches
    sortedTransposedPitches = List.sort transposedPitches
    pitchSequence           = fmap (cycle sortedTransposedPitches !!) (indexPermutation perm)
    music                   = level pp $ legato $ stretchTo 1 $ scat $ fmap fromPitch'' (pitchSequence :: [Pitch])



-----


-- Spread pitches (assumed to be in the first octaves) over the whole ambitus
-- Order is retained

-- TODO better algorithm here
--  * Given an ambitus of n octaves, distribute pitches into these
--    * If given to many pitches, put extremes in the middle
--    * If given to few pitches, skip the extra octaves

spreadPitchesIntoAmbitus :: Ambitus Pitch -> [Pitch] -> [Pitch]
spreadPitchesIntoAmbitus amb pitches = zipWith {-octavesUp-} fifthsUp
  (ambitusOctaveList amb (fromIntegral $ length pitches))
  pitches
  where
    ambitusOctaveList :: Ambitus Pitch -> Integer -> [Integer] -- 0 for same, -1 for one down etc
    -- ambitusOctaveList amb n = fmap fromIntegral $ (repeat lowestOctave) -- TODO
    ambitusOctaveList amb n = fmap (+ lowestOctave) $ fromNumPitchesAndOctaves n numOctaves
      where
        lowestOctave = fromIntegral $ ambitusLowestOctave amb
        numOctaves   = fromIntegral $ ambitusOctaves      amb
    fromNumPitchesAndOctaves :: Integer -> Integer -> [Integer]
    fromNumPitchesAndOctaves = go where
      go 4 0 = [0,0,0,0]
      go 4 1 = [0,0,0,0]
      go 4 2 = [0,0,1,1]
      go 4 3 = [0,1,1,2]
      go 4 4 = [1,2,3,4]
      go 5 0 = [0,0,0,0,0]
      go 5 1 = [0,0,0,0,0]
      go 5 2 = [0,0,0,1,1]
      go 5 3 = [0,1,1,2,2]
      go 5 4 = [0,1,1,2,3]
      go 5 5 = [1,2,3,4,5]
      go m n = error $ "fromNumPitchesAndOctaves: Unexpected: " ++ show m ++ " " ++ show n


newtype PitchClass = PC { getPC :: (Name, Accidental) }
  deriving (Eq, Ord, Show)
instance IsPitch PitchClass where
  fromPitch x = let p = fromPitch x in PC (name p, accidental p)

type PitchSet      = Set Pitch
type PitchClassSet = Set PitchClass

pitchClassToPitch :: PitchClass -> Pitch
pitchClassToPitch (PC (n,a)) = mkPitch n a

pitchToPitchClass :: Pitch -> PitchClass
pitchToPitchClass p = PC (name p, accidental p)

fromPitchSet :: PitchSet -> Score StandardNote
fromPitchSet = asScore . pcat . fmap fromPitch'' . Set.toList

fromPitchClassSet :: PitchClassSet -> Score StandardNote
fromPitchClassSet = fromPitchSet . Set.map pitchClassToPitch

pitchClassSet :: [PitchClass] -> PitchClassSet
pitchClassSet = Set.fromList


