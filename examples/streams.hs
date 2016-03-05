
{-# LANGUAGE OverloadedStrings, FlexibleContexts, ConstraintKinds, TypeFamilies, RankNTypes #-}

import Music.Score (pitch) -- TODO
import qualified Music.Score
import Music.Prelude.Standard hiding (pitch, open, play, openAndPlay)
import Control.Concurrent.Async
import Control.Applicative
import System.Process (system)
import qualified Data.Foldable
import Control.Lens hiding (Parts)
import Data.Default -- debug
import Math.OEIS

{-    
    A composition using sequences from the OEIS (http://oeis.org/)

    You might need to install OEIS:
      cabal install oeis
-}

main :: IO ()
main = openMusicXml music

ensemble :: [Part]
ensemble = (divide 4 (tutti violin)) <> (divide 2 (tutti viola)) <> (divide 2 (tutti cello)) <> [tutti doubleBass]

type Color = Integer -> Interval
scale :: Color
scale n = case n `mod` 6 of
  0 -> _P1
  1 -> _M2
  2 -> _M3
  3 -> _A4
  4 -> _P5
  5 -> _M6

len = 390

seq1 :: Score Integer
seq1 = scat $ take len $ fmap return $ fmap (`mod` 6) $ Data.Foldable.toList $ extendSequence [2,1,1,2,2]
seq2 = scat $ take len $ fmap return $ fmap (`mod` 6) $ Data.Foldable.toList $ extendSequence [2,1,1,2,2,1]

-- Thue–Morse sequence
seq3 = scat $ take len $ fmap return $ fmap (`mod` 6) $ Data.Foldable.toList $ extendSequence [2,1,1,2,2,1,1]

music = pcat [(partNs 0 & up (m3^*2) & compress 6),
              (partNs 1 & up (m3^*1) & compress 5),
              (partNs 2 & compress 4)]
  & fmap (pitches' %~ normalize) & compress 4 & staccato

partNs n = part1 n <> part2 n <> part3 n
part1 n = asScore $ (parts' .~ (ensemble !! (0+3*n))) $ fmap (\x -> pitches' %~ (.+^ (scale x)) $ (c::StandardNote)) $ seq1
part2 n = asScore $ (parts' .~ (ensemble !! (1+3*n))) $ fmap (\x -> pitches' %~ (.+^ (scale x)) $ (c::StandardNote)) $ seq2
part3 n = asScore $ (parts' .~ (ensemble !! (2+3*n))) $ fmap (\x -> pitches' %~ (.+^ (scale x)) $ (c::StandardNote)) $ seq3

-- instance Monoid Part where
--   mempty = def
-- instance Monoid p => Monad (PartT p) where
--   return x = PartT (mempty, x)











-- TODO remove Default 
{-
parts :: (Default (Music.Score.Part a), Traversable t, HasPart a) => Traversal' (t a) (Music.Score.Part a) 
parts = traverse . part

part :: (Default (Music.Score.Part a), HasPart a) => Lens' a (Music.Score.Part a)
part = lens getPart (flip setPart)

part_ :: HasSetPitch a b => Setter a b (Music.Score.Pitch a) (Music.Score.Pitch b)
part_ = sets __mapPitch
-}

class Normal a where
    normalize :: a -> a
instance Normal Pitch where
    normalize = relative c (spell usingSharps)
instance Normal a => Normal (Behavior a) where
    normalize = fmap normalize



merge xs ys = concatMap (\(x,y) -> [x,y]) $ xs `zip` ys

mapEvensOdds :: (a -> b) -> (a -> b) -> [a] -> [b]
mapEvensOdds f g xs = let

    evens [] = []
    evens (x:xs) = x:odds xs

    odds [] = []
    odds (x:xs)  = evens xs

    in take (length xs) $ map f (evens xs) `merge` map g (odds xs)



