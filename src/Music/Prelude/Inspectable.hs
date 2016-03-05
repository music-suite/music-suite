
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides miscellaneous instances.
--
-------------------------------------------------------------------------------------

module Music.Prelude.Inspectable (
  Inspectable(..),
  display,
  audify,
  displayAndAudify,
) where

import qualified System.Process

import Music.Prelude.Standard hiding (open,play)
import Music.Score ()
import qualified Music.Score
import qualified System.Process
import qualified System.Info
import Data.Ini

data Conf
  = Conf
    String --Midi player
    String --Midi converter
    String --Audio viwer
    String --Score viewer (TODO use program vs just write file etc)
defaultConf = Conf
  "timidity"
  "timidity -Ow"
  "open -a audacity"
  "lilypond"
  
-- Not perfect but works for many cases
--
-- >>> ucat [[violins],[violas]]
-- [Violin,Viola]
-- >>> ucat [[violins],[violins]]
-- [Violin I,Violin II]
-- >>> ucat [[violins1],[violins2]]
-- [Violin I,Violin II]
-- >>> ucat [[violins],[violins2]]
-- [Violin I,Violin II]
--
ucat :: (Monoid a, Semigroup a, HasParts a a, Music.Score.Part a ~ Part) => [a] -> a
ucat xs = if allDistinct ps 
    then pcat xs 
    else pcat $ zipWith (set parts') (divide (length xs) p) xs
  where
    ps = concatMap (toListOf parts') xs
    p  = foldr1 largestPart ps


class Inspectable a where
  inspectableToMusic :: a -> Music

displayAndAudify :: Inspectable a => a -> IO ()
displayAndAudify x = display x >> audify x

instance Inspectable (Score StandardNote) where
  inspectableToMusic = id


display :: Inspectable a => a -> IO ()
audify  :: Inspectable a => a -> IO ()
display = display' . inspectableToMusic
audify  = audify' . inspectableToMusic


display' = error "Disabled display"
-- display' = openLilypond
--   where
--     openLilypond' options sc = do
--       writeLilypond' options "test.ly" sc
--       runLilypond >> cleanLilypond >> runOpen
--         where
--           runLilypond = void $ System.Process.runCommand
--             -- "lilypond -f pdf test.ly"  >>= waitForProcess
--             "lilypond -f pdf test.ly 2>/dev/null"  >>= System.Process.waitForProcess
--           cleanLilypond = void $ System.Process.runCommand
--             "rm -f test-*.tex test-*.texi test-*.count test-*.eps test-*.pdf test.eps"
--           runOpen = void $ System.Process.runCommand
--             $ openCommand ++ " test.pdf"

openCommand :: String
openCommand = case System.Info.os of
  "darwin" -> "open"
  "linux"  -> "xdg-open"
    

audify'  = play_
  where
    play_ x = do
      writeMidi "test.mid" x
      System.Process.system "timidity test.mid 2>/dev/null >/dev/null"
      return ()
      
instance Inspectable a => Inspectable (Maybe a) where
  inspectableToMusic = maybe mempty id . fmap inspectableToMusic
instance Inspectable (Score Pitch) where
  inspectableToMusic = inspectableToMusic . asScore . fmap fromPitch''
instance Inspectable (Voice StandardNote) where
  inspectableToMusic = inspectableToMusic . renderAlignedVoice . aligned 0 0
instance Inspectable (Voice Pitch) where
  inspectableToMusic = inspectableToMusic . asScore . renderAlignedVoice . aligned 0 0 . fmap fromPitch''
-- instance Inspectable (Voice ()) where
  -- inspectableToMusic = inspectableToMusic . set pitches (c::Pitch)
instance Inspectable (Ambitus Pitch) where
  inspectableToMusic x = let (m,n) = x^.from ambitus in glissando $ fromPitch'' m |> fromPitch'' n
instance Inspectable [Chord Pitch] where
  inspectableToMusic = scat . fmap inspectableToMusic
instance Inspectable (Mode Pitch) where
  inspectableToMusic = inspectableToMusic . modeToScale c
instance Inspectable (Scale Pitch) where
  inspectableToMusic = fmap fromPitch'' . scat . map (\x -> pure x :: Score Pitch) . scaleToList
instance Inspectable (Function Pitch) where
  inspectableToMusic = inspectableToMusic . functionToChord c
instance Inspectable (Chord Pitch) where
  inspectableToMusic = fmap fromPitch'' . pcat . map (\x -> pure x :: Score Pitch) . chordToList
-- instance Inspectable [Hertz] where
--   inspectableToMusic xs = pcat $ map fromPitch'' $ map (^.from pitchHertz) xs
-- instance Inspectable [[Hertz]] where
--   inspectableToMusic = scat . fmap inspectableToMusic
instance Inspectable [Pitch] where
  inspectableToMusic = compress 8 . scat . fmap fromPitch''
-- instance Inspectable Hertz where
  -- inspectableToMusic = inspectableToMusic . (:[])
instance Inspectable Pitch where
  inspectableToMusic = inspectableToMusic . (:[])
instance Inspectable Interval where
  inspectableToMusic v = stretch 8 $ inspectableToMusic [c::Pitch, c .+^ v]
instance Inspectable Span where
  inspectableToMusic s = transform s c
instance Inspectable Time where
  inspectableToMusic t = inspectableToMusic (t >-> (1/4))
  -- TODO use power of 2 related to time...
instance Inspectable Duration where
  inspectableToMusic d = inspectableToMusic (0 >-> d)
instance Inspectable [Span] where
  inspectableToMusic xs = ucat $ fmap inspectableToMusic xs
instance Inspectable [Voice Pitch] where
  inspectableToMusic = asScore . ucat . fmap (fmap fromPitch'') . fmap (renderAlignedVoice . aligned 0 0)
instance Inspectable [Note Pitch] where
  inspectableToMusic = inspectableToMusic . fmap ((^.voice) . pure)

