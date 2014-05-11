

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Music.Pitch.Literal
import Music.Score hiding (
  toLilypond,
  toLilypondString
  )
import qualified Music.Lilypond as Lilypond
import qualified Text.Pretty                  as Pretty
import           Music.Score.Export.Common

{-
  Assume that Music is a type function that returns the underlying music
  representation for a given backend.

  Then, for each backend B we need to provide a function
    s a -> Music B
  where s is some score-like type constructor, and a is some note-like type.
  From a we need to fetch each aspect:
    pitch
    dynamic
    articulation
    part
  and convert it to the relevant representation of that aspect in B.
  For example with lilypond we need to convert to LilypondPitch, LilypondDynamic etc.
  Then we need to take s and convert it into some kind of fold for the musical types
  (usually a set of parallel, seequential compositions). Apply the folds, and we're done.
  
  
  
  
  

  chord
  behavior
  tie
  slide

  tremolo
  harmonic
  text
  clef
-}



class Functor (BackendScore b) => HasBackend b where
  -- | The full music representation
  type BackendMusic b :: *

  -- | Score, voice and time structure, with output handled by 'HasBackendScore' 
  type BackendScore b :: * -> *

  -- | Notes, chords and rests, with output handled by 'HasBackendNoteRest' 
  type BackendNoteRest b :: *

  -- | This type may be used to pass context from 'exportScore' to 'exportNote'.
  --   Often will typically include duration, onset or surrounding notes.
  --
  --   If the note export is not context-sensitive, 'Identity' can be used.
  type BackendContext b :: * -> *

  finalizeExport :: b -> BackendScore b (BackendNoteRest b) -> BackendMusic b
  
class (HasBackend b, Functor s) => HasBackendScore b s where
  exportScore :: b -> s a -> BackendScore b (BackendContext b a)
  -- default exportScore :: (BackendContext b ~ Identity) => b -> s a -> BackendScore b (BackendContext b a)
  -- exportScore b = fmap Identity

class (HasBackend b) => HasBackendNoteRest b a where
  exportNote :: b -> BackendContext b a -> BackendNoteRest b

  -- exportNote' :: (BackendContext b ~ Identity) => b -> a -> BackendNoteRest b
  -- exportNote' b x = exportNote b (Identity x)

export :: (HasBackendScore b s, HasBackendNoteRest b a) => b -> s a -> BackendMusic b
export b = finalizeExport b . export'
  where
    -- These commute except for BackendContext

    -- There seems to be a bug in ghc 7.6.3 that allow us to rearrange the two
    -- composed functions, event though the precence of (BackendContext b) clearly
    -- prevents this:
    export' = fmap (exportNote b) . exportScore b
    
    -- The offending version:
    -- export' = exportScore b . fmap (exportNote b)



data Foo
instance HasBackend Foo where
  type BackendScore Foo     = []
  type BackendContext Foo   = Identity
  type BackendNoteRest  Foo = [(Sum Int, Int)]
  type BackendMusic Foo     = [(Sum Int, Int)]
  finalizeExport _ = concat
instance HasBackendScore Foo [] where
  exportScore _ = fmap Identity
instance HasBackendNoteRest Foo Int where
  exportNote _ (Identity p) = [(mempty ,p)]
instance HasBackendNoteRest Foo a => HasBackendNoteRest Foo (DynamicT (Sum Int) a) where
  exportNote b (Identity (DynamicT (d,ps))) = set (mapped._1) d $ exportNote b (Identity ps)

-- main = print $ export (undefined::Foo) [DynamicT (Sum 4::Sum Int,3::Int), pure 1]





-- type Lilypond = Lilypond.Music
toLilypondString :: (HasBackendNoteRest Ly a, HasBackendScore Ly s) => s a -> String
toLilypondString = show . Pretty.pretty . toLilypond

toLilypond :: (HasBackendNoteRest Ly a, HasBackendScore Ly s) => s a -> Lilypond.Music
toLilypond = export (undefined::Ly)


data Ly
data LyScore a = LyScore [[a]] deriving (Functor, Eq, Show)
data LyContext a = LyContext Duration [a] deriving (Functor, Eq, Show)
instance HasBackend Ly where
  type BackendScore Ly = LyScore
  type BackendContext Ly = LyContext
  type BackendNoteRest Ly = Lilypond.Music
  type BackendMusic Ly = Lilypond.Music
  finalizeExport _ (LyScore xs) = pcatLilypond . fmap scatLilypond $ xs

-- instance HasBackendScore Ly Score where
  -- exportScore _ v = 
instance HasBackendScore Ly Voice where
  exportScore _ v = LyScore [map (\(d,x) -> LyContext d [x]) $ view eventsV v]

instance HasBackendNoteRest Ly Integer where
  -- exportNote _ (LyContext d [])  = (^*realToFrac (d*4)) . Lilypond.rest
  exportNote _ (LyContext d [x]) = (^*realToFrac (d*4)) . Lilypond.note  . spellLilypond $ x
  exportNote _ (LyContext d xs)  = (^*realToFrac (d*4)) . Lilypond.chord . fmap spellLilypond $ xs








pcatLilypond :: [Lilypond] -> Lilypond
pcatLilypond = pcatLilypond' False

pcatLilypond' :: Bool -> [Lilypond] -> Lilypond
pcatLilypond' p = foldr Lilypond.simultaneous e
    where
        e = Lilypond.Simultaneous p []

scatLilypond :: [Lilypond] -> Lilypond
scatLilypond = foldr Lilypond.sequential e
    where
        e = Lilypond.Sequential []

spellLilypond :: Integer -> Lilypond.Note
spellLilypond a = Lilypond.NotePitch (spellLilypond' a) Nothing

spellLilypond' :: Integer -> Lilypond.Pitch
spellLilypond' p = Lilypond.Pitch (
    toEnum $ fromIntegral pc,
    fromIntegral alt,
    fromIntegral oct
    )
    where (pc,alt,oct) = spellPitch (p + 72)





main = putStrLn $ toLilypondString $ (pure (-1) :: Voice Integer) <> d <> e
