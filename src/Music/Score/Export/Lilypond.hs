
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Score.Export.Lilypond (
    -- * Lilypond backend
    HasLilypondInstrument(..),
    Lilypond,
    LyContext(..),
    HasLilypond,

    -- ** Converting to Lilypond
    toLilypond,
    toLilypondString,

    -- ** Lilypond I/O
    showLilypond,
    openLilypond,
    writeLilypond,

    -- ** Customize Lilypond backend
    LilypondOptions(..),
    openLilypond',
    writeLilypond',
  ) where

import           Control.Applicative
import           Control.Comonad                         (Comonad (..), extract)
import           Control.Lens                            hiding (rewrite)
import           Control.Monad
import           Data.AffineSpace
import           Data.Bifunctor
import           Data.Colour.Names                       as Color
import           Data.Either
import           Data.Foldable                           (Foldable)
import           Data.Functor.Adjunction                 (unzipR)
import           Data.Functor.Context
-- import           Data.Functor.Contravariant
import           Data.Functor.Couple
import qualified Data.List
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup
import           Data.Traversable                        (Traversable,
                                                          sequenceA)
import           Data.VectorSpace                        hiding (Sum (..))
import           System.Process
import qualified Text.Pretty                             as Pretty

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Articulation
import           Music.Score.Color
import           Music.Score.Dynamics
import           Music.Score.Export.ArticulationNotation
import           Music.Score.Export.Backend
import           Music.Score.Export.Backend
import           Music.Score.Export.DynamicNotation
import           Music.Score.Harmonics
import           Music.Score.Internal.Export             hiding (MVoice)
import           Music.Score.Internal.Util               (composed,
                                                          retainUpdates, swap,
                                                          unRatio, withPrevNext)
import           Music.Score.Meta
import           Music.Score.Meta.Time
import           Music.Score.Meta.Title
import           Music.Score.Meta.Attribution
import           Music.Score.Part
import           Music.Score.Phrases
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Tremolo
import           Music.Time
import           Music.Score.Internal.Quantize
import           Music.Score.Internal.Data               (getData)

import qualified Data.Music.Lilypond                     as Lilypond







#define COMPLEX_POLY_STUFF 1





-- |
-- Extract instrument info as per "Music.Part"
-- This is really crude, needs rethinking!
--
class HasLilypondInstrument a where
  getLilypondClef :: a -> Int


{-
  TODO rewrite so that:
    - Each bar may include voices/layers (a la Sibelius)
    - Each part may generate more than one staff (for piano etc)
-}


-- | A token to represent the Lilypond backend.
data Lilypond

data ScoreInfo = ScoreInfo
  deriving (Eq, Show)

data StaffInfo = StaffInfo { staffName :: String,
                             staffClef :: Lilypond.Clef }
  deriving (Eq, Show)

data BarInfo = BarInfo { barTimeSignature :: Maybe TimeSignature }
  deriving (Eq, Show)

-- | Hierachical representation of a Lilypond score.
--   A score is a parallel composition of staves.
data LyScore a = LyScore { getLyScore :: (ScoreInfo, [LyStaff a]) }
  deriving (Functor, Eq, Show)

-- | A staff is a sequential composition of bars.
data LyStaff a = LyStaff { getLyStaff :: (StaffInfo, [LyBar a]) }
  deriving (Functor, Eq, Show)

-- | A bar is a sequential composition of chords/notes/rests.
data LyBar a = LyBar { getLyBar :: (BarInfo, Rhythm a) }
  deriving (Functor, Eq, Show)

-- | Context passed to the note export.
--   Includes duration and note/rest distinction.
data LyContext a = LyContext Duration (Maybe a)
  deriving (Functor, Foldable, Traversable, Eq, Show)

{-
TODO move
instance Monoid LyMusic where
  mempty      = pcatL []
  mappend x y = pcatL [x,y]
-}

type LyMusic = Lilypond.Music

instance HasBackend Lilypond where
  type BackendScore Lilypond   = LyScore
  type BackendContext Lilypond = LyContext
  type BackendNote Lilypond    = LyMusic
  type BackendMusic Lilypond   = LyMusic

  finalizeExport _ = finalizeScore
    where
      finalizeScore :: LyScore LyMusic -> LyMusic
      finalizeScore (LyScore (info, x)) = pcatL . map finalizeStaff $ x

      -- TODO finalizeStaffGroup

      finalizeStaff :: LyStaff LyMusic -> LyMusic
      finalizeStaff (LyStaff (info, x))
        = addStaff
        . addPartName (staffName info)
        . addClef (staffClef info)
        . scatL . map finalizeBar $ x
        where
          addStaff                = Lilypond.New "Staff" Nothing
          addClef c x             = scatL [Lilypond.Clef c, x]
          addPartName partName xs = scatL [longName, shortName, xs]
            where
              longName  = Lilypond.Set "Staff.instrumentName" (Lilypond.toValue partName)
              shortName = Lilypond.Set "Staff.shortInstrumentName" (Lilypond.toValue partName)

      finalizeBar :: LyBar LyMusic -> LyMusic
      finalizeBar (LyBar (BarInfo timeSignature, x))
        = (setTimeSignature `ifJust` timeSignature)
        . renderBarMusic $ x
        where
          ifJust = maybe id
          -- TODO key signatures
          -- TODO rehearsal marks
          -- TODO bar number change
          -- TODO compound time signatures
          setTimeSignature (getTimeSignature -> (ms, n)) x = scatL [Lilypond.Time (sum ms) n, x]

      renderBarMusic :: Rhythm LyMusic -> LyMusic
      renderBarMusic = go
        where
          go (Beat d x)            = Lilypond.removeSingleChords x
          go (Dotted n (Beat d x)) = Lilypond.removeSingleChords x
          go (Group rs)            = scatL $ map renderBarMusic rs
          go (Tuplet m r)          = Lilypond.Times (realToFrac m) (renderBarMusic r)
            where
              (a,b) = bimap fromIntegral fromIntegral $ unRatio $ realToFrac m

#ifdef COMPLEX_POLY_STUFF
instance (
  HasDynamicNotation a b c,
  HasArticulationNotation c d e,
  Part e ~ Part c,
  HasOrdPart a,
  Transformable a,
  Semigroup a,
  Tiable e, HasOrdPart c, Show (Part c), 
  HasLilypondInstrument (Part c),
  Satisfied
  )
  => HasBackendScore Lilypond (Score a) where
#else
instance (
  Tiable a, HasOrdPart a, Show (Part a), 
  HasLilypondInstrument (Part a),
  Satisfied
  )
  => HasBackendScore Lilypond (Score a) where
#endif

#ifdef COMPLEX_POLY_STUFF
  type BackendScoreEvent Lilypond (Score a) = SetArticulation ArticulationNotation (SetDynamic DynamicNotation a)
#else
  type BackendScoreEvent Lilypond (Score a) = a
#endif

  exportScore b score = LyScore
    . (ScoreInfo,)
    -- Store time signatures etc for later use by finalizeScore
    . map (uncurry $ exportPart timeSignatureMarks barDurations)
    . notateAspects . extractPartsWithInfo $ normScore
    where
      -- notateAspects :: [(part,score)]
      notateAspects = id
#ifdef COMPLEX_POLY_STUFF
          -- Notate articulation
          . map (second $ over articulations notateArticulation)
          . map (second $ preserveMeta addArtCon)

          -- Notate dynamics
          . map (second $ removeCloseDynMarks)
          . map (second $ over dynamics notateDynamic)
          . map (second $ preserveMeta addDynCon)

          -- Merge simultaneous chords
          . map (second $ preserveMeta simultaneous)
#endif
      (timeSignatureMarks, barDurations) = extractTimeSignatures normScore
      normScore = normalizeScore score

      -- | Export a score as a single part. Overlapping notes will cause an error.
      exportPart :: (
        Show (Part a),
        HasLilypondInstrument (Part a),
        Tiable a
        )
        => [Maybe TimeSignature]
        -> [Duration]
        -> Part a
        -> Score a
        -> LyStaff (LyContext a)
      exportPart timeSignatureMarks barDurations part
        = exportStaff timeSignatureMarks barDurations (show part) (getLilypondClef part)
        . view singleMVoice

      exportStaff :: Tiable a
        => [Maybe TimeSignature]
        -> [Duration]
        -> String -- ^ name
        -> Int    -- ^ clef, as per Music.Parts
        -> MVoice a
        -> LyStaff (LyContext a)
      exportStaff timeSignatures barDurations name clefId
        = LyStaff . addStaffInfo . zipWith exportBar timeSignatures . splitIntoBars barDurations
        where
          clef = case clefId of
            0 -> Lilypond.Treble
            1 -> Lilypond.Alto
            2 -> Lilypond.Bass
          addStaffInfo  = (,) $ StaffInfo { staffName = name, staffClef = clef }
          splitIntoBars = splitTiesAt

      exportBar :: Tiable a => Maybe TimeSignature -> MVoice a -> LyBar (LyContext a)
      exportBar timeSignature = LyBar . addBarInfo . quantizeBar
       where
         addBarInfo = (,) $ BarInfo timeSignature

      quantizeBar :: Tiable a => MVoice a -> Rhythm (LyContext a)
      quantizeBar = mapWithDur LyContext . rewrite . handleErrors . quantize . view pairs
        where
          -- FIXME propagate quantization errors
          handleErrors (Left e)  = error $ "Quantization failed: " ++ e
          handleErrors (Right x) = x


--------------------------------------------------------------------------------

{-
  Note:
    We want all note transformers to be applicative morphisms, i.e.

      notate (pure x)   = pure (notate x)

    Specifically
      notate (mempty,x) = id . notate x

  Note:
    We use these idioms:
      exportNote b = exportNote b . fmap extract
      exportNote b = uncurry notate . fmap (exportNote b) . getTieT . sequenceA

   The latter looks a lot like cotraverse. Generalization?


-}

instance HasBackendNote Lilypond a => HasBackendNote Lilypond [a] where
  exportNote = exportChord

instance HasBackendNote Lilypond Integer where
  -- TODO can we get rid of exportChord alltogether and just use LyContext?
  exportNote  _ (LyContext d Nothing)    = (^*realToFrac (4*d)) Lilypond.rest
  exportNote  _ (LyContext d (Just x))   = (^*realToFrac (4*d)) $ Lilypond.note $ spellL x

  exportChord _ (LyContext d Nothing)    = (^*realToFrac (4*d)) Lilypond.rest
  exportChord _ (LyContext d (Just xs))  = (^*realToFrac (4*d)) $ Lilypond.chord $ fmap spellL xs

instance HasBackendNote Lilypond Int where
  exportNote b = exportNote b . fmap toInteger
  exportChord b = exportChord b . fmap (fmap toInteger)

instance HasBackendNote Lilypond Float where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance HasBackendNote Lilypond Double where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance Integral a => HasBackendNote Lilypond (Ratio a) where
  exportNote b = exportNote b . fmap (toInteger . round)

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (Behavior a) where
  exportNote b = exportNote b . fmap (! 0)
  exportChord b = exportChord b . fmap (fmap (! 0))

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (Sum a) where
  exportNote b = exportNote b . fmap getSum

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (Product a) where
  exportNote b = exportNote b . fmap getProduct

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (PartT n a) where
  -- Part structure is handled by HasMidiBackendScore instances, so this is just an identity
  exportNote b = exportNote b . fmap extract
  exportChord b = exportChord b . fmap (fmap extract)


#ifdef COMPLEX_POLY_STUFF
instance HasBackendNote Lilypond a => HasBackendNote Lilypond (DynamicT DynamicNotation a) where
  exportNote b = uncurry notate . getDynamicT . fmap (exportNote b) . sequenceA
    where
      notate :: DynamicNotation -> LyMusic -> LyMusic
      notate (DynamicNotation (crescDims, level))
        = rcomposed (fmap notateCrescDim crescDims)
        . notateLevel level

      notateCrescDim crescDims = case crescDims of
        NoCrescDim -> id
        BeginCresc -> Lilypond.beginCresc
        EndCresc   -> Lilypond.endCresc
        BeginDim   -> Lilypond.beginDim
        EndDim     -> Lilypond.endDim

      -- TODO these literals are not so nice...
      notateLevel showLevel = case showLevel of
         Nothing -> id
         Just lvl -> Lilypond.addDynamics (fromDynamics (DynamicsL (Just (fixLevel . realToFrac $ lvl), Nothing)))

      fixLevel :: Double -> Double
      fixLevel x = fromIntegral (round (x - 0.5)) + 0.5

      -- Use rcomposed as notateDynamic returns "mark" order, not application order
      rcomposed = composed . reverse
#else
instance HasBackendNote Lilypond a => HasBackendNote Lilypond (DynamicT b a) where
  exportNote b = exportNote b . fmap extract
#endif

#ifdef COMPLEX_POLY_STUFF
instance HasBackendNote Lilypond a => HasBackendNote Lilypond (ArticulationT ArticulationNotation a) where
  exportNote b = uncurry notate . getArticulationT . fmap (exportNote b) . sequenceA
    where
      notate :: ArticulationNotation -> LyMusic -> LyMusic
      notate (ArticulationNotation (slurs, marks))
        = rcomposed (fmap notateMark marks)
        . rcomposed (fmap notateSlur slurs)

      notateMark mark = case mark of
        NoMark         -> id
        Staccato       -> Lilypond.addStaccato
        MoltoStaccato  -> Lilypond.addStaccatissimo
        Marcato        -> Lilypond.addMarcato
        Accent         -> Lilypond.addAccent
        Tenuto         -> Lilypond.addTenuto

      notateSlur slurs = case slurs of
        NoSlur    -> id
        BeginSlur -> Lilypond.beginSlur
        EndSlur   -> Lilypond.endSlur

      -- Use rcomposed as notateDynamic returns "mark" order, not application order
      rcomposed = composed . reverse
#else
instance HasBackendNote Lilypond a => HasBackendNote Lilypond (ArticulationT {-ArticulationNotation-}b a) where
  exportNote b = exportNote b . fmap extract     
#endif

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (ColorT a) where
  exportNote b = uncurry notate . getCouple . getColorT . fmap (exportNote b) . sequenceA
    where
      -- TODO This syntax will change in future Lilypond versions
      -- TODO handle any color
      notate (Option Nothing)             = id
      notate (Option (Just (Last color))) = \x -> Lilypond.Sequential [
        Lilypond.Override "NoteHead#' color"
          (Lilypond.toLiteralValue $ "#" ++ colorName color),
        x,
        Lilypond.Revert "NoteHead#' color"
        ]

      colorName c
        | c == Color.black = "black"
        | c == Color.red   = "red"
        | c == Color.blue  = "blue"
        | otherwise        = error "Lilypond backend: Unkown color"

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (TremoloT a) where
  -- TODO can this instance use the new shorter idiom?
  exportNote b (LyContext d x) =
    fst (notateTremolo x d) $ exportNote b $ LyContext (snd $ notateTremolo x d) (fmap extract x)


instance HasBackendNote Lilypond a => HasBackendNote Lilypond (TextT a) where
  exportNote b = uncurry notateText . getCouple . getTextT . fmap (exportNote b) . sequenceA
    where


instance HasBackendNote Lilypond a => HasBackendNote Lilypond (HarmonicT a) where
  exportNote b = uncurry notateHarmonic . getCouple . getHarmonicT . fmap (exportNote b) . sequenceA

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (SlideT a) where
  exportNote b  = uncurry notateGliss . getCouple . getSlideT . fmap (exportNote b) . sequenceA
  exportChord b = uncurry notateGliss . getCouple . getSlideT . fmap (exportChord b) . sequenceA . fmap sequenceA


{-
  exportNote        :: b -> BC a   -> BN
  exportChord       :: b -> BC [a] -> BN
  uncurry notateTie :: ((Any, Any), LyMusic) -> LyMusic
  
  BC (TieT a)        sequenceA
  TieT (BC a)        fmap (exportNote b)
  TieT BN            notate . getTieT
  BN
  
  BC [TieT a]        fmap sequenceA
  BC (TieT [a])      sequenceA
  TieT (BC [a])      fmap (exportChord b)
  TieT BN            notate . getTieT
  BN
-}
instance HasBackendNote Lilypond a => HasBackendNote Lilypond (TieT a) where
  exportNote b  = uncurry notateTie . getTieT . fmap (exportNote b) . sequenceA
  exportChord b = uncurry notateTie . getTieT . fmap (exportChord b) . sequenceA . fmap sequenceA


notateTremolo :: RealFrac b => Maybe (TremoloT a) -> b -> (LyMusic -> LyMusic, b)
notateTremolo Nothing d                        = (id, d)
notateTremolo (Just (runTremoloT -> (0, _))) d = (id, d)
notateTremolo (Just (runTremoloT -> (n, _))) d = let
  scale   = 2^n
  newDur  = (d `min` (1/4)) / scale
  repeats = d / newDur
  in (Lilypond.Tremolo (round repeats), newDur)

notateText :: [String] -> LyMusic -> LyMusic
notateText texts = composed (fmap Lilypond.addText texts)

notateHarmonic :: (Eq t, Num t) => (Any, Sum t) -> LyMusic -> LyMusic
notateHarmonic (Any isNat, Sum n) = case (isNat, n) of
  (_,     0) -> id
  (True,  n) -> notateNatural n
  (False, n) -> notateArtificial n
  where
    notateNatural n = Lilypond.addFlageolet -- addOpen?
    notateArtificial n = id -- TODO

notateGliss :: ((Any, Any), (Any, Any)) -> LyMusic -> LyMusic
notateGliss ((Any eg, Any es),(Any bg, Any bs))
  | bg  = Lilypond.beginGlissando
  | bs  = Lilypond.beginGlissando
  | otherwise = id

notateTie :: (Any, Any) -> LyMusic -> LyMusic
notateTie (Any ta, Any tb)
  | ta && tb  = Lilypond.beginTie
  | tb        = Lilypond.beginTie
  | ta        = id
  | otherwise = id

-- |
-- Constraint for types that has a Lilypond representation.
--
type HasLilypond a = (HasBackendNote Lilypond (BackendScoreEvent Lilypond a), HasBackendScore Lilypond a)

-- |
-- Convert a score to a Lilypond representation.
--
toLilypond :: HasLilypond a => a -> LyMusic
toLilypond = export (undefined::Lilypond)

-- |
-- Convert a score to a Lilypond string.
--
toLilypondString :: HasLilypond a => a -> String
toLilypondString = show . Pretty.pretty . toLilypond

-- |
-- Convert a score to a Lilypond representaiton and print it on the standard output.
--
showLilypond :: HasLilypond a => a -> IO ()
showLilypond = putStrLn . toLilypondString

-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLilypond :: HasLilypond a => FilePath -> a -> IO ()
writeLilypond = writeLilypond' mempty

data LilypondOptions
  = LyInlineFormat
  | LyScoreFormat
  | LyLargeScoreFormat

instance Monoid LilypondOptions where
  mempty  = LyInlineFormat
  mappend = const

-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLilypond' :: HasLilypond a => LilypondOptions -> FilePath -> a -> IO ()
writeLilypond' options path sc = writeFile path $ (lyFilePrefix ++) $ toLilypondString sc
  where
    -- title    = fromMaybe "" $ flip getTitleAt 0                  $ metaAtStart sc
    -- composer = fromMaybe "" $ flip getAttribution "composer"     $ metaAtStart sc
    title    = ""
    composer = ""
    -- TODO generalize metaAtStart!

    lyFilePrefix = case options of
        LyInlineFormat -> lyInlinePrefix
        LyScoreFormat  -> lyScorePrefix
        LyLargeScoreFormat -> getData "ly_big_score.ily" -- TODO use files for other headers as well

    lyInlinePrefix = mempty                                        ++
        "%%% Generated by music-score %%%\n"                       ++
        "\\include \"lilypond-book-preamble.ly\"\n"                ++
        "\\paper {\n"                                              ++
        "  #(define dump-extents #t)\n"                            ++
        "\n"                                                       ++
        "  indent = 0\\mm\n"                                       ++
        "  line-width = 210\\mm - 2.0 * 0.4\\in\n"                 ++
        "  ragged-right = ##t\n"                                   ++
        "  force-assignment = #\"\"\n"                             ++
        "  line-width = #(- line-width (* mm  3.000000))\n"        ++
        "}\n"                                                      ++
        "\\header {\n"                                             ++
        "  title = \"" ++ title ++ "\"\n"                          ++
        "  composer = \"" ++ composer ++ "\"\n"                    ++
        "}\n"                                                      ++
        "\\layout {\n"                                             ++
        "}"                                                        ++
        "\n\n"

    lyScorePrefix = mempty                                         ++
        "\\paper {"                                                ++
        "  indent = 0\\mm"                                         ++
        "  line-width = 210\\mm - 2.0 * 0.4\\in"                   ++
        "}"                                                        ++
        "\\header {\n"                                             ++
        "  title = \"" ++ title ++ "\"\n"                          ++
        "  composer = \"" ++ composer ++ "\"\n"                    ++
        "}\n"                                                      ++
        "\\layout {"                                               ++
        "}" ++
        "\n\n"


-- |
-- Typeset a score using Lilypond and open it. (This is simple wrapper around
-- 'writeLilypond' that may not work well on all platforms.)
--
openLilypond :: HasLilypond a => a -> IO ()
openLilypond = openLilypond' mempty

-- |
-- Typeset a score using Lilypond and open it. (This is simple wrapper around
-- 'writeLilypond' that may not work well on all platforms.)
--
openLilypond' :: HasLilypond a => LilypondOptions -> a -> IO ()
openLilypond' options sc = do
  writeLilypond' options "test.ly" sc
  runLilypond >> cleanLilypond >> runOpen
    where
      runLilypond = void $ runCommand
        -- "lilypond -f pdf test.ly"  >>= waitForProcess
        "lilypond -f pdf test.ly 2>/dev/null"  >>= waitForProcess
      cleanLilypond = void $ runCommand
        "rm -f test-*.tex test-*.texi test-*.count test-*.eps test-*.pdf test.eps"
      runOpen = void $ runCommand
        $ openCommand ++ " test.pdf"

































                                                                          


pcatL :: [LyMusic] -> LyMusic
pcatL = pcatL' False

pcatL' :: Bool -> [LyMusic] -> LyMusic
pcatL' p = foldr Lilypond.simultaneous (Lilypond.Simultaneous p [])

scatL :: [LyMusic] -> LyMusic
scatL = foldr Lilypond.sequential (Lilypond.Sequential [])

spellL :: Integer -> Lilypond.Note
spellL a = Lilypond.NotePitch (spellL' a) Nothing

spellL' :: Integer -> Lilypond.Pitch
spellL' p = Lilypond.Pitch (
  toEnum $ fromIntegral pc,
  fromIntegral alt,
  fromIntegral oct
  )
  where (pc,alt,oct) = spellPitch (p + 72)

-- | A constraint that is always satisfied.
type Satisfied = (() ~ ())

-- | A constraint that is never satisfied.
type Unsatisfied = (() ~ Bool)


