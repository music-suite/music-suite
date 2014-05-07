
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Score.Export.Lilypond (
        Lilypond,
        HasLilypond(..),

        toLilypond,
        toLilypondString,

        showLilypond,
        openLilypond,
        writeLilypond,

        -- * Options
        LilypondOptions(..),
        writeLilypond',
        openLilypond',
  ) where

import           Prelude                      hiding (concat, concatMap, foldl,
                                               foldr, mapM, maximum, minimum,
                                               sum)

import           Control.Applicative
import           Control.Arrow
import           Control.Lens                 hiding (rewrite)
import           Control.Monad                hiding (mapM)
import           Data.AffineSpace
import           Data.Default
import           Data.Foldable
import           Data.Function                (on)
import           Data.Maybe                   (fromMaybe)
import           Data.Ord                     (comparing)
import           Data.Ratio
import           Data.Semigroup
import           Data.String
import           Data.Traversable
import           Data.Typeable
import           Data.VectorSpace             hiding (Sum)
import           System.Process

import           Music.Dynamics.Literal
import           Music.Score.Convert (reactiveToVoice')
import           Music.Pitch.Literal
import           Music.Score.Articulation
import           Music.Score.Clef
import           Music.Score.Meta
import           Music.Score.Dynamics
import           Music.Score.Export.Common
import           Music.Score.Instances
import           Music.Score.Meta.Attribution
import           Music.Score.Meta.Clef
import           Music.Score.Meta.Time
import           Music.Score.Meta.Title
import           Music.Score.Ornaments
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Rhythm
import           Music.Score.Ties
import           Music.Score.Util
import           Music.Time hiding (time)

import qualified Codec.Midi                   as Midi
import qualified Data.List                    as List
import qualified Data.Map                     as Map
import qualified Music.Lilypond               as Lilypond
import qualified Music.MusicXml.Simple        as Xml
import qualified Text.Pretty                  as Pretty


type Lilypond = Lilypond.Music

-- |
-- Class of types that can be converted to Lilypond.
--
class Tiable a => HasLilypond a where

    -- |
    -- Convert a value to a Lilypond music expression.
    --
    getLilypond      :: Duration -> a -> Lilypond

    getLilypondChord :: Duration -> [a] -> Lilypond
    getLilypondChord d = pcatLilypond . fmap (getLilypond d)

    getLilypondWithPrefix      :: Duration -> a -> (Lilypond -> Lilypond,Lilypond)
    getLilypondWithPrefix d x = (id, getLilypond d x)

    getLilypondChordWithPrefix :: Duration -> [a] -> (Lilypond -> Lilypond,Lilypond)
    getLilypondChordWithPrefix d x = (id, getLilypondChord d x)

instance HasLilypond Int                        where   getLilypond d = getLilypond d . toInteger
instance HasLilypond Float                      where   getLilypond d = getLilypond d . toInteger . round
instance HasLilypond Double                     where   getLilypond d = getLilypond d . toInteger . round
instance Integral a => HasLilypond (Ratio a)    where   getLilypond d = getLilypond d . toInteger . round

instance HasLilypond Integer where
    getLilypond      d = (^*realToFrac (d*4)) . Lilypond.note  . spellLilypond
    getLilypondChord d = (^*realToFrac (d*4)) . Lilypond.chord . fmap spellLilypond

instance HasLilypond a => HasLilypond [a] where
    getLilypond d = getLilypondChord d

instance HasLilypond a => HasLilypond (Sum a) where
    getLilypond d (Sum a) = getLilypond d a

instance HasLilypond a => HasLilypond (Product a) where
    getLilypond d (Product a) = getLilypond d a

-- instance HasLilypond a => HasLilypond (ChordT a) where
--     getLilypond d = getLilypondChord d . getChordT
-- 
instance HasLilypond a => HasLilypond (PartT n a) where
  getLilypond d (PartT (_,x))                     = getLilypond d x

instance HasLilypond a => HasLilypond (TieT a) where
    getLilypond d (TieT ((Any ta, Any tb),x)) = addTies $ getLilypond d x
        where
            addTies | ta && tb                      = id . Lilypond.beginTie
                    | tb                            = Lilypond.beginTie
                    | ta                            = id
                    | otherwise                     = id


{-
  The instance for (HasLilypond (DynamicT ...)) must look at context for cresc/dim etc to work
  If DynamicT is part of a transformer stack passed to toLilypond, then it does not include context

  Solution 1:
    Include context in the type used in the stack (used in toLilypond)
    Context starts out empty and is filled in "dynamically" before the notation is rendered
    GOOD easy
    BAD  not very elegant
    BAD  forced to pass around empty contexts
  
  Solution 2:
    Add an extra type Dynamic2T
    BAD  to much code, ugly
  
  Solution 3:
    Replace the dynamic type
    Add a contraint to toLilypond like (Dynamic a ~ Double) or similar
    Run 'addDynCon' to add the context (i.e. the dynamic type changes)
    What to do with the (HasLilypond (DynamicT ...)) instance?
-}

type DynType = Behavior (Product Double)


-- This is not used in the actual export
instance (HasLilypond a) => HasLilypond (DynamicT DynType a) where
  getLilypond d (DynamicT (n, a)) = notate n $ getLilypond d a
    where
      notate n = Lilypond.addText (show n)


instance (HasLilypond a) => HasLilypond (DynamicT (Ctxt DynType) a) where
  getLilypond d (DynamicT (n, a)) = notate (mapCtxt (! 0) n) $ getLilypond d a
    where
      notate ctxt = notateDD (extractCtxt ctxt) (dynamicDisplay ctxt)

notateDD :: (Ord a, Real a) => a -> ([CrescDim], ShowDyn) -> Lilypond -> Lilypond
notateDD lvl (cds, showLevel) = (composed $ fmap notateCrescDim cds) . notateLevel
  where         
    notateCrescDim x = case x of
      NoCrescDim -> id
      BeginCresc -> Lilypond.beginCresc
      EndCresc   -> Lilypond.endCresc
      BeginDim   -> Lilypond.beginDim
      EndDim     -> Lilypond.endDim
    
    -- TODO these literals are not so nice...
    notateLevel = if not showLevel then id else 
        Lilypond.addDynamics (fromDynamics (DynamicsL (Just . fixLevel . realToFrac $ lvl, Nothing)))
    



fixLevel :: Double -> Double
fixLevel x = (fromIntegral $ round (x - 0.5)) + 0.5

data CrescDim = NoCrescDim | BeginCresc | EndCresc | BeginDim | EndDim

instance Monoid CrescDim where
  mempty = NoCrescDim
  mappend a _ = a

type ShowDyn  = Bool

mapCtxt :: (a -> b) -> Ctxt a -> Ctxt b
mapCtxt f (a,b,c) = (fmap f a, f b, fmap f c)

extractCtxt :: Ctxt a -> a
extractCtxt (_,x,_) = x

-- Given a dynamic value and its context, decide:
--   
--   1) Whether we should begin or end a crescendo or diminuendo
--   2) Whether we should display the current dynamic value
--   
dynamicDisplay :: (Ord a, Real a) => Ctxt a -> ([CrescDim], ShowDyn)
dynamicDisplay x = case x of
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


    -- getLilypond d (DynamicT (n, a)) = notate $ getLilypond d a
    --   where
    --     notate = case n of
    --       lvl -> Lilypond.addDynamics (fromDynamics (DynamicsL (Just . fixLevel . realToFrac $ lvl, Nothing)))
    --     
    --     fixLevel :: Double -> Double
    --     fixLevel x = (fromIntegral $ round (x - 0.5)) + 0.5
          
-- 
-- instance HasLilypond a => HasLilypond (DynamicT a) where
--     getLilypond d (DynamicT (((Any ec,Any ed),Option l,(Any bc,Any bd)), a)) = notate $ getLilypond d a
--         where
--             notate x = nec . ned . nl . nbc . nbd $ x
--             nec    = if ec then Lilypond.endCresc    else id
--             ned    = if ed then Lilypond.endDim      else id
--             nbc    = if bc then Lilypond.beginCresc  else id
--             nbd    = if bd then Lilypond.beginDim    else id
--             nl     = case l of
--                 Nothing          -> id
--                 Just (First lvl) -> Lilypond.addDynamics (fromDynamics (DynamicsL (Just lvl, Nothing)))
-- 
-- instance HasLilypond a => HasLilypond (ArticulationT a) where
--     getLilypond d (ArticulationT (((Any es, Any us, Any bs), (Sum al, Sum sl)), a)) = notate $ getLilypond d a
--         where
--             notate = nes . nal . nsl . nbs
--             nes    = if es then Lilypond.endSlur else id
--             nal    = case al of
--                 0    -> id
--                 1    -> Lilypond.addAccent
--                 2    -> Lilypond.addMarcato
--             nsl    = case sl of
--                 (-2) -> Lilypond.addTenuto
--                 (-1) -> Lilypond.addPortato
--                 0    -> id
--                 1    -> Lilypond.addStaccato
--                 2    -> Lilypond.addStaccatissimo
--             nbs    = if bs then Lilypond.beginSlur else id
-- 
instance HasLilypond a => HasLilypond (TremoloT a) where
    getLilypond d (TremoloT (Max 0, x)) = getLilypond d x
    getLilypond d (TremoloT (Max n, x)) = notate $ getLilypond newDur x
        where
            scale   = 2^n
            newDur  = (d `min` (1/4)) / scale
            repeats = d / newDur
            notate = Lilypond.Tremolo (round repeats)

instance HasLilypond a => HasLilypond (TextT a) where
    getLilypond d (TextT (s,x)) = notate s $ getLilypond d x
        where
            notate ts = foldr (.) id (fmap Lilypond.addText ts)

instance HasLilypond a => HasLilypond (HarmonicT a) where
    getLilypond d (HarmonicT ((view _Wrapped' -> isNat, view _Wrapped' -> n),x)) = notate isNat n $ getLilypond d x
        where
            notate _     0 = id
            notate True  n = notateNatural n
            notate False n = notateArtificial n

            notateNatural n = Lilypond.addFlageolet -- addOpen?

            notateArtificial n = id -- TODO

instance HasLilypond a => HasLilypond (SlideT a) where
    getLilypond d (SlideT (((eg,es),(bg,bs)),a)) = notate $ getLilypond d a
        where
            notate = if view _Wrapped' bg || view _Wrapped' bs then Lilypond.beginGlissando else id

instance HasLilypond a => HasLilypond (ClefT a) where
    -- TODO consolidate
    getLilypondWithPrefix d (ClefT (c, a)) = (notate c, getLilypond d a)
        where
            notate c = case fmap getLast $ getOption c of
                Nothing -> id
                Just c -> \x -> Lilypond.Sequential [addClef c, x]
    getLilypond d           (ClefT (c, a)) = notate c $ getLilypond d a
        where
            notate c = case fmap getLast $ getOption c of
                Nothing -> id
                Just c -> \x -> Lilypond.Sequential [addClef c, x]

instance HasLilypond a => HasLilypond (Behavior a) where
    getLilypond d      = getLilypond d      . (! 0)
    getLilypondChord d = getLilypondChord d . fmap (! 0)


-- TODO
addClef GClef = Lilypond.Clef Lilypond.Treble
addClef CClef = Lilypond.Clef Lilypond.Alto
addClef FClef = Lilypond.Clef Lilypond.Bass


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


-- |
-- Convert a score to a Lilypond representaiton and print it on the standard output.
--
showLilypond :: (HasLilypond2 a, HasPart2 a, Semigroup a) => Score a -> IO ()
showLilypond = putStrLn . toLilypondString

-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLilypond :: (HasLilypond2 a, HasPart2 a, Semigroup a) => FilePath -> Score a -> IO ()
writeLilypond = writeLilypond' def

data LilypondOptions
    = Inline
    | Score
instance Default LilypondOptions where
    def = Inline

-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLilypond' :: (HasLilypond2 a, HasPart2 a, Semigroup a) => LilypondOptions -> FilePath -> Score a -> IO ()
writeLilypond' options path sc = writeFile path $ (lyFilePrefix ++) $ toLilypondString sc
    where
        title    = fromMaybe "" $ flip getTitleAt 0                  $ metaAtStart sc
        composer = fromMaybe "" $ flip getAttribution "composer"     $ metaAtStart sc

        lyFilePrefix = case options of
            Inline -> lyInlinePrefix
            Score  -> lyScorePrefix

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
-- Typeset a score using Lilypond and open it.
--
openLilypond :: (HasLilypond2 a, HasPart2 a, Semigroup a) => Score a -> IO ()
openLilypond = openLilypond' def

openLilypond' :: (HasLilypond2 a, HasPart2 a, Semigroup a) => LilypondOptions -> Score a -> IO ()
openLilypond' options sc = do
    writeLilypond' options "test.ly" sc
    runLilypond
    cleanLilypond
    openLilypond''

runLilypond    = void $ runCommand "lilypond -f pdf test.ly 2>/dev/null" >>= waitForProcess
cleanLilypond  = void $ runCommand "rm -f test-*.tex test-*.texi test-*.count test-*.eps test-*.pdf test.eps"
openLilypond'' = void $ runCommand $ openCommand ++ " test.pdf"

-- |
-- Convert a score to a Lilypond string.
--
toLilypondString :: (HasLilypond2 a, HasPart2 a, Semigroup a) => Score a -> String
toLilypondString = show . Pretty.pretty . toLilypond

-- |
-- Convert a score to a Lilypond representation.
--
toLilypond :: (HasLilypond2 a, HasPart2 a, Semigroup a) => Score a -> Lilypond
toLilypond sc =
          -- Score structure
          pcatLilypond . fmap (
                addStaff . scatLilypond . uncurry addPartName

                -- Main notation pipeline
                . second (voiceToLilypond barTimeSigs barDurations . temporaryClefFix . toMVoice)

                -- Meta-event expansion
                 . uncurry addClefs . second addDynCon2
                )

        . extractParts' $ sc

    where
        -- TODO temporary to make most tests pass
        temporaryClefFix = over (_Wrapped._head.traverse.traverse) $ applyClef GClef
        -- temporaryClefFix = id

        addClefT :: a -> ClefT a
        addClefT = return

        addClefs p = (,) p . setClef . fmap addClefT
        setClef = withClef def $ \c x -> applyClef c x where def = GClef -- TODO use part default

        timeSigs = getTimeSignatures (time 4 4) sc -- 4/4 is default
        timeSigsV = fmap swap $ unvoice $ fuse $ reactiveToVoice' (0 <-> _offset sc) timeSigs

        -- Despite the fuse above we need retainUpdates here to prevent redundant repetition of time signatures
        barTimeSigs  = retainUpdates $ getBarTimeSignatures $ timeSigsV
        barDurations =                 getBarDurations      $ timeSigsV


        -- getTimeSignatures def       =           fmap (fromMaybe def . unOptionFirst) . runMeta (Nothing::Maybe Int) . getScoreMeta
        -- getTimeSignatureChanges def = updates . fmap (fromMaybe def . unOptionFirst) . runMeta (Nothing::Maybe Int) . getScoreMeta

        addStaff = Lilypond.New "Staff" Nothing
        addPartName partName x = Lilypond.Set "Staff.instrumentName" (Lilypond.toValue $ show partName)
            : Lilypond.Set "Staff.shortInstrumentName" (Lilypond.toValue $ show partName)
            : x

mergeBars :: [Lilypond] -> Lilypond
mergeBars [x] = x
mergeBars _   = error "mergeBars: Not supported"

-- |
-- Convert a voice score to a list of bars.
--
voiceToLilypond :: HasLilypond15 a => [Maybe TimeSignature] -> [Duration] -> Voice (Maybe a) -> [Lilypond]
voiceToLilypond barTimeSigs barDurations = zipWith setBarTimeSig barTimeSigs . fmap barToLilypond . voiceToBars' barDurations
--
-- This is where notation of a single voice takes place
--      * voiceToBars is generic for most notations outputs: it handles bar splitting and ties
--      * barToLilypond is specific: it handles quantization and notation
--
    where
        -- FIXME compounds
        setBarTimeSig Nothing x = x
        setBarTimeSig (Just (getTimeSignature -> (m:_, n))) x = scatLilypond [Lilypond.Time m n, x]


barToLilypond :: HasLilypond15 a => [(Duration, Maybe a)] -> Lilypond
barToLilypond bar = case (fmap rewrite . quantize) bar of
    Left e   -> error $ "barToLilypond: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToLilypond rh

rhythmToLilypond = uncurry ($) . rhythmToLilypond2

-- rhythmToLilypond :: HasLilypond2 a => Rhythm (Maybe a) -> Lilypond
-- rhythmToLilypond (Beat d x)            = noteRestToLilypond d x
-- rhythmToLilypond (Dotted n (Beat d x)) = noteRestToLilypond (dotMod n * d) x
-- rhythmToLilypond (Group rs)            = scatLilypond $ map rhythmToLilypond rs
-- rhythmToLilypond (Tuplet m r)          = Lilypond.Times (realToFrac m) (rhythmToLilypond r)
--     where (a,b) = fromIntegral *** fromIntegral $ unRatio $ realToFrac m
--
-- noteRestToLilypond :: HasLilypond2 a => Duration -> Maybe a -> Lilypond
-- noteRestToLilypond d Nothing  = Lilypond.rest^*(realToFrac d*4)
-- noteRestToLilypond d (Just p) = Lilypond.removeSingleChords $ getLilypond d p



rhythmToLilypond2 :: HasLilypond15 a => Rhythm (Maybe a) -> (Lilypond -> Lilypond, Lilypond)
rhythmToLilypond2 (Beat d x)            = noteRestToLilypond2 d x
rhythmToLilypond2 (Dotted n (Beat d x)) = noteRestToLilypond2 (dotMod n * d) x

-- TODO propagate
rhythmToLilypond2 (Group rs)            = first (maybe id id) $ second scatLilypond $ extract1 $ map rhythmToLilypond2 $ rs

rhythmToLilypond2 (Tuplet m r)          = second (Lilypond.Times (realToFrac m)) $ (rhythmToLilypond2 r)
    where (a,b) = fromIntegral *** fromIntegral $ unRatio $ realToFrac m

noteRestToLilypond2 :: HasLilypond15 a => Duration -> Maybe a -> (Lilypond -> Lilypond, Lilypond)
noteRestToLilypond2 d Nothing  = ( id, Lilypond.rest^*(realToFrac d*4) )
noteRestToLilypond2 d (Just p) = second Lilypond.removeSingleChords $ getLilypondWithPrefix d p

-- extract first value of type b
extract1 :: [(b, a)] -> (Maybe b, [a])
extract1 []         = (Nothing, [])
extract1 ((p,x):xs) = (Just p, x : fmap snd xs)

spellLilypond :: Integer -> Lilypond.Note
spellLilypond a = Lilypond.NotePitch (spellLilypond' a) Nothing

spellLilypond' :: Integer -> Lilypond.Pitch
spellLilypond' p = Lilypond.Pitch (
    toEnum $ fromIntegral pc,
    fromIntegral alt,
    fromIntegral oct
    )
    where (pc,alt,oct) = spellPitch (p + 72)






-- TODO remove
type HasPart2 a = (HasPart' a, Ord (Part a), Show (Part a))
type HasLilypond15 a = (HasLilypond a, Transformable a)

type HasLilypond2 a = (
  HasLilypond a, 
  Transformable a, 
  HasPart' a,
  HasDynamic    a a,

  HasDynamic    a (SetDynamic (Ctxt (Dynamic a)) a),
  Dynamic       (SetDynamic (Ctxt (Dynamic a)) a) ~ Ctxt (Dynamic a),
  HasLilypond   (SetDynamic (Ctxt (Dynamic a)) a),
  Semigroup     (SetDynamic (Ctxt (Dynamic a)) a),
  HasPart'      (SetDynamic (Ctxt (Dynamic a)) a),
  Transformable (SetDynamic (Ctxt (Dynamic a)) a)
  )


type Ctxt a = (Maybe a, a, Maybe a)

addDynCon2 :: (
  HasDynamic a a, HasDynamic a b, 
  Dynamic b ~ Ctxt (Dynamic a),
  HasPart' a,
  Ord (Part a),
  Transformable a,
  b ~ SetDynamic (Ctxt (Dynamic a)) a
  
  ) => Score a -> Score b
addDynCon2 = addDynCon







type instance Pitch (ClefT a) = Pitch a
type instance SetPitch b (ClefT a) = ClefT (SetPitch b a)

instance (HasPitches a b) => HasPitches (ClefT a) (ClefT b) where
  pitches = _Wrapped . pitches
instance (HasPitch a b) => HasPitch (ClefT a) (ClefT b) where
  pitch = _Wrapped . pitch
