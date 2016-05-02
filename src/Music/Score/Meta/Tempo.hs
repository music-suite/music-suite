
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

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
-- Provides tempo meta-data.
--
-------------------------------------------------------------------------------------

module Music.Score.Meta.Tempo (
        -- * Tempo type
        Bpm,
        NoteValue,
        Tempo,
        metronome,
        tempoNoteValue,
        tempoBeatsPerMinute,
        getTempo,
        tempoToDuration,

        -- * Adding tempo to scores
        tempo,
        tempoDuring,

        -- ** Common tempi
        presto,
        allegro,
        allegretto,
        moderato,
        andante,
        adagio,
        largo,
        lento,

        -- * Extracting tempo
        renderTempo,
  ) where


import           Control.Lens
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.Foldable             (Foldable)
import qualified Data.Foldable             as F
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.String
import           Data.Traversable          (Traversable)
import qualified Data.Traversable          as T
import           Data.Typeable
import           Data.VectorSpace

import           Music.Pitch.Literal
import           Music.Score.Meta
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Internal.Util
import           Music.Time                hiding (time)

type Bpm       = Duration
type NoteValue = Duration

-- | Represents musical tempo as a scaling factor with an optional name and/or beat duration.
--
-- 'tempoToDuration' provides a scaling factor such that
--
-- > stretch (tempoToDuration t) notation = sounding
-- > compress (tempoToDuration t) sounding = notation
--
-- You can construct a tempo in various ways
--
-- > tempoToDuration (metronome (1/4) 120) == tempoToDuration (metronome (1/2) 60)
-- > metronome (1/4) 120                   /=                  metronome (1/2) 60

data Tempo = Tempo (Maybe String) (Maybe Duration) Duration
    deriving (Eq, Ord, Typeable)
-- The internal representation is actually: maybeName maybeDisplayNoteValue scalingFactor

-- TODO
instance Num Tempo where
    fromInteger = Tempo Nothing Nothing . fromInteger

instance Show Tempo where
    show (getTempo -> (nv, bpm)) = "metronome " ++ showR nv ++ " " ++ showR bpm
        where

showR = showR' . realToFrac
showR' ((unRatio -> (x, 1))) = show x
showR' ((unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"

{-
instance Default Tempo where
    def = mempty
-}

instance Semigroup Tempo where
    (<>) = mappend

instance Monoid Tempo where
    mempty = metronome (1/4) 120
    a `mappend` b
      | a == mempty = b
      | b == mempty = a
      | otherwise   = a

-- | Create a tempo from a duration and a number of beats per minute.
--
--   For example @metronome (1/2) 48@ means 48 half notes per minute.
metronome :: Duration -> Bpm -> Tempo
metronome noteVal bpm = Tempo Nothing (Just noteVal) $ 60 / (bpm * noteVal)


-- TODO use lenses
--
-- noteValue :: Lens' Tempo (Maybe NoteValue)
-- noteValue = lens g s
--   where
--     g (Tempo n nv d)    = nv
--     s (Tempo n _  d) nv = Tempo n nv d
--
-- bpm :: Lens' Tempo Bpm
-- bpm = lens g s
--   where
--     g (Tempo n nv d)    = nv
--     s (Tempo n _  d) nv = Tempo n nv d


-- | Get the note value indicated by a tempo.
tempoNoteValue :: Tempo -> Maybe NoteValue
tempoNoteValue (Tempo n nv d) = nv

-- | Get the number of beats per minute indicated by a tempo.
tempoBeatsPerMinute :: Tempo -> Bpm
tempoBeatsPerMinute = snd . getTempo

-- | Get the note value and number of beats per minute indicated by a tempo.
--
-- Typically used with the @ViewPatterns@ extension, as in
--
-- > foo (getTempo -> (nv, bpm)) = ...
--
getTempo :: Tempo -> (NoteValue, Bpm)
getTempo (Tempo _ Nothing x)   = (1, (60 * recip x) / 1) -- assume whole note
getTempo (Tempo _ (Just nv) x) = (nv, (60 * recip x) / nv)

-- | Convert a tempo to a duration suitable for converting written to sounding durations.
--
-- > stretch (tempoToDuration t) notation = sounding
-- > compress (tempoToDuration t) sounding = notation
--
tempoToDuration :: Tempo -> Duration
tempoToDuration (Tempo _ _ x) = x

-- | Set the tempo of the given score.
tempo :: (HasMeta a, HasPosition a) => Tempo -> a -> a
tempo c x = tempoDuring (_era x) c x

-- | Set the tempo of the given part of a score.
tempoDuring :: HasMeta a => Span -> Tempo -> a -> a
tempoDuring s c = addMetaNote $ view event (s, c)


-- TODO move
inSpan :: Span -> Time -> Bool
inSpan (view onsetAndOffset -> (t,u)) x = t <= x && x <= u

inSpan' (view onsetAndOffset -> (t,u)) x = t <= x && x < u

mkNote s x = view note (s, x)



-- | Extract all tempi from the given score, using the given default tempo.
-- withTempo :: (Tempo -> Score a -> Score a) -> Score a -> Score a
-- withTempo f = withMeta (f . fromMaybe def . fmap getFirst . getOption)

renderTempo :: Score a -> Score a
renderTempo = error "renderTempo: Not implemented"
{-

-- | Split a reactive into notes, as well as the values before and after the first/last update
-- TODO fails if not positive
-- TODO consolidate
reactiveIn :: Span -> Reactive a -> [Note a]
reactiveIn s r
    | _duration s <= 0 = error "reactiveIn: Needs positive duration"
    | otherwise       = let r2 = trimR s (fmap optionFirst r)
    in fmap (fmap $ fromJust . unOptionFirst) $ case updates r2 of
        -- We have at least 2 value because of trim
        (frl -> ((t,x),[],(u,_))) -> [view note (t <-> u, x)] -- one note
        (frl -> ((t0,x0), unzip -> (tn,xn), (tl,_))) -> let
            times  = [t0] ++ tn
            spans  = mapWithNext (\t mu -> t <-> fromMaybe tl mu) times
            values = [x0] ++ xn
            in zipWith mkNote spans values

renderTempo sc =
    flip composed sc $ fmap renderTempoScore
        $ tempoRegions (_era sc)
        $ tempoRegions0 (_era sc)
        $ getTempoChanges defTempo sc

renderTempoTest :: Score a -> [TempoRegion]
renderTempoTest sc = id
    $ tempoRegions (_era sc)
    $ tempoRegions0 (_era sc)
    $ getTempoChanges defTempo sc


-- | Standard tempo
--
-- > tempoToDuration defTempo == 1
defTempo :: Tempo
defTempo = metronome (1/1) 60

getTempoChanges :: Tempo -> Score a -> Reactive Tempo
getTempoChanges def = fmap (fromMaybe def . unOptionFirst) . fromMetaReactive (Nothing::Maybe Int) . (view meta)


-- | Get all tempo regions for the given span.
tempoRegions0 :: Span -> Reactive Tempo -> [TempoRegion0]
tempoRegions0 s r = fmap f $ s `reactiveIn` r
    where
        f (view (from note) -> (view delta -> (t,u),x)) = TempoRegion0 t u (tempoToDuration x)

tempoRegions :: Span -> [TempoRegion0] -> [TempoRegion]
tempoRegions s = snd . List.mapAccumL f (s^.onset, s^.onset) -- XXX offset?
    where
        f (nt,st) (TempoRegion0 _ d x) = ((nt .+^ d, st .+^ (d*x)),
            TempoRegion nt (nt .+^ d) st x
            )





-- | Return the sounding position of the given notated position, given its tempo region.
--   Does nothing if the given point is outside the given region.
renderTempoTime :: TempoRegion -> Time -> Time
renderTempoTime (TempoRegion notRegOn notRegOff soRegOn str) t
    | notRegOn <= t && t < notRegOff = soRegOn .+^ (t .-. notRegOn) ^* str
    | otherwise                      = t

renderTempoTime' (TempoRegion notRegOn notRegOff soRegOn str) t  = soRegOn .+^ ((t .-. notRegOn) ^* str)

renderTempoSpan :: TempoRegion -> Span -> Span
renderTempoSpan tr = over onsetAndOffset $ \(t,u) ->
    if inSpan' (tempoRegionNotated tr) t
        then (renderTempoTime' tr t, renderTempoTime' tr u)
        else (t, u)

-- TODO use lens
renderTempoScore :: TempoRegion -> Score a -> Score a
renderTempoScore tr = over notes $ fmap $ over (from note . _1) $ renderTempoSpan tr


data TempoRegion0 =
    TempoRegion0 {
        notatedOnset0    :: Time,
        notatedDuration0 :: Duration,
        stretching0      :: Duration
    }
    deriving (Eq, Ord, Show)

data TempoRegion =
    TempoRegion {
        notatedOnset  :: Time,           -- same
        notatedOffset :: Time,          -- notOns + notDur
        soundingOnset :: Time,          -- sum of previous sounding durations
        stretching    :: Duration          -- same
    }
    deriving (Eq, Ord, Show)

tempoRegionNotated (TempoRegion t u _ _) = t <-> u
-}

-- span :: Iso (Note a) (Note b) Span Span

{-
    A "tempo region" is a consecutive span in which the tempo is constant (obtained by @renderR tempo@)

    Tempo region:
        - Its offset is the sum of the duration of all the previous regions
        - Its scaling is simply (tempoToDuration tempo)
        - Its duration is (scaling `stretch` notatedDuration)

    To "render tempo" for a time point:
        - Its position is the offset in its tempo region + the offset of the tempo region

    To "render tempo" for a span:
        - Its onset and offset are rendered separately
        - Its duration is (offset - onset) as per the duration law




-}

-- presto     = metronome (1/4) 125
-- allegro    = metronome (1/4) 115
-- allegretto = metronome (1/4) 105
-- moderato   = metronome (1/4) 95
-- andante    = metronome (1/4) 85
-- adagio     = metronome (1/4) 65
-- largo      = metronome (1/4) 55
-- lento      = metronome (1/4) 45

presto      = metronome (1/4) 140
allegro     = metronome (1/4) 128
allegretto  = metronome (1/4) 118
moderato    = metronome (1/4) 98
andante     = metronome (1/4) 84
adagio      = metronome (1/4) 64
largo       = metronome (1/4) 48
lento       = metronome (1/4) 42

-- TODO consolidate
-- optionFirst = Option . Just . First
-- unOptionFirst = fmap getFirst . getOption

-- TODO move
frl []  = error "frl: No value"
frl [x] = error "frl: Just one value"
frl xs  = (head xs, (tail.init) xs, last xs)

-- JUNK
