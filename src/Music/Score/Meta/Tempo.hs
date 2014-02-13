
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds,
    FlexibleContexts, 
    GADTs, 
    ViewPatterns,
    TypeFamilies,
    MultiParamTypeClasses, 
    FlexibleInstances #-}

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
        
        -- * Extracting tempo
        renderTempo,
  ) where


import Control.Lens
import Control.Arrow
import Control.Monad.Plus       
import Data.Default
import Data.Void
import Data.Maybe
import Data.Semigroup
import Data.Monoid.WithSemigroup
import Data.Typeable
import Data.VectorSpace
import Data.AffineSpace
import Data.String
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Music.Time
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Meta
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Util
import Music.Pitch.Literal

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

instance Show Tempo where
    show (getTempo -> (nv, bpm)) = "metronome " ++ showR nv ++ " " ++ showR bpm
        where
            showR (realToFrac -> (unRatio -> (x, 1))) = show x
            showR (realToFrac -> (unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"

instance Default Tempo where
    def = metronome (1/1) 60

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
tempo :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Tempo -> a -> a
tempo c x = tempoDuring (era x) c x

-- | Set the tempo of the given part of a score.
tempoDuring :: (HasMeta a, HasPart' a) => Span -> Tempo -> a -> a
tempoDuring s c = addGlobalMetaNote (s =: (optionFirst c))


-- TODO move
inSpan :: Span -> Time -> Bool
inSpan (view range -> (t,u)) x = t <= x && x <= u

inSpan' (view range -> (t,u)) x = t <= x && x < u

-- TODO move
final :: Reactive a -> a
final (renderR -> (i,[])) = i
final (renderR -> (i,xs)) = snd $ last xs

-- TODO move
trim :: Monoid a => Span -> Reactive a -> Reactive a
trim (view range -> (t,u)) = since t . Music.Time.Reactive.until u


-- | Split a reactive into notes, as well as the values before and after the first/last update
-- TODO fails if not positive
-- TODO same as reactiveToVoice
reactiveIn :: Span -> Reactive a -> [Note a]
reactiveIn s r 
    | duration s <= 0 = error "reactiveIn: Needs positive duration"
    | otherwise       = let r2 = trim s (fmap optionFirst r)
    in fmap (fmap $ fromJust . unOptionFirst) $ case updates r2 of
        -- We have at least 2 value because of trim
        (frl -> ((t,x),[],(u,_))) -> [t <-> u =: x] -- one note
        (frl -> ((t0,x0), unzip -> (tn,xn), (tl,_))) -> let
            times  = [t0] ++ tn
            spans  = mapWithNext (\t mu -> t <-> fromMaybe tl mu) times
            values = [x0] ++ xn
            in zipWith (=:) spans values





-- | Extract all tempi from the given score, using the given default tempo. 
-- withTempo :: (Tempo -> Score a -> Score a) -> Score a -> Score a
-- withTempo f = withGlobalMeta (f . fromMaybe def . fmap getFirst . getOption)

renderTempo :: Score a -> Score a
renderTempo sc = 
    flip composed sc
        $ fmap renderTempoScore 
        $ tempoRegions (era sc) 
        $ tempoRegions0 (era sc) 
        $ getTempoChanges defTempo sc
    where         
-- | Standard tempo
--
-- > tempoToDuration defTempo == 1
defTempo :: Tempo
defTempo = metronome (1/1) 60 

getTempoChanges :: Tempo -> Score a -> Reactive Tempo
getTempoChanges def = fmap (fromMaybe def . unOptionFirst) . runMeta (Nothing::Maybe Int) . getScoreMeta


-- | Get all tempo regions for the given span.
tempoRegions0 :: Span -> Reactive Tempo -> [TempoRegion0]
tempoRegions0 s r = fmap f $ s `reactiveIn` r
    where
        f (getNote -> (view delta -> (t,u),x)) = TempoRegion0 t u (tempoToDuration x)

tempoRegions :: Span -> [TempoRegion0] -> [TempoRegion]
tempoRegions s = snd . List.mapAccumL f (onset s, onset s) -- XXX offset?
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
renderTempoSpan tr = over range $ \(t,u) -> 
    if inSpan' (tempoRegionNotated tr) t
        then (renderTempoTime' tr t, renderTempoTime' tr u)
        else (t, u)

-- TODO use lens
renderTempoScore :: TempoRegion -> Score a -> Score a
renderTempoScore tr = over notes $ fmap $ over (note_ . _1) $ renderTempoSpan tr 
                                            

data TempoRegion0 = 
    TempoRegion0 {
        notatedOnset0 :: Time,
        notatedDuration0 :: Duration,
        stretching0 :: Duration
    } 


data TempoRegion = 
    TempoRegion {
        notatedOnset :: Time,           -- same
        notatedOffset :: Time,          -- notOns + notDur
        soundingOnset :: Time,          -- sum of previous sounding durations
        stretching :: Duration          -- same
    } 
tempoRegionNotated (TempoRegion t u _ _) = t <-> u

-- TODO add to Music.Score.Note
note_ :: Iso (Note a) (Note b) (Span, a) (Span, b)
note_ = iso getNote (uncurry (=:))

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


-- TODO consolidate
optionFirst = Option . Just . First
unOptionFirst = fmap getFirst . getOption

-- TODO move
frl []  = error "frl: No value"
frl [x] = error "frl: Just one value"
frl xs  = (head xs, (tail.init) xs, last xs)
