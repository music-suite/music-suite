{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing
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
--
-- Provides tempo meta-data.
module Music.Score.Meta.Tempo
  ( -- * Tempo type
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
  )
where

import Control.Lens
import Control.Monad.Plus
import Data.AffineSpace
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Typeable
import Data.VectorSpace
import Music.Pitch.Literal
import Music.Score.Internal.Util
import Music.Score.Meta
import Music.Score.Part
import Music.Score.Pitch
import Music.Time

type Bpm = Duration

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

instance Num Tempo where
  (+) = error "Num Tempo: No implementation"

  (-) = error "Num Tempo: No implementation"

  (*) = error "Num Tempo: No implementation"

  abs = error "Num Tempo: No implementation"

  signum = error "Num Tempo: No implementation"

  fromInteger = Tempo Nothing Nothing . fromInteger

instance Show Tempo where
  show (getTempo -> (nv, bpm)) = "metronome " ++ showR nv ++ " " ++ showR bpm
    where
      showR = showR' . realToFrac @_ @Rational
      showR' ((unRatio -> (x, 1))) = show x
      showR' ((unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"

instance Semigroup Tempo where
  a <> b
    | a == mempty = b
    | b == mempty = a
    | otherwise = a

instance Monoid Tempo where
  mempty = metronome (1 / 4) 120

-- | Create a tempo from a duration and a number of beats per minute.
--
--   For example @metronome (1/2) 48@ means 48 half notes per minute.
metronome :: Duration -> Bpm -> Tempo
metronome noteVal bpm = Tempo Nothing (Just noteVal) $ 60 / (bpm * noteVal)

-- | Get the note value indicated by a tempo.
tempoNoteValue :: Tempo -> Maybe NoteValue
tempoNoteValue (Tempo _n nv _d) = nv

-- | Get the number of beats per minute indicated by a tempo.
tempoBeatsPerMinute :: Tempo -> Bpm
tempoBeatsPerMinute = snd . getTempo

-- | Get the note value and number of beats per minute indicated by a tempo.
--
-- Typically used with the @ViewPatterns@ extension, as in
--
-- > foo (getTempo -> (nv, bpm)) = ...
getTempo :: Tempo -> (NoteValue, Bpm)
getTempo (Tempo _ Nothing x) = (1, (60 * recip x) / 1) -- assume whole note
getTempo (Tempo _ (Just nv) x) = (nv, (60 * recip x) / nv)

-- | Convert a tempo to a duration suitable for converting written to sounding durations.
--
-- > stretch (tempoToDuration t) notation = sounding
-- > compress (tempoToDuration t) sounding = notation
tempoToDuration :: Tempo -> Duration
tempoToDuration (Tempo _ _ x) = x

-- | Set the tempo of the given score.
tempo :: (HasMeta a, HasPosition a) => Tempo -> a -> a
tempo c x = case _era x of
  Nothing -> x
  Just e -> tempoDuring e c x

-- | Set the tempo of the given part of a score.
tempoDuring :: HasMeta a => Span -> Tempo -> a -> a
tempoDuring s c = addMetaEvent $ view event (s, c)

{-
        inSpan' (view onsetAndOffset -> (t,u)) x = t <= x && x < u
        mkNote s x = view note (s, x)

        -- | Split a reactive into notes, as well as the values before and after the first/last update
        -- TODO fails if not positive
        -- TODO consolidate
        reactiveIn :: Span -> Reactive a -> [Note a]
        reactiveIn s r
            | _duration s <= 0 = error "reactiveIn: Needs positive duration"
            | otherwise       = let r2 = trimR s (fmap optionFirst r)
            in fmap (fmap $ fromJust . getFirst) $ case updates r2 of
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

        frl []  = error "frl: No value"
        frl [x] = error "frl: Just one value"
        frl xs  = (head xs, (tail.init) xs, last xs)

-}

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

adagio = metronome (1 / 4) 64

adagio :: Tempo

allegretto = metronome (1 / 4) 118

allegretto :: Tempo

allegro = metronome (1 / 4) 128

allegro :: Tempo

andante = metronome (1 / 4) 84

andante :: Tempo

largo = metronome (1 / 4) 48

largo :: Tempo

lento = metronome (1 / 4) 42

lento :: Tempo

moderato = metronome (1 / 4) 98

moderato :: Tempo

presto = metronome (1 / 4) 140

presto :: Tempo
