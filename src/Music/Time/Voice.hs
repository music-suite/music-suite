
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
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
-------------------------------------------------------------------------------------

module Music.Time.Voice (

        -- * Voice type
        Voice,

        -- * Construction
        voice,

        -- ** Extracting values
        stretcheds,
        eventsV,

        -- ** Pattern matching
        singleStretched,

        -- *** Unsafe versions
        unsafeStretcheds,
        unsafeEventsV,


        -- * Fusion
        fuse,
        fuseBy,

        -- ** Special cases
        fuseRests,
        coverRests,

        -- * Traversing
        -- ** Separating rhythms and values
        withValues,
        withDurations,
        rotateValues,
        rotateDurations,
        reverseValues,
        reverseDurations,

        -- ** Zips
        unzipVoice,
        zipVoice,
        zipVoiceWith,
        zipVoiceWith',
        zipVoiceWithNoScale,

        -- * Context
        withContext,

        -- headV,
        -- middleV,
        -- lastV,

  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Functor.Adjunction  (unzipR)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.VectorSpace

import           Music.Time.Reverse
import           Music.Time.Split
import           Music.Time.Stretched

import           Control.Applicative
import           Control.Lens             hiding (Indexable, Level, above,
                                           below, index, inside, parts,
                                           reversed, transform, (<|), (|>))
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.Foldable            (Foldable)
import qualified Data.Foldable            as Foldable
import qualified Data.List
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Traversable         (Traversable)
import qualified Data.Traversable         as T
import           Data.Typeable
import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Internal.Util

-- |
-- A 'Voice' is a sequential composition of values. Events may not overlap.
--
-- @
-- type Voice a = [Stretched a]
-- @
--
newtype Voice a = Voice { getVoice :: VoiceList (VoiceEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

-- A voice is a list of events with explicit duration. Events can not overlap.
--
-- Voice is a 'Monoid' under sequential composition. 'mempty' is the empty part and 'mappend'
-- appends parts.
--
-- Voice is a 'Monad'. 'return' creates a part containing a single value of duration
-- one, and '>>=' transforms the values of a part, allowing the addition and
-- removal of values under relative duration. Perhaps more intuitively, 'join' scales
-- each inner part to the duration of the outer part, then removes the
-- intermediate structure.

-- Can use [] or Seq here
type VoiceList = []

-- Can use any type as long as voiceEv provides an Iso
type VoiceEv a = Stretched a

voiceEv :: Iso (Stretched a) (Stretched b) (VoiceEv a) (VoiceEv b)
voiceEv = id

instance Applicative Voice where
  pure  = return
  (<*>) = ap

instance Alternative Voice where
  (<|>) = (<>)
  empty = mempty

instance Monad Voice where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance MonadPlus Voice where
  mzero = mempty
  mplus = mappend
  
-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Voice a) where
  type Unwrapped (Voice a) = (VoiceList (VoiceEv a))
  _Wrapped' = iso getVoice Voice

instance Rewrapped (Voice a) (Voice b)

instance Cons (Voice a) (Voice b) (Stretched a) (Stretched b) where
  _Cons = prism (\(s,v) -> (view voice.return $ s) <> v) $ \v -> case view stretcheds v of
    []      -> Left  mempty
    (x:xs)  -> Right (x, view voice xs)

instance Snoc (Voice a) (Voice b) (Stretched a) (Stretched b) where
  _Snoc = prism (\(v,s) -> v <> (view voice.return $ s)) $ \v -> case unsnoc (view stretcheds v) of
    Nothing      -> Left  mempty
    Just (xs, x) -> Right (view voice xs, x)

instance Transformable (Voice a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Voice a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

-- TODO move
instance Splittable () where
  split _ x = (x,x)

instance Splittable a => Splittable (Voice a) where
  split t x
    | t <= 0           = (mempty, x)
    | t >= _duration x = (x,      mempty)
    | otherwise        = let (a,b) = split' t {-split-} (x^._Wrapped) in (a^._Unwrapped, b^._Unwrapped)
    where
      split' = error "TODO"

      -- split' :: Time -> (Duration -> a -> (a,a)) -> [Stretched a] -> ([Stretched a], [Stretched a])
      -- split' t f = over both mcatMaybes . unzipR . snd . Data.List.mapAccumL go (0::Time)
      --   where
      --     go :: Time -> Stretched a -> (Time, (Maybe (Stretched a), Maybe (Stretched a)))
      --     go currentOnset (view (from stretched) -> (d,x))
      --       = let currentOffset = currentOnset .+^ d
      --             theSplit = if t              <= currentOnset           then (Nothing, Just (view stretched (d,x))) else
      --                        if currentOnset   <  t && t < currentOffset then
      --                          let x1 = x
      --                              x2 = x in
      --                          (Just (view stretched (t .-^ currentOnset,x1)), Just (view stretched (d ^-^ (t .-^ currentOnset),x2))) else
      --                        if currentOffset  <= t                      then (Just (view stretched (d,x)), Nothing) else error "Impossible"
      --           in (currentOnset .+^ d, theSplit)

instance Reversible a => Reversible (Voice a) where
  rev = over _Wrapped' (fmap rev) -- TODO OK?


-- Lifted instances

instance IsPitch a => IsPitch (Voice a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Voice a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Voice a) where
  fromDynamics = pure . fromDynamics

-- Bogus instance, so we can use [c..g] expressions
instance Enum a => Enum (Voice a) where
  toEnum = return . toEnum
  fromEnum = list 0 (fromEnum . head) . Foldable.toList

-- Bogus instance, so we can use numeric literals
instance Num a => Num (Voice a) where
  fromInteger = return . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = error "Not implemented"
  (-)    = error "Not implemented"
  (*)    = error "Not implemented"

-- Bogus instances, so we can use c^*2 etc.
instance AdditiveGroup (Voice a) where
  zeroV   = error "Not implemented"
  (^+^)   = error "Not implemented"
  negateV = error "Not implemented"

instance VectorSpace (Voice a) where
  type Scalar (Voice a) = Duration
  d *^ s = d `stretch` s


-- |
-- Create a 'Voice' from a list of 'Stretched' values.
--
-- This is a 'Getter' (rather than a function) for consistency:
--
-- @
-- [ (0 '<->' 1, 10)^.'stretched',
--   (1 '<->' 2, 20)^.'stretched',
--   (3 '<->' 4, 30)^.'stretched' ]^.'voice'
-- @
--
-- @
-- 'view' 'voice' $ 'map' ('view' 'stretched') [(0 '<->' 1, 1)]
-- @
--
-- Se also 'stretcheds'.
--
voice :: Getter [Stretched a] (Voice a)
voice = from unsafeStretcheds
-- voice = to $ flip (set stretcheds) empty
{-# INLINE voice #-}

-- |
-- View a 'Voice' as a list of 'Stretched' values.
--
-- @
-- 'view' 'stretcheds'                        :: 'Voice' a -> ['Stretched' a]
-- 'set'  'stretcheds'                        :: ['Stretched' a] -> 'Voice' a -> 'Voice' a
-- 'over' 'stretcheds'                        :: (['Stretched' a] -> ['Stretched' b]) -> 'Voice' a -> 'Voice' b
-- @
--
-- @
-- 'preview'  ('stretcheds' . 'each')           :: 'Voice' a -> 'Maybe' ('Stretched' a)
-- 'preview'  ('stretcheds' . 'element' 1)      :: 'Voice' a -> 'Maybe' ('Stretched' a)
-- 'preview'  ('stretcheds' . 'elements' odd)   :: 'Voice' a -> 'Maybe' ('Stretched' a)
-- @
--
-- @
-- 'set'      ('stretcheds' . 'each')           :: 'Stretched' a -> 'Voice' a -> 'Voice' a
-- 'set'      ('stretcheds' . 'element' 1)      :: 'Stretched' a -> 'Voice' a -> 'Voice' a
-- 'set'      ('stretcheds' . 'elements' odd)   :: 'Stretched' a -> 'Voice' a -> 'Voice' a
-- @
--
-- @
-- 'over'     ('stretcheds' . 'each')           :: ('Stretched' a -> 'Stretched' b) -> 'Voice' a -> 'Voice' b
-- 'over'     ('stretcheds' . 'element' 1)      :: ('Stretched' a -> 'Stretched' a) -> 'Voice' a -> 'Voice' a
-- 'over'     ('stretcheds' . 'elements' odd)   :: ('Stretched' a -> 'Stretched' a) -> 'Voice' a -> 'Voice' a
-- @
--
-- @
-- 'toListOf' ('stretcheds' . 'each')                :: 'Voice' a -> ['Stretched' a]
-- 'toListOf' ('stretcheds' . 'elements' odd)        :: 'Voice' a -> ['Stretched' a]
-- 'toListOf' ('stretcheds' . 'each' . 'filtered'
--              (\\x -> '_duration' x \< 2))  :: 'Voice' a -> ['Stretched' a]
-- @
--
-- This is not an 'Iso', as the note list representation does not contain meta-data.
-- To construct a score from a note list, use 'score' or @'flip' ('set' 'stretcheds') 'empty'@.
--
stretcheds :: Lens (Voice a) (Voice b) [Stretched a] [Stretched b]
stretcheds = unsafeStretcheds
{-# INLINE stretcheds #-}

eventsV :: Lens (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
eventsV = unsafeEventsV
{-# INLINE eventsV #-}

unsafeEventsV :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
unsafeEventsV = iso (map (^.from stretched) . (^.stretcheds)) ((^.voice) . map (^.stretched))
{-# INLINE unsafeEventsV #-}

unsafeStretcheds :: Iso (Voice a) (Voice b) [Stretched a] [Stretched b]
unsafeStretcheds = _Wrapped
{-# INLINE unsafeStretcheds #-}

singleStretched :: Prism' (Voice a) (Stretched a)
singleStretched = unsafeStretcheds . single
{-# INLINE singleStretched #-}


-- |
-- Unzip the given voice. This is specialization of 'unzipR'.
--
unzipVoice :: Voice (a, b) -> (Voice a, Voice b)
unzipVoice = unzipR

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice :: Voice a -> Voice b -> Voice (a, b)
zipVoice = zipVoiceWith (,)

-- |
-- Join the given voices by multiplying durations and combining values using the given function.
--
zipVoiceWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith = zipVoiceWith' (*)

-- |
-- Join the given voices without combining durations.
--
zipVoiceWithNoScale :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWithNoScale f a b = zipVoiceWith' (\x y -> x) f a b

-- |
-- Join the given voices by combining durations and values using the given function.
--
zipVoiceWith' :: (Duration -> Duration -> Duration) -> (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith' f g
  ((unzip.view eventsV) -> (ad, as))
  ((unzip.view eventsV) -> (bd, bs))
  = let cd = zipWith f ad bd
        cs = zipWith g as bs
     in view (from unsafeEventsV) (zip cd cs)

-- |
-- Merge consecutive equal notes.
--
fuse :: Eq a => Voice a -> Voice a
fuse = fuseBy (==)

-- |
-- Merge consecutive equal notes using the given function.
--
fuseBy :: (a -> a -> Bool) -> Voice a -> Voice a
fuseBy p = fuseBy' p head

-- |
-- Merge consecutive equal notes using the given equality predicate and merge function.
--
fuseBy' :: (a -> a -> Bool) -> ([a] -> a) -> Voice a -> Voice a
fuseBy' p g = over unsafeEventsV $ fmap foldNotes . Data.List.groupBy (inspectingBy snd p)
  where
    -- Add up durations and use a custom function to combine notes
    -- Typically, the combination function us just 'head', as we know that group returns
    -- non-empty lists of equal elements.
    foldNotes (unzip -> (ds, as)) = (sum ds, g as)

-- | 
-- Fuse all rests in the given voice.
--
-- The resulting voice will have no consecutive rests.
--
fuseRests :: Voice (Maybe a) -> Voice (Maybe a)
fuseRests = fuseBy (\x y -> isNothing x && isNothing y)

-- | 
-- Remove all rests in the given voice by prolonging the previous note.
--
-- Returns 'Nothing' if and only if the given voice contains rests only.
--
coverRests :: Voice (Maybe a) -> Maybe (Voice a)
coverRests x = if hasOnlyRests then Nothing else Just (fmap fromJust $ fuseBy merge x)
  where
    norm = fuseRests x
    merge Nothing  Nothing  = error "Voice normalized, so consecutive rests are impossible"
    merge (Just x) Nothing  = True
    merge Nothing  (Just x) = True
    merge (Just x) (Just y) = False
    hasOnlyRests = all isNothing $ toListOf traverse x -- norm


withContext :: Voice a -> Voice (Maybe a, a, Maybe a)
withContext = over valuesV withPrevNext

-- TODO expose?
voiceFromRhythm :: [Duration] -> Voice ()
voiceFromRhythm = mkVoice . map (, ())

mkVoice = view voice . map (view stretched)

--
-- TODO more elegant definition of durationsV and valuesV using indexed traversal or similar?
--

durationsV :: Lens' (Voice a) [Duration]
durationsV = lens getDurs (flip setDurs)
  where
    getDurs :: Voice a -> [Duration]
    getDurs = map fst . view eventsV

    setDurs :: [Duration] -> Voice a -> Voice a
    setDurs ds as = zipVoiceWith' (\a b -> a) (\a b -> b) (mconcat $ map durToVoice ds) as

    durToVoice d = stretch d $ pure ()

valuesV :: Lens (Voice a) (Voice b) [a] [b]
valuesV = lens getValues (flip setValues)
  where
    getValues :: Voice a -> [a]
    getValues = map snd . view eventsV

    setValues :: [a] -> Voice b -> Voice a
    setValues as bs = zipVoiceWith' (\a b -> b) (\a b -> a) (listToVoice as) bs

    listToVoice = mconcat . map pure

-- |
-- Transform the durations, leaving values intact.
withDurations :: ([Duration] -> [Duration]) -> Voice a -> Voice a
withDurations = over durationsV

-- |
-- Transform the values, leaving durations intact.
withValues :: ([a] -> [b]) -> Voice a -> Voice b
withValues = over valuesV

-- |
-- Rotate durations by the given number of steps, leaving values intact.
--
rotateDurations :: Int -> Voice a -> Voice a
rotateDurations n = over durationsV (rotate n)

-- |
-- Rotate values by the given number of steps, leaving durations intact.
--
rotateValues :: Int -> Voice a -> Voice a
rotateValues n = over valuesV (rotate n)

-- |
-- Reverse durations, leaving values intact.
--
reverseDurations :: Voice a -> Voice a
reverseDurations = over durationsV reverse

-- |
-- Reverse values, leaving durations intact.
--
reverseValues :: Voice a -> Voice a
reverseValues = over valuesV reverse


--
-- TODO
-- Implement meta-data
--


