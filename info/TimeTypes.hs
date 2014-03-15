
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module TimeTypes (

        -- * Data.Normalized
        Normalized(..),
        normalize,
        
        -- * Data.Value
        -- Single(..),
        -- left3,
        -- right3,
        -- left4,
        -- right4,
        -- valueDefault,

        -- * Music.Time.Transform
        Transformable(..),
        delaying,       -- :: Duration -> Span
        undelaying,
        stretching,     -- :: Duration -> Span
        compressing,
        delay,          -- :: Transformable a => Duration -> a -> a
        -- delay',
        undelay,
        stretch,        -- :: Transformable a => Duration -> a -> a
        compress,

        -- * Music.Time.Duration
        HasDuration(..),
        stretchTo,      -- :: (Transformable a, HasDuration a) => Duration -> a -> a
        stretchNorm,

        -- * Music.Time.Position
        HasPosition(..),
        era,
        -- preOnset,       -- :: HasPosition a => a -> Time
        onset,          -- :: HasPosition a => a -> Time
        -- postOnset,      -- :: HasPosition a => a -> Time
        offset,         -- :: HasPosition a => a -> Time
        -- postOffset,     -- :: HasPosition a => a -> Time
        startAt,        -- :: (Transformable a, HasPosition a) => Time -> a -> a
        stopAt,         -- :: (Transformable a, HasPosition a) => Time -> a -> a
        alignAt,        -- :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
        retainOnset,        -- :: (HasPosition a, HasPosition b, Transformable b) => (a -> b) -> a -> b

        -- * Music.Time.Combinators
        lead,           -- :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
        follow,         -- :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
        (|>),
        (>|),
        scat,
        pcat,
        sustain,
        times,

        -- * Music.Time.Reverse
        Reversible(..),

        -- * Music.Time.Split
        Splittable(..),

        -- * Music.Time.Types
        Duration,
        Time,
        Span,
        (<->),
        (>->),
        range,
        delta,
        under,      -- :: (Transformable a, Transformable b) => Span -> (a -> b) -> a -> b
        -- conjugate,  -- :: Span -> Span -> Span

        -- * Music.Time.Stretched
        Stretched,
        stretched,
        runStretched,

        -- * Music.Time.Delayed
        Delayed,
        delayed,
        runDelayed,

        -- * Music.Time.Note
        Note,
        note,
        mapNote,
        runNote,

        -- * Music.Time.Bounds
        Bounds,
        bounds,
        trim,

        -- * Music.Time.Segment
        Segment,

        -- * Music.Time.Behavior
        Behavior,

        -- * Music.Time.Voice
        Voice,
        voice,

        -- * Music.Time.VoiceMap
        VoiceList,
        voiceList,
        VoiceMap,
        voiceMap,
        concatSubVoices,

        -- * Music.Time.Reactive
        Reactive,

        -- * Music.Time.Score
        Score,



  ) where
import           Control.Applicative
import           Control.Arrow          (first, second, (***))
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens           hiding (transform, under, (|>))
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe
import           Data.AffineSpace.Point
import           Data.Foldable          (Foldable)
-- import           Data.Key (or use Control.Lens.Indexed?)
import           Data.Semigroup
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable
import           Data.VectorSpace
import           Data.Sequence(Seq)
import qualified Data.Sequence as Seq

import           Data.Int
import           Test.SmallCheck.Series (Serial (..), cons0, newtypeCons,
                                         series, (\/))
import           Test.Tasty
import           Test.Tasty.SmallCheck

import qualified Data.Ratio             as Util_Ratio


-- Misc instances

instance Monoid b => Monad ((,) b) where
  return = pure
  (>>=) = flip (=<<)
    where
      (=<<) = (join .) . fmap
      join (b, (b', a)) = (b `mappend` b', a)

-- These are already in 'lens'
-- deriving instance Foldable ((,) o)
-- deriving instance Traversable ((,) o)


{-
    TODO

    - Use graphing and verify that timed fmap (i.e. reactive's apply) works.

    - New representations for Score, Voice and Reactive


-}




-- Types etc

{-
    LAWS Semigroup
        a <> (b <> c)    = (a <> b) <> c
        a      <> mempty = a
        mempty <> a      = a

    LAWS AdditiveGroup
        a ^+^ (b ^+^ c)    = (a ^+^ b) ^+^ c
        a      ^+^ zeroV   = a
        zeroV  ^+^ a       = a
        a      ^+^ negateV a = zeroV
        negateV a ^+^ a      = zeroV
        a ^+^ b              = b ^+^ a

    LAWS Functor
    LAWS Eq
    LAWS Ord
    LAWS
-}

-- | A value in the unit interval /(0,1)/.
newtype Normalized a = Normalized { getNormalized :: a }
    deriving (Eq, Ord, Show, Functor)

zipNormalizedWith :: (Num a, Ord a, Num b, Ord b, Num c, Ord c) => (a -> b -> c) -> Normalized a -> Normalized b -> Maybe (Normalized c)
zipNormalizedWith f a b = ((a^.unnormalize) `f` (b^.unnormalize))^? normalize

normalize' = Normalized
addLim = zipNormalizedWith (+)    
    
normalize :: (Num a, Ord a) => Prism' a (Normalized a)
normalize = prism getNormalized (\x -> if 0 <= x && x <= 1 then Right (Normalized x) else Left x)

unnormalize :: (Num a, Ord a) => Getter (Normalized a) a
unnormalize = re normalize

-- |
-- Duration, corresponding to note values in standard notation.
--
-- The standard names can be used: @1\/2@ for half note @1\/4@ for a quarter note and so on.
--
-- Duration is a one-dimensional 'VectorSpace', and is the associated vector space of time points.
-- It is a also an 'AdditiveGroup' (and hence also 'Monoid' and 'Semigroup') under addition.
--
newtype Duration = Duration { getDuration :: Rational }
instance Show Duration where
    show = showRatio . getDuration
deriving instance Typeable Duration
deriving instance Eq Duration
deriving instance Ord Duration
deriving instance Num Duration
deriving instance Enum Duration
deriving instance Fractional Duration
deriving instance Real Duration
deriving instance RealFrac Duration
deriving instance AdditiveGroup Duration
instance VectorSpace Duration where
    type Scalar Duration = Duration
    (*^) = (*)
instance Floating Duration
instance InnerSpace Duration
instance Semigroup Duration where
    (<>) = (*^)
instance Monoid Duration where
    mempty  = 1 -- TODO use some notion of norm
    mappend = (*^)
instance Transformable Duration where
    Span (_, d1) `transform` d2 = d1 * d2
instance HasDuration Duration where
    duration = id

-- |
-- Time points, representing duration since some known reference time, typically the start of the music.
--
-- 'Time' forms an 'AffineSpace' with 'Duration' as difference space.
--
newtype Time = Time { getTime :: Rational }
instance Show Time where
    show = showRatio . getTime
deriving instance Typeable Time
deriving instance Eq Time
deriving instance Ord Time
deriving instance Num Time
deriving instance Enum Time
deriving instance Fractional Time
deriving instance Real Time
deriving instance RealFrac Time
deriving instance AdditiveGroup Time
instance VectorSpace Time where
    type Scalar Time = Duration
    Duration x *^ Time y = Time (x * y)
instance AffineSpace Time where
    type Diff Time = Duration
    Time x .-. Time y     = Duration (x - y)
    Time x .+^ Duration y = Time     (x + y)
instance Semigroup Time where
    (<>) = (^+^)
instance Monoid Time where
    mempty  = zeroV
    mappend = (^+^)
    mconcat = sumV
instance Transformable Time where
    Span (t1, d1) `transform` t2 = t1 ^+^ d1 *^ t2
instance HasPosition Time where
    position = const


-- Inverse semigroup:
--
--  negateV x <> x = zeroV
--  OR
--  x         = x         <> negateV x <> x
--  negateV x = negateV x <> x         <> negateV x
--
newtype Span = Span { getDelta :: (Time, Duration) }
    deriving (Eq, Ord, Show, Typeable)

(<->) :: Time -> Time -> Span
(>->) :: Time -> Duration -> Span
t <-> u = t >-> (u .-. t)
t >-> d = Span (t, d)

-- |
-- > 1 <-> 2 == (1,2)^.from range
--
range :: Iso' Span (Time, Time)
range = iso getRange $ uncurry (<->) where getRange x = let (t, d) = getDelta x in (t, t .+^ d)

-- |
-- > 1 >-> 2 == (1,2)^.from delta
--
delta :: Iso' Span (Time, Duration)
delta = iso getDelta $ uncurry (>->)

instance HasPosition Span where
    position (view range -> (t1, t2)) = alerp t1 t2
instance HasDuration Span where
    duration = snd . view delta
instance Transformable Span where
    transform = (<>)
instance Splittable Span where
    -- XXX
instance Semigroup Span where
    (<>) = (^+^)
instance Monoid Span where
    mempty  = zeroV
    mappend = (^+^)
instance AdditiveGroup Span where
    zeroV   = 0 <-> 1
    Span (t1, d1) ^+^ Span (t2, d2) = Span (t1 ^+^ d1 *^ t2, d1*d2)
    negateV (Span (t, d)) = Span (-t ^/ d, recip d)

-- > forall s . id `sunder` s = id
under :: (Transformable a, Transformable b) => Span -> (a -> b) -> a -> b
s `under` f = transform (negateV s) . f . transform s

conjugate :: Span -> Span -> Span
conjugate t1 t2  = negateV t1 <> t2 <> t1


{-
    (\x -> (transl x, matrixRep x)) ((inv $ translation 2 <> scaling 2) :: Transformation Double)
-}

{-
    -- | Invert a transformation.
    inv :: HasLinearMap v => Transformation v -> Transformation v
    inv (Transformation t t' v) = Transformation (linv t) (linv t')
                                                 (negateV (lapp (linv t) v))

    -- | Get the transpose of a transformation (ignoring the translation
    --   component).
    transp :: Transformation v -> (v :-: v)
    transp (Transformation _ t' _) = t'

    -- | Get the translational component of a transformation.
    transl :: Transformation v -> v
    transl (Transformation _ _ v) = v

    -- | Transformations are closed under composition; @t1 <> t2@ is the
    --   transformation which performs first @t2@, then @t1@.
    instance HasLinearMap v => Semigroup (Transformation v) where
      Transformation t1 t1' v1 <> Transformation t2 t2' v2
        = Transformation (t1 <> t2) (t2' <> t1') (v1 ^+^ lapp t1 v2)
-}













-- newtype Time -- Semigroup, Monoid (sum)
-- newtype Span -- Semigroup, Monoid, AdditiveGroup (composition)
--
-- spans :: Iso (Time^2) (Time, Dur)
-- spans = iso (\(t,d) -> (t,t.+^d)) (\(t,u) -> (t,u.-.t))
--
--
--
--

-- |
-- A 'Note' is a value with a known 'position' and 'duration'.
-- Notes are isomorphic pairs of spans and values, as whitnessed by 'note'.
--
-- Another way is to view a note is that it is a suspended application of a time
-- transformation, with 'runNote' extracting the transformed value:
--
-- There is a morphism from 'runNote' to 'transform':
--
-- > runNote . transform s = transform s . runNote
--
-- Note is a 'Monad' and 'Applicative' in the style of pair, with 'return' placing a value
-- at the default span 'mempty' and 'join' composing time transformations.
--
newtype Note a      = Note      { getNote :: (Span, a)     } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Applicative, Monad, Comonad, Foldable, Traversable)

-- |
-- A 'Delayed' value has a known 'position', but no duration.
--
newtype Delayed a   = Delayed   { getDelayed :: (Time, a)     } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Applicative, Monad, Comonad, Foldable, Traversable)

-- |
-- A 'Stretched' value has a known 'position', but no duration.
--
newtype Stretched a = Stretched { getStretched :: (Duration, a) } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Applicative, Monad, Comonad, Foldable, Traversable)

-- instance Single Note where
--     value = valueDefault
-- instance Single Delayed where
--     value = valueDefault
-- instance Single Stretched where
--     value = valueDefault

instance Reversible (Note a) where
    rev = stretch (-1)
instance Splittable a => Splittable (Note a) where

instance Reversible (Delayed a) where

instance Reversible (Stretched a) where
    rev = stretch (-1)
instance Splittable a => Splittable (Stretched a) where

-- XXX Compare with Located in diagrams

instance Wrapped (Note a) where { type Unwrapped (Note a) = (Span, a) ; _Wrapped' = iso getNote Note }
instance Wrapped (Delayed a) where { type Unwrapped (Delayed a) = (Time, a) ; _Wrapped' = iso getDelayed Delayed }
instance Wrapped (Stretched a) where { type Unwrapped (Stretched a) = (Duration, a) ; _Wrapped' = iso getStretched Stretched }
instance Rewrapped (Note a) (Note b)
instance Rewrapped (Delayed a) (Delayed b)
instance Rewrapped (Stretched a) (Stretched b)

instance Transformable (Note a) where transform t = unwrapped $ first (transform t)
instance Transformable (Delayed a) where transform t = unwrapped $ first (transform t)
instance Transformable (Stretched a) where transform t = unwrapped $ first (transform t)

instance HasDuration (Note a) where duration = duration . ask . unwr
instance HasDuration (Stretched a) where duration = duration . ask . unwr

instance HasPosition (Note a) where x `position` p = ask (unwr x) `position` p
instance HasPosition (Delayed a) where x `position` p = ask (unwr x)`position` p

note :: Iso' (Note a) (Span, a)
note = _Wrapped'

-- noteValue :: Lens' (Note a) a
-- noteValue = value

noteEra :: Lens' (Note a) Span
noteEra = lens era undefined

mkNote :: Time -> Duration -> a -> Note a
mkNote t d v = t >-> d `transform` return v

delayed :: Iso' (Delayed a) (Time, a)
delayed = _Wrapped'
stretched :: Iso' (Stretched a) (Duration, a)
stretched = _Wrapped'

mapNote :: (Transformable a, Transformable b) => (a -> b) -> Note a -> Note b
mapNote f (Note (s,x)) = Note (s, under s f x)
-- TODO use unwr

runNote :: Transformable a => Note a -> a
runNote = uncurry transform . unwr

runDelayed :: Transformable a => Delayed a -> a
runDelayed = uncurry delay' . unwr

runStretched :: Transformable a => Stretched a -> a
runStretched = uncurry stretch . unwr

-- instance HasPosition (Note a) where position n







-- |
-- 'Bounds' restricts the start and stop time of a value.
--
newtype Bounds a      = Bounds      { getBounds :: (Span, a)     } 
    deriving (Functor, Foldable, Traversable)

bounds :: Time -> a -> Time -> Bounds a
bounds t x u = Bounds (t <-> u, x)

-- | XXX 
trim :: Monoid a => Bounds a -> Bounds a
trim = undefined

instance Reversible (Bounds a) where
    rev = stretch (-1)
instance Splittable a => Splittable (Bounds a) where
instance Wrapped (Bounds a) where { type Unwrapped (Bounds a) = (Span, a) ; _Wrapped' = iso getBounds Bounds }
instance Rewrapped (Bounds a) (Bounds b)
instance Transformable (Bounds a) where transform t = unwrapped $ first (transform t)
instance HasDuration (Bounds a) where duration = duration . ask . unwr
instance HasPosition (Bounds a) where x `position` p = ask (unwr x) `position` p

-- TODO Compare Diagram's Trail and Located (and see the conal blog post)

-- |
--
-- A 'Segment' is a function of 'Duration'. Intuitively, it is a value varying over some unknown time span.
-- To place a segment in a particular time span, we use 'Note' 'Segment'.
--
-- Segment is a 'Monad' and 'Applicative' functor, similar to the function instance:
--
-- > pure s ! t == s
--
-- > fs <*> xs ! t == (fs ! t) (xs ! t)
--
-- > join s ! t == (s ! t) ! t
--
newtype Segment a = Segment (Duration -> a)    deriving (Functor, Applicative, Monad{-, Comonad-})
-- Defined 0-1
instance Semigroup a => Semigroup (Segment a) where
    (<>) = undefined
instance Monoid a => Monoid (Segment a) where

-- |
--
-- A 'Behavior' is a function of 'Time'. Intuitively, it is a value varying over the set of all time points.
-- While a 'Behavior' can not be placed (as it has no endpoints), it can be "focused", by placing it inside
-- 'Bounds'.
--
-- Behavior is a 'Monad' and 'Applicative' functor, similar to the function instance:
--
-- > pure s ! t == s
--
-- > fs <*> xs ! t == (fs ! t) (xs ! t)
--
-- > join s ! t == (s ! t) ! t
--
newtype Behavior a  = Behavior (Time -> a)     deriving (Functor, Applicative, Monad, Comonad)
-- Defined throughout, "focused" on 0-1
instance Semigroup a => Semigroup (Behavior a) where
    (<>) = undefined
instance Monoid a => Monoid (Behavior a) where




newtype Score a     = Score      { getScore :: Seq (Note a)     } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Monoid)
instance Wrapped (Score a) where { type Unwrapped (Score a) = Seq (Note a) ; _Wrapped' = iso getScore Score }
instance Rewrapped (Score a) (Score b)
instance Applicative Score where
    pure  = return
    (<*>) = ap
instance Monad Score where
    return = (^. _Unwrapped') . return . return
    xs >>= f = (^. _Unwrapped') $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)
instance Alternative Score where
    empty = mempty
    (<|>) = mappend
instance MonadPlus Score where
    mzero = mempty
    mplus = mappend
instance Transformable (Score a) where
    transform t (Score xs) = Score (fmap (transform t) xs)
instance Reversible a => Reversible (Score a) where
    rev (Score xs) = Score (fmap rev xs)
instance HasPosition (Score a) where
instance HasDuration (Score a) where
instance Splittable a => Splittable (Score a) where

-- | XXX indexed traversal?
score :: Traversal (Voice a) (Voice b) (Note a) (Note b)
score = undefined


-- |
-- A 'Voice' is a sequence of stretched values.
--
newtype Voice a     = Voice      { getVoice :: Seq (Stretched a)     } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Monoid)
instance Applicative Voice where
    pure  = return
    (<*>) = ap
instance Monad Voice where
    -- TODO
instance Transformable (Voice a) where
instance Reversible a => Reversible (Voice a) where
instance HasDuration (Voice a) where
instance Splittable a => Splittable (Voice a) where

-- | XXX indexed traversal?
voice :: Traversal (Voice a) (Voice b) (Stretched a) (Stretched b)
voice = undefined

-- |
-- The 'VoiceList' and 'VoiceMap' types represent a sequence of voices and sub-voices with possibly infinite division.
--
newtype VoiceList a = VoiceList (NonEmpty (Voice a)) deriving (Functor, Foldable, Traversable)
instance Applicative VoiceList where
    pure  = return
    (<*>) = ap
instance Monad VoiceList where
    -- TODO
instance Transformable (VoiceList a) where
instance Reversible a => Reversible (VoiceList a) where
instance HasDuration (VoiceList a) where
instance Splittable a => Splittable (VoiceList a) where

voiceList :: Iso' (VoiceList a) (NonEmpty (VoiceMap a))
voiceList = undefined

newtype VoiceMap a     = VoiceMap      { getVoiceMap :: Seq (Stretched a)     } deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Monoid)
instance Applicative VoiceMap where
    pure  = return
    (<*>) = ap
instance Monad VoiceMap where
    -- TODO
instance Transformable (VoiceMap a) where
instance Reversible a => Reversible (VoiceMap a) where
instance HasDuration (VoiceMap a) where
instance Splittable a => Splittable (VoiceMap a) where

-- | XXX
voiceMap :: Traversal (VoiceMap a) (VoiceMap b) (Stretched (Either a (VoiceList a))) (Stretched (Either a (VoiceList a)))
voiceMap = undefined

concatSubVoices :: Monoid a => VoiceMap a -> Voice a
concatSubVoices = undefined

-- | XXX only defined positively
-- Need to use alternative to voice similar to a zipper etc
type Reactive a = Voice (Segment a)

-- newinstance Functor Behavior
-- -- Distributive?
-- -- XXX potentially slow, improve by memoization/const optimization
-- instance Representable Behavior where
--     type Rep = Time
-- instance Applicative Behavior
-- instance Monad Behavior
-- instance Monoid a => Monoid (Behavior a)
--
-- newtype Track a = [(Time, a)]
--     -- XXX Start time, laziness
--     -- Functor, Monad
-- newtype Score a = [(Span, a)]
-- -- XXX Start time, laziness
--     -- Functor, Monad
--
--
-- newtype Reactive a = (Time -> (a, Duration^2))
-- -- XXX Start time, laziness
-- -- Distributive?
-- instance Representable Reactive where
--     type Rep = Time
-- instance Applicative Reactive
-- instance Monad Reactive
-- instance Monoid a => Monoid (Reactive a)

{-

-- Fre monad of ?
{-
data Score s a
    = SOne a
    | SPlus s [Score a]
-}
newtype Trans s a = Trans (s, [a]) deriving (Functor)
instance Monoid s => Monad (Trans s) where
    return = Trans . return . return
    -- TODO the usual >>=

type Score s a = Free (Trans s) a

viewScore :: Monoid s => Score s a -> [(s, a)]
viewScore x = case retract x of
    Trans (s,as) -> zip (repeat s) as


-- Free monad of (a,a)
{-
data Tree a
    = One a
    | Plus (Tree a) (Tree a)
-}
data Pair a = Pair a a deriving (Functor)
newtype MaybePair a = MaybePair (Maybe (Pair a)) deriving (Functor) -- Use compose
type Tree a = Free MaybePair a

-- CPS-version of Tree
newtype Search a = Search { getSearch :: forall r . (a -> Tree r) -> Tree r }
   -}







-- |
-- Functors with a single readable and writable value.
--
-- XXX Always isomorphic to pairs?
--
-- For any @'Comonad' f@ , you can simply define:
--
-- > instance Single f where
-- >     value = valueDefault
--
class Functor f => Single f where
    value' :: Lens' (f a) a
    value  :: Lens  (f a) (f a) a a
    value = value'

instance Single ((,) a) where
    value = valueDefault

left3 :: Iso ((a1, b1), c1) ((a2, b2), c2) (a1, b1, c1) (a2, b2, c2)
left3 = iso f g where f ((x,y), z) = (x, y, z); g (x, y,  z) = ((x, y), z)

right3 :: Iso (a1, (b1, c1)) (a2, (b2, c2)) (a1, b1, c1) (a2, b2, c2)
right3 = iso f g where f (x, (y,z)) = (x, y, z); g (x, y,  z) = (x, (y,z))

-- TODO run 3 version over this
left4 = iso f g where f ((x,y,z), a) = (x, y, z, a); g (x, y, z, a) = ((x, y, z), z)
right4 = iso f g where f (x, (y,z,a)) = (x, y, z, a); g (x, y, z, a) = (x, (y, z, a))


-- Why is this not in base?
instance Functor ((,,) a b) where
    fmap f = from left3 %~ fmap f

instance Single ((,,) a b) where
    value = from left3 . valueDefault

-- | XXX suspect setter (uses const)
instance Monoid a => Single ((->) a) where
    value = valueDefault


-- Iso (g a) (g b) (c, a) (c, b)

valueDefault :: Comonad g => Lens (g a) (g b) a b
valueDefault = lens extract (\s b -> fmap (const b) s)


-- Moving and scaling things

-- FIXME compare with diagrams variant
-- translation vs linear etc

-- |
-- LAW
--
-- > transform s . transform t = transform (s <> t)
--
-- LAW
--
-- > onset (delay n a) = n + onset a
-- > offset (delay n a) = n + offset a
-- > duration (stretch n a) = n * (duration a)
--
-- LEMMA
--
-- > duration a = duration (delay n a)
class Transformable a where
    transform :: Span -> a -> a


-- FIXME strange
-- transformInv (view delta -> (t,d)) = stretch (recip d) . delay' (reflectThrough 0 t)

-- FIXME get rid of this
delay' :: Transformable a => Time -> a -> a
delay' t     = delay (t .-. 0)


delaying x   = (0 .+^ x) >-> 1
stretching x = 0         >-> x
undelaying x = delaying (negate x)
compressing x = stretching (recip x)
delay    = transform . delaying
undelay  = transform . undelaying
stretch  = transform . stretching
compress = transform . compressing

-- Fitting things

-- Things with a duration

-- |
-- LAW Duration
--
-- > duration x = (offset x .-. onset x)
--
class HasDuration a where
    duration :: a -> Duration

stretchTo :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchTo d x = (d ^/ duration x) `stretch` x

stretchNorm :: (Transformable a, HasDuration a, InnerSpace Duration) => a -> a
stretchNorm x = stretchTo (normalized $ duration x) x


-- Placing things

class HasPosition a where
    position :: a -> {-Scalar-} Duration -> Time

-- |  XXX make into lens for any positionable thing
era :: HasPosition a => a -> Span

onset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
offset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
era x = onset x <-> offset x
preOnset    = (`position` (-0.5))
onset       = (`position` 0)
postOnset   = (`position` 0.5)
offset      = (`position` 1.0)
postOffset  = (`position` 1.5)

startAt :: (Transformable a, HasPosition a) => Time -> a -> a
stopAt  :: (Transformable a, HasPosition a) => Time -> a -> a
startAt t x   = (t .-. onset x) `delay` x
stopAt t  x   = (t .-. offset x) `delay` x

-- alignAt p t places the given thing so that its position p is at time t
-- > alignAt 0 == startAt
-- > alignAt 1 == stopAt
alignAt :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
alignAt p t x = (t .-. x `position` p) `delay` x


-- a `lead`   b  moves a so that (offset a' == onset b)
-- a `follow` b  moves b so that (offset a  == onset b')
lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `lead` b   = alignAt 1 (b `position` 0) a
a `follow` b = alignAt 0 (a `position` 1) b


-- Mnemonics:
--   > is at the side of the element being moved
--   >| >| etc indicates offset/onset

-- | XXX More generic version of sequential catenation (which can only be used for voices etc)
(|>) :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
(>|) :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a |> b =  a <> (a `follow` b)
a >| b =  (a `lead` b) <> b

retainOnset :: (HasPosition a, HasPosition b, Transformable b) => (a -> b) -> a -> b
retainOnset f x = startAt (onset x) (f x)

scat = Prelude.foldr (|>) mempty
pcat = mconcat

x `sustain` y     = x <> duration x `stretchTo` y
times n     = scat . replicate n

-- | Splitting and reversing things
--
--
-- Works for both positioned and unpositioned things
-- For positioned types, splits relative onset
-- XXX would some instances look nicer if duration was absolute (compare mapping) and is this a bad sign?
-- XXX what about Behavior (infinite span)
--
-- LAW
--
-- > let (a, b) = split x in duration a + duration b = duration x
--
class HasDuration a => Splittable a where
    split  :: Duration -> a -> (a, a)

before d = fst . split d
after d = snd . split d



class Reversible a where
    rev :: a -> a
instance Reversible () where
    rev = id
instance Reversible Int where
    rev = id
instance Reversible Double where
    rev = id
instance Reversible Integer where
    rev = id
instance Reversible a => Reversible [a] where
    rev = reverse . fmap rev
instance Reversible Duration where
    rev = stretch (-1)
instance Reversible Time where
    rev = stretch (-1)
instance Reversible Span where
    rev = stretch (-1)





-- -- Monoid/Semigroup
--
--
-- -- Has... Pitch Dynamics Articulation Part Chord?? Clef Slide Tremolo Text Harmonic Meta
-- -- Has+Is ... Midi/MusicXml
-- -- Is ... Pitch Interval Dynamic
--
--
--
-- reverse
-- split
--     take
--     drop
-- duration
-- position
--     onset
--     offset
-- transform
--     delay
--     stretch
-- scat
-- pcat
--
-- -- a `lead`   b  moves a so that (offset a' == onset b)
-- -- a `follow` b  moves b so that (offset a  == onset b')
-- lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
-- follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
--






















-- Utility

showRatio :: (Integral a, Show a) => Util_Ratio.Ratio a -> String
showRatio (realToFrac -> (unRatio -> (x, 1))) = show x
showRatio (realToFrac -> (unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"

unRatio :: Integral a => Util_Ratio.Ratio a -> (a, a)
unRatio x = (Util_Ratio.numerator x, Util_Ratio.denominator x)

mjoin :: (Monad m, Monad n, Functor m, Traversable n) => m (n (m (n a))) -> m (n a)
mjoin = fmap join . join . fmap T.sequence

mbind :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mbind = (join .) . fmap . (fmap join .) . T.mapM

-- Same as @flip const@, useful to fix the type of the first argument.
assuming :: a -> b -> b
assuming = flip const

sameType :: a -> a -> ()
sameType = undefined

-- TODO must be a better way to do this
unwrapped f = wr . f . unwr
wr   = (^. _Unwrapped')
unwr = (^. _Wrapped')




-- Tests

-- sc_semigroup :: (Semigroup a, Typeable a, Eq a, Serial IO a) => a -> TestTree
-- sc_semigroup x = testGroup ("Semigroup " ++ show (typeOf x)) [
    -- testProperty "mempty <> a == a" $ \a -> mempty <> a == (a :: a)
    -- ]

newtype BadMonoid a = BadMonoid [a]
    deriving (Eq, Ord, Show, Typeable)
instance Monoid (BadMonoid a) where
    BadMonoid x `mappend` BadMonoid y = BadMonoid (y `mappend` reverse x) -- lawless
    mempty = BadMonoid []
instance Functor BadMonoid where
    fmap f (BadMonoid xs) = BadMonoid (fmap f $ reverse $ xs) -- lawless

data BadFunctor a = BF1 | BF2
    deriving (Eq, Ord, Show, Typeable)

instance Functor BadFunctor where
    fmap f BF1 = BF2 -- lawless
    fmap f BF2 = BF2

instance Serial IO Span where
    series = newtypeCons Span
instance Serial IO a => Serial IO (BadFunctor a) where
    series = cons0 BF1 \/ cons0 BF2
instance Serial IO a => Serial IO (BadMonoid a) where
    series = newtypeCons BadMonoid
instance Serial IO Int8 where
    series = msum $ fmap return [0..2]
instance Serial IO Time where
    series = msum $ fmap return [1..2]
instance Serial IO Duration where
    series = msum $ fmap return [0..2]

monoid :: (Monoid t, Eq t, Show t, Typeable t, Serial IO t) => t -> TestTree
monoid typ = testGroup ("instance Monoid " ++ show (typeOf typ)) $ [
    testProperty "x <> (y <> z) == (x <> y) <> z" $ \x y z -> assuming (sameType typ x)
                  x <> (y <> z) == (x <> y) <> z,

    testProperty "mempty <> x == x"               $ \x     -> assuming (sameType typ x)
                  mempty <> x == x,

    testProperty "x <> mempty == x"               $ \x     -> assuming (sameType typ x)
                 (x <> mempty == x)
    ]
    where
        (<>) = mappend

-- functor :: (Functor f, Eq (f b), Show (f b), Typeable b, Typeable1 f, Serial IO (f b)) => f b -> TestTree
-- functor typ = testGroup ("instance Functor " ++ show (typeOf typ)) $ [
--     testProperty "fmap id = id" $ \x -> assuming (sameType typ x)
--                  (fmap id x == id x)
--     ]

-- applicative :: (Applicative f, Eq (f b), Show (f b), Typeable b, Typeable1 f, Serial IO (f b)) => f b -> TestTree
-- applicative typ = testGroup ("instance Applicative " ++ show (typeOf typ)) $ [
--
--     testProperty "pure id <*> v = v" $ \x -> assuming (sameType typ x) ((pure id <*> x) == x),
--
--     testProperty "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)"
--         $ \u v w -> assuming (sameType typ w)
--             ((pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w)))
--
--     ]


ap2 u v w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

main = defaultMain $ testGroup "" $ [
    testProperty "rev . rev == id" $ \(x :: ())    -> rev (rev x) == x,
    testProperty "rev . rev == id" $ \(x :: [Int]) -> rev (rev x) == x,

    monoid (undefined :: ()),
    monoid (undefined :: Maybe ()),
    monoid (undefined :: [()]),
    monoid (undefined :: BadMonoid Int8),

    monoid (undefined :: Time),
    monoid (undefined :: Duration),
    monoid (undefined :: Span)

    -- functor (undefined :: BadFunctor Int8),
    -- functor (undefined :: BadMonoid Int8)

    ]





