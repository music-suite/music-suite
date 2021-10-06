{-# OPTIONS_GHC -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Time.Reactive
  ( -- * Reactive type
    Reactive,

    -- * Query
    initial,
    final,
    intermediate,
    discrete,
    updates,
    occs,
    atTime,

    -- * Construction
    alwaysR,

    -- * Combine
    switchR,
    trigger,
    trimR,

    -- * Split
    splitReactive,

    -- * Sampling
    Segment,
    continous,
    continousWith,
    sample,
  )
where

import Control.Applicative
import Control.Lens hiding
  ( Indexable,
    Level,
    below,
    index,
    inside,
    parts,
    reversed,
    transform,
    (<|),
    (|>),
  )
import Control.Monad
import Control.Monad.Plus
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Semigroup hiding ()
import Data.Typeable
import Music.Dynamics.Literal
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Music.Pitch.Literal
import Music.Time.Behavior
import Music.Time.Event
import Music.Time.Juxtapose
import Music.Time.Score

-- |
-- Forms an applicative as per 'Behavior', but only switches at discrete points.
newtype Reactive a = Reactive {getReactive :: ([Time], Behavior a)}
  deriving (Functor, Semigroup, Monoid, Typeable)

instance Eq a => Eq (Reactive a) where
  a == b =
    initial a == initial b && updates a == updates b

instance Show a => Show (Reactive a) where
  showsPrec n a = showsPrec n (initial a, updates a)

instance Transformable (Reactive a) where
  transform s (Reactive (t, r)) = Reactive (transform s t, transform s r)

instance Wrapped (Reactive a) where
  type Unwrapped (Reactive a) = ([Time], Behavior a)

  _Wrapped' = iso getReactive Reactive

instance Rewrapped (Reactive a) (Reactive b)

instance Applicative Reactive where
  pure = pureDefault
    where
      pureDefault = view _Unwrapped . pure . pure

  (<*>) = apDefault
    where
      (view _Wrapped -> (tf, rf)) `apDefault` (view _Wrapped -> (tx, rx)) = view _Unwrapped (tf <> tx, rf <*> rx)

instance IsPitch a => IsPitch (Reactive a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Reactive a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Reactive a) where
  fromDynamics = pure . fromDynamics

instance Alterable a => Alterable (Reactive a) where
  sharpen = fmap sharpen

  flatten = fmap flatten

instance Augmentable a => Augmentable (Reactive a) where
  augment = fmap augment

  diminish = fmap diminish

-- | Construct a reactive that always returns the value of the Behavior.
alwaysR :: Behavior a -> Reactive a
alwaysR b = 
  Reactive ([], b)

-- |
-- Get the initial value.
initial :: Reactive a -> a
initial r = r `atTime` minB (occs r)
  where
    -- If there are no updates, just use value at time 0
    -- Otherwise pick an arbitrary time /before/ the first value
    -- It looks strange but it works
    minB [] = 0
    minB (x : _) = x - 1

-- | Get the time of all updates and the value switched to at this point.
updates :: Reactive a -> [(Time, a)]
updates r = (\t -> (t, r `atTime` t)) <$> (List.sort . List.nub) (occs r)

-- | Get the time of all updates.
occs :: Reactive a -> [Time]
occs = fst . (^. _Wrapped')

-- | Split a reactive into events, as well as the values before and after the first/last update
splitReactive :: Reactive a -> Either a ((a, Time), [Event a], (Time, a))
splitReactive r = case updates r of
  [] -> Left (initial r)
  (t, x) : [] -> Right ((initial r, t), [], (t, x))
  (t, x) : xs -> Right ((initial r, t), fmap mkEvent $ mrights (res $ (t, x) : xs), head $ mlefts (res $ (t, x) : xs))
  where
    mkEvent (t, u, x) = (t <-> u, x) ^. event
    -- Always returns a 0 or more Right followed by one left
    res :: [(Time, a)] -> [Either (Time, a) (Time, Time, a)]
    res rs =
      let (ts, xs) = unzip rs
       in flip fmap (withNext ts `zip` xs) $
            \((t, mu), x) -> case mu of
              Nothing -> Left (t, x)
              Just u -> Right (t, u, x)
    -- lenght xs == length (withNext xs)
    withNext :: [a] -> [(a, Maybe a)]
    withNext = go
      where
        go [] = []
        go [x] = [(x, Nothing)]
        go (x : y : rs) = (x, Just y) : withNext (y : rs)

-- | Sample a reactive at the given time.
--
-- @
-- x `atTime` t = discrete x ! t
-- @
atTime :: Reactive a -> Time -> a
atTime = (!) . snd . (^. _Wrapped')

-- |
-- Get the final value.
final :: Reactive a -> a
final x = case (initial x, updates x) of
  (i, []) -> i
  (_i, xs) -> snd $ last xs

-- | @switch t a b@ behaves as @a@ before time @t@, then as @b@.
switchR :: Time -> Reactive a -> Reactive a -> Reactive a
switchR t (Reactive (tx, bx)) (Reactive (ty, by)) =
  Reactive $
    (,)
      (filter (< t) tx <> [t] <> filter (> t) ty)
      (switch t bx by)

-- | @switch t a b@ behaves as @b@ during span @s@, otherwise as @a@.
latchDuringR :: Span -> Reactive a -> Reactive a -> Reactive a
latchDuringR s (Reactive (tx, bx)) (Reactive (ty, by)) =
  Reactive $
    (,)
      (tx1 <> ty1 <> tx2)
      (latchDuring' s bx by)
  where
    tx1 = filter (< (s ^.onset)) tx
    ty1 = filter (\y -> (y >= (s ^.onset)) && (y < s ^.offset)) ty
    tx2 = filter (>= (s ^.offset)) tx

-- | @switch t a b@ behaves as @a@ except at time @t@ when it behaves as @b@.
trigger :: Time -> Reactive a -> Reactive a -> Reactive a
trigger t (Reactive (tx, bx)) (Reactive (ty, by)) =
  Reactive $
    (,)
      (filter (< t) tx <> [t] <> filter (> t) ty)
      (switch' t bx by bx)


-- | Replace everything outside the given span by mempty.
trimR :: Monoid a => Span -> Reactive a -> Reactive a
trimR (view onsetAndOffset -> (t, u)) x = switchR t mempty (switchR u x mempty)

-- |
-- Get all intermediate values.
intermediate :: Transformable a => Reactive a -> [Event a]
intermediate (updates -> []) = []
intermediate (updates -> xs) = fmap (\((t1, x), (t2, _)) -> (t1 <-> t2, x) ^. event) $ withNext $ xs
  where
    withNext xs = zip xs (tail xs)

-- |
-- Realize a 'Reactive' value as a discretely changing behavior.
discrete :: Reactive a -> Behavior a
discrete = continous . fmap pure

-- | A behavior only defined on the unit span ('mempty' for 'Span').
type Segment a = Behavior a

-- | Realize a 'Reactive' value as an continous behavior.
continous :: Reactive (Segment a) -> Behavior a
continous = join . reactiveToBehavior
  where
    reactiveToBehavior (Reactive (_, b)) = b

-- | Realize a 'Reactive' value as an continous behavior.
continousWith :: Segment (a -> b) -> Reactive a -> Behavior b
continousWith f x = continous $ liftA2 (<*>) (pure f) (fmap pure x)

-- | Sample a 'Behavior' into a reactive.
sample :: [Time] -> Behavior a -> Reactive a
sample ts b = Reactive (ts, b)

