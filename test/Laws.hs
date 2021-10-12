

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}


import Prelude hiding ((**))

import Data.Void (Void)
import Data.Monoid.Average
import Data.Ord (comparing)
import Music.Prelude hiding (defaultMain, elements)
import Music.Pitch.Common.Internal (Interval(..), ChromaticSteps(..), DiatonicSteps(..))
import Music.Time.Internal.Placed (Placed, placed)
import Music.Time.Internal.Track (Track, track)

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen
import Test.Tasty.QuickCheck.Laws( testEqLaws, testMonadLaws, testApplicativeLaws )

import Data.Typeable
import Data.Maybe
import Data.Semigroup
import Control.Monad
import Control.Applicative
import Control.Comonad
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List

-- ALTERNATE SPEC

-- From http://austinrochford.com/posts/2014-05-27-quickcheck-laws.html

monoidAssocProp :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssocProp x y z = (x `mappend` (y `mappend` z)) == ((x `mappend` y) `mappend` z)

monoidRightIdProp :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdProp x = x == (x <> mempty)

monoidLeftIdProp :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdProp x = (mempty <> x) == x

functorIdProp :: (Functor f, Eq (f a)) => f a -> Bool
functorIdProp x = (fmap id x) == x

functorCompProp :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompProp x (apply -> f) (apply -> g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

monadRightIdProp :: (Monad m, Eq (m a)) => m a -> Bool
monadRightIdProp x = (x >>= return) == x

monadLeftIdProp :: (Monad m, Eq (m b)) => a -> Fun a (m b) -> Bool
monadLeftIdProp x (apply -> f) = (return x >>= f) == (f x)

monadAssocProp :: (Monad m, Eq (m c)) => m a -> Fun a (m b) -> Fun b (m c) -> Bool
monadAssocProp x (apply -> f) (apply -> g) = ((x >>= f) >>= g) == (x >>= (\x' -> f x' >>= g))



type Checkable  a     = (Eq a, Show a, Arbitrary a)

_Semigroup :: (Checkable a, Semigroup a) => a -> Property
_Semigroup t = property assoc
  where
    assoc a b c = (a <> b) <> c === a <> (b <> c .: t)

_Monoid :: (Checkable a, Semigroup a, Monoid a) => a -> Property
_Monoid t = idL .&&. idR .&&. assoc
  where
    idL   m     = m <> mempty   === m .: t
    idR   m     = mempty <> m   === m .: t
    assoc a b c = (a <> b) <> c === a <> (b <> c .: t)

{-
prop_functor :: (Functor f, Eq (f ()), Arbitrary (f ()), Show (f ()), Eq (f c), Arbitrary (f a), Show (f a)) => f () -> (b -> c) -> (a -> b) -> Property
prop_functor typ f g = fc .&&. fi
  where
    fi x = x == (sameType typ x)
    fc x = (fmap f . fmap g) x == (fmap (f . g) $ sameType1 typ x)

-- prop_applicative typ

unit :: Applicative f => f ()
unit = pure ()

(**) :: Applicative f => f a -> f b -> f (a, b)
a ** b = liftA2 (,) a b


-- prop_app typ = appLId .&&. appRId .&&. appAssoc
--   where
--     appLId v       = property $ unit ** v == fmap ((),) (sameType typ v)
--     appRId u       = property $ u ** unit == fmap (,()) (sameType typ u)
--     appAssoc u v w = property $ u ** (v ** w) == (fmap unass $ (u ** v) ** sameType typ w)

appLId :: (Eq (f ((), b)), Applicative f) => f b -> Property
appLId v = property $ unit ** v == fmap ((),) v

appRId :: (Eq (f (a, ())), Applicative f) => f a -> Property
appRId u = property $ u ** unit == fmap (,()) u

appAssoc :: (Eq (f (a, (b, c))), Applicative f) => f a -> f b -> f c -> Property
appAssoc u v w = property $ u ** (v ** w) == (fmap unass $ (u ** v) ** w)

unass :: ((a, b), c) -> (a, (b, c))
unass = \((a, b), c) -> (a, (b, c))

-}

{-
  transform mempty = id
  transform (s <> t) = transform s . transform t
  transform (s <> negateV s) = id
-}
_Transformable :: (Checkable a, Transformable a) => a -> Property
_Transformable t = te .&&. tc .&&. tn
  where
    te x     = True                          ==> transform mempty x === x .: t
    tc s t x = True ==> transform (s <> t) x === transform s (transform t $ x .: t)
    tn s x   = _duration s /= 0              ==> transform (s <> negateV s) x === x .: t

{-
  Duration vs. onset and offset
    _duration a = (offset a .-. onset a)
  or equivalently
    _duration . _era = _duration

  Transform vs. onset and offset
    for all n and a
      _onset (delay n a)       = n ^+. _onset a
    for all n and a
      _offset (delay n a)      = n ^+. _offset a
    for all n and a
      _duration (stretch n a)  = n ^*  _duration a

  More generally
    for all s, a and p
      (s `transform` a) `_position` p = s `transform` (a `_position` p)
  or equivalently
    for all s and a
      _era (s `transform` a) = s `transform` (_era a)
  or equivalently
    for all s and a
      _era . transform s = transform s . _era
-}
_HasDuration :: (Checkable a, Transformable a, HasDuration a) => a -> Property
_HasDuration t = property cd
  where
    -- cd n a  = n /= 0 ==> _duration (stretch (n) (a .: t)) === (n) * _duration a
    cd n a  = n >= 0 ==> _duration (stretch (n) (a .: t)) === (n) * _duration a

-- TODO FIXME replace use of (.:)/asTypeOf with TypeApplications!

-- TODO tests HasPosition/HasDuration/Transformable instances
-- We should also test types that are not instances of all these classes at once
_HasDurationHasPosition :: (Checkable a, Transformable a, HasDuration a, HasPosition a) => a -> Property
_HasDurationHasPosition t = property sd -- .&&. ass
  where
    sd x =
      True ==>
        fmap _duration (_era $ x .: t) === Just (_duration x)

_HasPositionTransformable:: (Checkable a, Transformable a, HasPosition a) => a -> Property
_HasPositionTransformable t = property ass -- .&&. ass
  where
    ass s x =
      isForwardSpan s  ==>
        _era (transform s x) === fmap (transform s) (_era (x .: t))

_onset, _offset :: (HasPosition1 a, Transformable a) => a -> Time
_onset = view onset
_offset = view offset

{-
  _duration (beginning t x) + _duration (ending t x) = _duration x

  _duration (beginning t x) = t `min` _duration x
    iff t >= 0
-}
_Splittable :: (Checkable a, Transformable a, Splittable a, HasDuration a) => a -> Property
_Splittable t = property sameDur
  where
    sameDur t a  = True   ==> _duration (beginning t a) ^+^ _duration (ending t a) === _duration (a .: t)

    -- ond n a = n /= 0 ==> _onset (delay n $ a .: t)       === _onset a  .+^ n
    -- ofd n a = n /= 0 ==> _offset (delay n $ a .: t)      === _offset a .+^ n
    -- sd n a  = n /= 0 ==> _duration (stretch n $ a .: t)  === n * _duration a
    -- cd n a  = n /= 0 ==> _duration (stretch (1/n) $ a .: t) === (1/n) * _duration a
    -- TODO more general


data BadMonoid = BM | BM2
  deriving (Eq, Ord, Show, Typeable)
instance Monoid BadMonoid where
  mempty = BM
  _ `mappend` _ = BM
instance Semigroup BadMonoid where
  (<>) = mappend
instance Arbitrary BadMonoid where
  arbitrary = elements [BM, BM2]


data BadFunctor a = BF1 | BF2
  deriving (Eq, Ord, Show, Typeable)
instance Functor BadFunctor where
  fmap f BF1 = BF2 -- lawless
  fmap f BF2 = BF1
instance Applicative BadFunctor where
  pure _ = BF1
  f <*> BF1 = BF2
  f <*> BF2 = BF1
instance Arbitrary (BadFunctor a) where
  arbitrary = elements [BF2, BF1]



sameType :: a -> a -> a
sameType _ x = x

infixl 9 .:
x .: t = x `asTypeOf` t

sameType1 :: f a -> f b -> f b
sameType1 _ x = x



instance CoArbitrary Time

instance Arbitrary Instrument where
  -- TODO can only select GM instruments!
  arbitrary = fromMidiProgram . (`mod` 128) <$> arbitrary

instance Arbitrary Pitch where
  arbitrary = (c .+^) <$> arbitrary

instance Arbitrary Part where
  arbitrary = do
    x <- arbitrary
    -- TODO also set subpart, division
    pure $ set instrument x mempty

instance Arbitrary Time where
  arbitrary = fmap toTime (arbitrary::Gen Double)
    where
      toTime :: Real a => a -> Time
      toTime = realToFrac
instance Arbitrary Duration where
  arbitrary = fmap toDuration (arbitrary::Gen Double)
    where
      toDuration :: Real a => a -> Duration
      toDuration = realToFrac

instance Arbitrary Span where
  arbitrary = liftA2 (<->) arbitrary arbitrary

instance Arbitrary a => Arbitrary (Placed a) where
  arbitrary = fmap (view placed) arbitrary

instance Arbitrary a => Arbitrary (Note a) where
  arbitrary = fmap (view note) arbitrary

instance Arbitrary a => Arbitrary (Event a) where
  arbitrary = fmap (view event) arbitrary

instance Arbitrary a => Arbitrary (AddMeta a) where
  arbitrary = fmap pure arbitrary

instance Arbitrary a => Arbitrary (Aligned a) where
  arbitrary = aligned <$> arbitrary <*> arbitrary <*> arbitrary

{-
instance (Ord a, Arbitrary a) => Arbitrary (Set.Set a) where
  arbitrary = fmap Set.fromList arbitrary

instance (Ord k, Arbitrary k, Ord a, Arbitrary a) => Arbitrary (Map.Map k a) where
  arbitrary = fmap Map.fromList $ liftA2 zip arbitrary arbitrary
-}

instance Arbitrary a => Arbitrary (Voice a) where
  arbitrary = fmap (view voice . take 3) arbitrary

instance Arbitrary a => Arbitrary (Score a) where
  arbitrary = fmap (view score . take 3) arbitrary

instance Arbitrary a => Arbitrary (Track a) where
  arbitrary = fmap (view track . take 3) arbitrary

instance Arbitrary a => Arbitrary (After a) where
  arbitrary = fmap After arbitrary

instance Arbitrary a => Arbitrary (Average a) where
  arbitrary = fmap Average arbitrary

instance Arbitrary a => Arbitrary (Behavior a) where
  arbitrary = view behavior <$> arbitrary

instance Arbitrary a => Arbitrary (Reactive a) where
  arbitrary = Music.Prelude.sample <$> arbitrary <*> arbitrary


-- TODO move
instance Semigroup a => Semigroup (Placed a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (Placed a) where
  mempty = pure mempty
  mappend = liftA2 mappend
instance Semigroup a => Semigroup (Note a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (Note a) where
  mempty = pure mempty
  mappend = liftA2 mappend
instance Semigroup a => Semigroup (Event a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (Event a) where
  mempty = pure mempty
  mappend = liftA2 mappend



unzipR f = (fmap fst f, fmap snd f)


-- main = quickCheck $ \() () -> True

main = defaultMain $ testGroup "all" [newTests, oldTests]

data TProxy (a :: k) where
  TP :: Typeable a => TProxy a

unT :: TProxy a -> Proxy a
unT _ = Proxy

applicative :: ( Applicative m
  , forall x . Eq x => Eq (m x)
  , forall x . Show x => Show (m x)
  , forall x . Arbitrary x => Arbitrary (m x)
  ) => TProxy m -> TestTree
applicative p@TP = testApplicativeLaws
    (unT p)
    (Proxy @())
    (Proxy @())
    (Proxy @Int)
    (Proxy @[Int])
    (\() -> (==))

monad :: (Monad m
  , forall x . Eq x => Eq (m x)
  , forall x . Show x => Show (m x)
  , forall x . Arbitrary x => Arbitrary (m x)
  ) => TProxy m -> TestTree
monad p@TP = testMonadLaws
    (unT p)
    (Proxy @())
    (Proxy @())
    (Proxy @Int)
    (Proxy @[Int])
    (\() -> (==))

newTests = testGroup "Instances (new tests)"
  [ testEqLaws (Proxy @(Reactive ()))
  , testEqLaws (Proxy @(Reactive Int))
  , applicative (TP @Maybe)
  , applicative (TP @Note)
  , applicative (TP @Event)
  , applicative (TP @Reactive)
  , applicative (TP @Voice)
  , applicative (TP @Score)
  , monad (TP @Maybe)
  , monad (TP @Note)
  , monad (TP @Event)
  , monad (TP @Voice)
  , monad (TP @Score)
  ]

oldTests =
  testGroup "Instances (old tests)" [
  testProperty "instance Monoid ()" $ _Monoid (undefined:: ()),
  testProperty "instance Monoid Sum Int" $ _Monoid (undefined:: Sum Int),
  testProperty "instance Monoid [Int]" $ _Monoid (undefined:: [Int]),

  testProperty "instance Monoid Average Rational" $ _Monoid (undefined:: Average Rational),
  testProperty "instance Monoid Average Double" $ _Monoid (undefined:: Average Double),

  testProperty "instance Monoid Time" $ _Monoid (undefined:: Time),
  testProperty "instance Monoid Duration" $ _Monoid (undefined:: Duration),
  testProperty "instance Monoid Span" $ _Monoid (undefined:: Span),

  testProperty "instance Monoid Event ()" $ _Monoid (undefined:: Event ()),
  testProperty "instance Monoid Placed ()" $ _Monoid (undefined:: Placed ()),
  testProperty "instance Monoid Note ()" $ _Monoid (undefined:: Note ()),

  testProperty "instance Monoid Voice Int" $ _Monoid (undefined:: Voice Int),
  testProperty "instance Monoid Score Int" $ _Monoid (undefined:: Score Int),

  -- TODO lawless!
  -- testProperty "instance Monoid (After (Score ()))" $ _Monoid (undefined:: After (Score ())),
  -- TODO lawless!
  -- testProperty "instance Monoid (After (Score Int))" $ _Monoid (undefined:: After (Score Int)),

  testProperty "instance Transformable ()" $ _Transformable (undefined:: ()),
  testProperty "instance Transformable Bool" $ _Transformable (undefined:: Bool),
  testProperty "instance Transformable Pitch" $ _Transformable (undefined:: Pitch),
  testProperty "instance Transformable Part" $ _Transformable (undefined:: Part),

  testProperty "instance Transformable Time" $ _Transformable (undefined:: Time),
  testProperty "instance Transformable Duration" $ _Transformable (undefined:: Duration),
  testProperty "instance Transformable Span" $ _Transformable (undefined:: Span),

  testProperty "instance Transformable [Time]" $ _Transformable (undefined:: [Time]),
  testProperty "instance Transformable [Duration]" $ _Transformable (undefined:: [Duration]),
  testProperty "instance Transformable [Span]" $ _Transformable (undefined:: [Span]),

  testProperty "instance Transformable Set.Set Time" $ _Transformable (undefined:: Set.Set Time),
  testProperty "instance Transformable Set.Set Duration" $ _Transformable (undefined:: Set.Set Duration),
  testProperty "instance Transformable Set.Set Span" $ _Transformable (undefined:: Set.Set Span),

  testProperty "instance Transformable Map.Map Int Time" $ _Transformable (undefined:: Map.Map Int Time),
  testProperty "instance Transformable Map.Map Int Duration" $ _Transformable (undefined:: Map.Map Int Duration),
  testProperty "instance Transformable Map.Map Int Span" $ _Transformable (undefined:: Map.Map Int Span),

  testProperty "instance Transformable Int" $ _Transformable (undefined:: Int),
  testProperty "instance Transformable Double" $ _Transformable (undefined:: Double),

  testProperty "instance Transformable Event Int" $ _Transformable (undefined:: Event Int),
  testProperty "instance Transformable Event Double" $ _Transformable (undefined:: Event Double),
  testProperty "instance Transformable Note Int" $ _Transformable (undefined:: Note Int),
  testProperty "instance Transformable Note Double" $ _Transformable (undefined:: Note Double),
  testProperty "instance Transformable Placed Int" $ _Transformable (undefined:: Placed Int),
  testProperty "instance Transformable Placed Double" $ _Transformable (undefined:: Placed Double),
  testProperty "instance Transformable AddMeta (Placed Double)" $ _Transformable (undefined:: AddMeta (Placed Double)),

  testProperty "instance Transformable Reactive Int" $ _Transformable (undefined:: Reactive Int),
  testProperty "instance Transformable Aligned Int" $ _Transformable (undefined:: Aligned Int),
  testProperty "instance Transformable Aligned (Voice Int)" $ _Transformable (undefined:: Aligned (Voice Int)),
  testProperty "instance HasPosition/Transformable (Aligned (Voice Int))" $ _HasPositionTransformable (undefined:: Aligned (Voice Int)),
  
  testProperty "instance HasPosition/HasDuration (Aligned (Voice Int))" $ _HasDurationHasPosition (undefined:: Aligned (Voice Int)),

  
  testProperty "instance Transformable Voice Int" $ _Transformable (undefined:: Voice Int),
  testProperty "instance Transformable Score Int" $ _Transformable (undefined:: Score Int),
  testProperty "instance Transformable Track Int" $ _Transformable (undefined:: Track Int),
  testProperty "instance Transformable [Voice Int]" $ _Transformable (undefined:: [Voice Int]),

  -- TODO requires comparison of rendered form
  -- Use newtype wrapper for Eq instance!
  -- Similarly for Behavior
  --  testProperty "instance Transformable Pattern Int" $ _Transformable (undefined:: Pattern Int),

  testProperty "instance HasDuration Span" $ _HasDuration (undefined:: Span),
  testProperty "instance HasDuration Event Int" $ _HasDuration (undefined:: Event Int),
  testProperty "instance HasDuration Event Double" $ _HasDuration (undefined:: Event Double),

  testProperty "instance HasPosition/HasDuration Span" $ _HasDurationHasPosition (undefined:: Span),
  testProperty "instance HasPosition/HasDuration Event Int" $ _HasDurationHasPosition (undefined:: Event Int),
  testProperty "instance HasPosition/HasDuration Event Double" $ _HasDurationHasPosition (undefined:: Event Double),
  testProperty "instance HasPosition/Transformable Score Int" $ _HasPositionTransformable (undefined:: Score Int),
  testProperty "instance HasPosition/HasDuration Event (Event Int)" $ _HasDurationHasPosition (undefined:: Event (Event Int)),
  testProperty "instance HasPosition/HasDuration Event (Score Int)" $ _HasDurationHasPosition (undefined:: Event (Score Int)),

  testProperty "instance Splittable Duration" $ _Splittable (undefined:: Duration),
  testProperty "instance Splittable AddMeta Duration" $ _Splittable (undefined:: AddMeta Duration),
  testProperty "instance Splittable Voice ()" $ _Splittable (undefined:: Voice ()),
  testProperty "instance Splittable Note ()" $ _Splittable (undefined:: Note ()),
  testProperty "instance Splittable Voice Int" $ _Splittable (undefined:: Voice Int),
  testProperty "instance Splittable Note Int" $ _Splittable (undefined:: Note Int),

  testProperty "instance Transformable Note [Event Int]" $ _Transformable (undefined:: Note [Event Int])

  ]

