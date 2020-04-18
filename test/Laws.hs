

{-# LANGUAGE CPP #-}
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

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen
import Test.Tasty.QuickCheck.Laws( testMonadLaws )

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
    te x     = True                               ==> transform mempty x === x .: t
    tc s t x = isForwardSpan s && isForwardSpan t ==> transform (s <> t) x === transform s (transform t $ x .: t)
    tn s x   = isForwardSpan s                    ==> transform (s <> negateV s) x === x .: t

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

instance Arbitrary DiatonicSteps where
  arbitrary = DiatonicSteps <$> arbitrary

instance Arbitrary ChromaticSteps where
  arbitrary = ChromaticSteps <$> arbitrary

instance Arbitrary Instrument where
  -- TODO can only select GM instruments!
  arbitrary = fromMidiProgram . (`mod` 128) <$> arbitrary

instance Arbitrary Interval where
  arbitrary = Interval <$> arbitrary

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

{-
instance (Ord a, Arbitrary a) => Arbitrary (Set.Set a) where
  arbitrary = fmap Set.fromList arbitrary

instance (Ord k, Arbitrary k, Ord a, Arbitrary a) => Arbitrary (Map.Map k a) where
  arbitrary = fmap Map.fromList $ liftA2 zip arbitrary arbitrary
-}

instance Arbitrary a => Arbitrary (Voice a) where
  arbitrary = fmap (view voice) arbitrary
-- instance Arbitrary a => Arbitrary (Chord a) where
  -- arbitrary = fmap (view chord) arbitrary
instance Arbitrary a => Arbitrary (Score a) where
  arbitrary = fmap (view score) arbitrary
instance Arbitrary a => Arbitrary (Track a) where
  arbitrary = fmap (view track) arbitrary

instance Arbitrary a => Arbitrary (After a) where
  arbitrary = fmap After arbitrary

-- instance Arbitrary a => Arbitrary (Reactive a) where
  -- arbitrary = liftA2 zip arbitrary arbitrary


-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = fmap Sum arbitrary
-- instance Arbitrary a => Arbitrary (Product a) where
--   arbitrary = fmap Product arbitrary
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


-- instance Ord a => Ord (Event a) where
--   x `compare` y = (x^.from note) `compare` (y^.from note)
instance Eq a => Eq (Score a) where
  x == y = Data.List.sortBy (comparing (^.era)) (x^.events) == Data.List.sortBy (comparing (^.era)) (y^.events)
-- instance Splittable Integer where
  -- split _ x = (x,x)

unzipR f = (fmap fst f, fmap snd f)


-- main = quickCheck $ \() () -> True

#define A_TEST(EXPR) (testProperty "EXPR" $ EXPR)

#define I_TEST2(INSTANCE_NAME,CLASS,TYPE) ( \
  testProperty ("instance "++ INSTANCE_NAME) $ (CLASS (undefined::TYPE)) \
  )


main = defaultMain $ testGroup "all" [newTests, oldTests]

data TProxy (a :: k) where
  TP :: Typeable a => TProxy a

unT :: TProxy a -> Proxy a
unT _ = Proxy

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
  [ monad (TP @Maybe)
  , monad (TP @Voice)
  , monad (TP @Score)
  ]

oldTests =
  testGroup "Instances (old tests)" [

  I_TEST2("Monoid ()", _Monoid, ()),
  I_TEST2("Monoid Sum Int", _Monoid, Sum Int),
  I_TEST2("Monoid [Int]", _Monoid, [Int]),

  I_TEST2("Monoid Average Rational", _Monoid, Average Rational),
  I_TEST2("Monoid Average Double", _Monoid, Average Double),

  I_TEST2("Monoid Time", _Monoid, Time),
  I_TEST2("Monoid Duration", _Monoid, Duration),
  I_TEST2("Monoid Span", _Monoid, Span),

  I_TEST2("Monoid Event ()", _Monoid, Event ()),
  I_TEST2("Monoid Placed ()", _Monoid, Placed ()),
  I_TEST2("Monoid Note ()", _Monoid, Note ()),

  I_TEST2("Monoid Voice Int", _Monoid, Voice Int),
  I_TEST2("Monoid Score Int", _Monoid, Score Int),

  -- TODO lawless!
  -- I_TEST2("Monoid (After (Score ()))", _Monoid, After (Score ())),
  -- TODO lawless!
  -- I_TEST2("Monoid (After (Score Int))", _Monoid, After (Score Int)),

  I_TEST2("Transformable ()", _Transformable, ()),
  I_TEST2("Transformable Bool", _Transformable, Bool),
  I_TEST2("Transformable Pitch", _Transformable, Pitch),
  I_TEST2("Transformable Part", _Transformable, Part),

  I_TEST2("Transformable Time", _Transformable, Time),
  I_TEST2("Transformable Duration", _Transformable, Duration),
  I_TEST2("Transformable Span", _Transformable, Span),

  I_TEST2("Transformable [Time]", _Transformable, [Time]),
  I_TEST2("Transformable [Duration]", _Transformable, [Duration]),
  I_TEST2("Transformable [Span]", _Transformable, [Span]),

  I_TEST2("Transformable Set.Set Time", _Transformable, Set.Set Time),
  I_TEST2("Transformable Set.Set Duration", _Transformable, Set.Set Duration),
  I_TEST2("Transformable Set.Set Span", _Transformable, Set.Set Span),

  I_TEST2("Transformable Map.Map Int Time", _Transformable, Map.Map Int Time),
  I_TEST2("Transformable Map.Map Int Duration", _Transformable, Map.Map Int Duration),
  I_TEST2("Transformable Map.Map Int Span", _Transformable, Map.Map Int Span),

  I_TEST2("Transformable Int", _Transformable, Int),
  I_TEST2("Transformable Double", _Transformable, Double),

  I_TEST2("Transformable Event Int", _Transformable, Event Int),
  I_TEST2("Transformable Event Double", _Transformable, Event Double),
  I_TEST2("Transformable Note Int", _Transformable, Note Int),
  I_TEST2("Transformable Note Double", _Transformable, Note Double),
  I_TEST2("Transformable Placed Int", _Transformable, Placed Int),
  I_TEST2("Transformable Placed Double", _Transformable, Placed Double),
  I_TEST2("Transformable AddMeta (Placed Double)", _Transformable, AddMeta (Placed Double)),

  I_TEST2("Transformable Reactive Int", _Transformable, Reactive Int),

  I_TEST2("Transformable Voice Int", _Transformable, Voice Int),
  I_TEST2("Transformable Score Int", _Transformable, Score Int),
  I_TEST2("Transformable Track Int", _Transformable, Track Int),
  I_TEST2("Transformable [Voice Int]", _Transformable, [Voice Int]),

  I_TEST2("HasDuration Span", _HasDuration, Span),
  I_TEST2("HasDuration Event Int", _HasDuration, Event Int),
  I_TEST2("HasDuration Event Double", _HasDuration, Event Double),

  I_TEST2("HasPosition/HasDuration Span", _HasDurationHasPosition, Span),
  I_TEST2("HasPosition/HasDuration Event Int", _HasDurationHasPosition, Event Int),
  I_TEST2("HasPosition/HasDuration Event Double", _HasDurationHasPosition, Event Double),
  I_TEST2("HasPosition/Transformable Score Int", _HasPositionTransformable, Score Int),
  I_TEST2("HasPosition/HasDuration Event (Event Int)", _HasDurationHasPosition, Event (Event Int)),
  I_TEST2("HasPosition/HasDuration Event (Score Int)", _HasDurationHasPosition, Event (Score Int)),

  I_TEST2("Splittable Duration", _Splittable, Duration),
  I_TEST2("Splittable AddMeta Duration", _Splittable, AddMeta Duration),
  I_TEST2("Splittable Voice ()", _Splittable, Voice ()),
  I_TEST2("Splittable Note ()", _Splittable, Note ()),
  I_TEST2("Splittable Voice Int", _Splittable, Voice Int),
  I_TEST2("Splittable Note Int", _Splittable, Note Int),

  I_TEST2("Transformable Note [Event Int]", _Transformable, Note [Event Int])

  ]

