

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}


import Prelude hiding ((**))

import Data.Monoid.Average
import Data.Ord (comparing)
import Music.Prelude hiding (defaultMain, elements)

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen

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

_HasPosition :: (Checkable a, Transformable a, HasPosition a) => a -> Property
_HasPosition t = eqd .&&. ond .&&. ofd .&&. sd .&&. ass
  where
    eqd a   = True   ==> _duration a                        === _offset a .-. _onset (a .: t)
    ond n a = n /= 0 ==> _onset (delay n $ a .: t)          === _onset a  .+^ n
    ofd n a = n /= 0 ==> _offset (delay n $ a .: t)         === _offset a .+^ n
    sd n a  = n /= 0 ==> _duration (stretch n $ a .: t)     === n * _duration a
    ass s a p = True ==> (s `transform` (a .: t)) `_position` p === s `transform` (a `_position` p)
    -- TODO more general

_onset, _offset :: (HasPosition a, Transformable a) => a -> Time
_onset = view onset
_offset = view offset

{-
  _duration (beginning t x) + _duration (ending t x) = _duration x

  _duration (beginning t x) = t `min` _duration x
    iff t >= 0
-}
_Splittable :: (Checkable a, Transformable a, Splittable a, HasDuration a) => a -> Property
_Splittable t = sameDur .&&. minBegin
  where
    sameDur t a  = True   ==> _duration (beginning t a) ^+^ _duration (ending t a) === _duration (a .: t)
    minBegin     = True   ==> 1 === (1::Int)

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

-- instance Arbitrary a => Arbitrary (Reactive a) where
  -- arbitrary = liftA2 zip arbitrary arbitrary


-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = fmap Sum arbitrary
-- instance Arbitrary a => Arbitrary (Product a) where
--   arbitrary = fmap Product arbitrary
instance Arbitrary a => Arbitrary (Average a) where
  arbitrary = fmap Average arbitrary


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


main = defaultMain $ testGroup "Instances" $ [
  -- I_TEST2("Moinoid BadMonoid", _Monoid, BadMonoid),

  I_TEST2("Monoid ()", _Monoid, ()),
  I_TEST2("Monoid Sum Int", _Monoid, Sum Int),
  I_TEST2("Monoid [Int]", _Monoid, [Int]),

  -- SLOW I_TEST(_Monoid, Average Rational)
  I_TEST2("Monoid Average Double", _Monoid, Average Double),

  I_TEST2("Monoid Time", _Monoid, Time),
  I_TEST2("Monoid Duration", _Monoid, Duration),
  I_TEST2("Monoid Span", _Monoid, Span),

  I_TEST2("Monoid Event ()", _Monoid, Event ()),
  I_TEST2("Monoid Placed ()", _Monoid, Placed ()),
  I_TEST2("Monoid Note ()", _Monoid, Note ()),

  I_TEST2("Monoid Voice Int", _Monoid, Voice Int),
  -- I_TEST2("Monoid Chord Int", _Monoid, Chord Int),
  I_TEST2("Monoid Score Int", _Monoid, Score Int),



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

  -- TODO how to test "pointwise" for Segment and Behavior
  -- I_TEST2("Transformable Reactive Int", _Transformable, Reactive Int),

  I_TEST2("Transformable Voice Int", _Transformable, Voice Int),
  -- I_TEST2("Transformable Chord Int", _Transformable, Chord Int),
  I_TEST2("Transformable Score Int", _Transformable, Score Int),
  I_TEST2("Transformable Track Int", _Transformable, Track Int),
  -- SLOW I_TEST2("Transformable [Voice Int]", _Transformable, [Voice Int]),
  -- SLOW I_TEST2("Transformable [Chord Int]", _Transformable, [Chord Int]),
  -- SLOW I_TEST2("Transformable [Score Int]", _Transformable, [Score Int]),

  -- I_TEST2("HasDuration Time", _HasDuration, Time),
  I_TEST2("HasDuration Span", _HasDuration, Span),
  I_TEST2("HasDuration Event Int", _HasDuration, Event Int),
  I_TEST2("HasDuration Event Double", _HasDuration, Event Double),
  -- I_TEST2("HasDuration Placed Int", _HasDuration, Placed Int),
  -- I_TEST2("HasDuration Placed Double", _HasDuration, Placed Double),

  I_TEST2("HasDuration Score Int", _HasDuration, Score Int),
  -- I_TEST2("HasDuration Chord Int", _HasDuration, Chord Int),
  -- TODO remove instance I_TEST2("HasDuration [Score Int]", _HasDuration, [Score Int]),
  -- TODO remove instance I_TEST2("HasDuration [Chord Int]", _HasDuration, [Chord Int]),


  I_TEST2("HasPosition Span", _HasPosition, Span),
  I_TEST2("HasPosition Event Int", _HasPosition, Event Int),
  I_TEST2("HasPosition Event Double", _HasPosition, Event Double),
  -- I_TEST2("HasPosition Placed Int", _HasPosition, Placed Int),
  -- I_TEST2("HasPosition Placed Double", _HasPosition, Placed Double),
  I_TEST2("HasPosition Score Int", _HasPosition, Score Int),
  I_TEST2("HasPosition Event (Event Int)", _HasPosition, Event (Event Int)),
  I_TEST2("HasPosition Event (Score Int)", _HasPosition, Event (Score Int)),
  -- I_TEST2("HasPosition Score (Placed Int)", _HasPosition, Score (Placed Int)),

  -- I_TEST2("HasPosition AddMeta (Placed Duration)", _HasPosition, AddMeta (Placed Duration)),
  -- I_TEST2("HasPosition Chord Int", _HasPosition, Chord Int),
  -- TODO remove instance I_TEST2("HasPosition [Score Int]", _HasPosition, [Score Int]),
  -- TODO remove instance I_TEST2("HasPosition [Chord Int]", _HasPosition, [Chord Int]),










  -- Test meaningless... I_TEST2("Splittable ()", _Splittable, ()),
  I_TEST2("Splittable Duration", _Splittable, Duration),
  -- I_TEST2("Splittable Span", _Splittable, Span),
  -- TODO arbitrary I_TEST2("Splittable Meta", _Splittable, Meta),
  -- TODO arbitrary I_TEST2("Splittable Attribute", _Splittable, Attribute),
  I_TEST2("Splittable AddMeta Duration", _Splittable, AddMeta Duration),

  -- TODO remove instance I_TEST2("Splittable [Duration]", _Splittable, [Duration]),
  -- TODO remove instance I_TEST2("Splittable [Span]", _Splittable, [Span]),

  -- I_TEST2("Splittable Set.Set Duration", _Splittable, Set.Set Duration),
  -- I_TEST2("Splittable Set.Set Span", _Splittable, Set.Set Span),
  -- I_TEST2("Splittable Map.Map Int Duration", _Splittable, Map.Map Int Duration),
  -- I_TEST2("Splittable Map.Map Int Span", _Splittable, Map.Map Int Span),

  -- I_TEST2("Splittable Int", _Splittable, Int),
  -- I_TEST2("Splittable Double", _Splittable, Double),
  -- I_TEST2("Splittable Event Int", _Splittable, Event Int),
  -- I_TEST2("Splittable Event Double", _Splittable, Event Double),
  -- I_TEST2("Splittable Note Int", _Splittable, Note Int),
  -- I_TEST2("Splittable Note Double", _Splittable, Note Double),
  -- I_TEST2("Splittable Placed Int", _Splittable, Placed Int),
  -- I_TEST2("Splittable Placed Double", _Splittable, Placed Double),



  -- TODO how to test "pointwise" for Segment and Behavior
  -- I_TEST2("Splittable Reactive Int", _Splittable, Reactive Int),

  -- I_TEST2("Splittable Voice Int", _Splittable, Voice Int),
--  I_TEST2("Splittable Track Int", _Splittable, Track Int),
  -- I_TEST2("Splittable Chord Int", _Splittable, Chord Int),
  -- I_TEST2("Splittable Score Int", _Splittable, Score Int),
  -- I_TEST2("Splittable Event (Event Int)", _Splittable, Event (Event Int)),



  I_TEST2("Transformable Note [Event Int]", _Transformable, Note [Event Int])

  ]



{-
OK

let t = -8
let s = [(5,3)^.note,((-3),-4)^.note]^.voice
stretch t (_duration s)
_duration (stretch t s)


FAIL

let t = -8
let s = [(5 <-> 23,3)^.event,(3 <-> (-3),-4)^.event]^.score
stretch t (_duration s)
_duration (stretch t s)
-}
