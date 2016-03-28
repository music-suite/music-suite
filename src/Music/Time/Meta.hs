
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Provides a way to annotate data-types with 'Transformable' meta-data.
-- Inspired by Clojure meta-data and Diagrams styles.
--
module Music.Time.Meta (
        -- * Attributes
        AttributeClass,
        TAttributeClass,

        Attribute,

        -- ** Creating attributes
        wrapAttr,
        wrapTAttr,
        unwrapAttr,
        -- unwrapTAttr,

        -- * Meta-data
        Meta,

        -- ** Creating meta-data
        wrapMeta,
        wrapTMeta,
        unwrapMeta,

        -- ** The HasMeta class
        HasMeta(..),
        getMeta,
        mapMeta,
        setMeta,
        metaTypes,
        applyMeta,
        setMetaAttr,
        setMetaTAttr,
        preserveMeta,
        preserveMetaF,

        -- ** Add meta-data to arbitrary types
        AddMeta,
        annotated,
        unannotated,
        unsafeAnnotated
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens             hiding (transform)
import           Control.Monad.Plus
import           Data.Foldable            (Foldable)
import qualified Data.Foldable            as F
import           Data.Functor.Adjunction  (unzipR)
import           Data.Functor.Couple
import           Data.Functor.Rep
import qualified Data.List                as List
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.String
import           Data.Typeable

import           Music.Time.Internal.Util
import           Music.Time.Juxtapose

-- | Class of values that can be wrapped.
type AttributeClass a = (Typeable a, Monoid a, Semigroup a)

-- | Class of values that can be wrapped and transformed.
type TAttributeClass a = (Transformable a, AttributeClass a)

-- | An existential wrapper type to hold attributes.
data Attribute :: * where
  Attribute  :: AttributeClass a => a -> Attribute
  TAttribute :: TAttributeClass a  => a -> Attribute

-- | Wrap up an attribute.
wrapAttr :: AttributeClass a => a -> Attribute
wrapAttr = Attribute

-- | Wrap up a transformable attribute.
wrapTAttr :: TAttributeClass a => a -> Attribute
wrapTAttr = TAttribute

-- | Convert something from an attribute.
--   Also works with transformable attributes
unwrapAttr :: AttributeClass a => Attribute -> Maybe a
unwrapAttr (Attribute a)  = cast a
unwrapAttr (TAttribute a) = cast a

instance Semigroup Attribute where
  (Attribute a1) <> a2 = case unwrapAttr a2 of
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> Attribute (a1 <> a2')
  (TAttribute a1) <> a2 = case unwrapAttr a2 of
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> TAttribute (a1 <> a2')

instance Transformable Attribute where
  transform _ (Attribute a) = Attribute a
  transform s (TAttribute a) = TAttribute (transform s a)

instance Reversible Attribute where
  rev = id

-- Meta is Transformable because the contents of the map is transformable
newtype Meta = Meta { _getMeta :: Map String Attribute }
  deriving (Transformable, Reversible)

instance Semigroup Meta where
  Meta s1 <> Meta s2 = Meta $ Map.unionWith (<>) s1 s2

-- | The empty meta contains no attributes; composition of metas is
--   a union of attributes; if the two metas have attributes of the
--   same type they are combined according to their semigroup
--   structure.
instance Monoid Meta where
  mempty = Meta Map.empty
  mappend = (<>)

-- | Convert something to meta-data.
wrapTMeta :: forall a. TAttributeClass a => a -> Meta
wrapTMeta a = Meta $ Map.singleton key $ wrapTAttr a
  where
    key = show $ typeOf (undefined :: a)

-- | Convert something from meta-data.
unwrapMeta :: forall a. AttributeClass a => Meta -> Maybe a
unwrapMeta (Meta s) = (unwrapAttr =<<) $ Map.lookup key s
-- Note: unwrapAttr should never fail
  where
    key = show . typeOf $ (undefined :: a)

-- | Convert something from meta-data.
--   Also works with transformable attributes
wrapMeta :: forall a. AttributeClass a => a -> Meta
wrapMeta a = Meta $ Map.singleton key $ wrapAttr a
  where
    key = show $ typeOf (undefined :: a)


-- | Type class for things which have meta-data.
class HasMeta a where
  -- | Access the meta-data.
  meta :: Lens' a Meta

instance Show Meta where
  show _ = "{ meta }"

instance HasMeta Meta where
  meta = ($)

instance HasMeta a => HasMeta (Maybe a) where
  meta = lens viewM $ flip setM
    where
      viewM Nothing  = mempty
      viewM (Just x) = view meta x
      setM m = fmap (set meta m)

instance HasMeta a => HasMeta (b, a) where
  meta = _2 . meta

instance HasMeta a => HasMeta (Twain b a) where
  meta = _Wrapped . meta

-- | Extract meta-data.
getMeta :: HasMeta a => a -> Meta
getMeta = view meta

-- | Update meta-data.
setMeta :: HasMeta a => Meta -> a -> a
setMeta = set meta

-- | Map over meta-data.
mapMeta :: HasMeta a => (Meta -> Meta) -> a -> a
mapMeta = over meta

-- | Show the types of meta-data attachd to this value.
--   Useful for debugging.
metaTypes :: HasMeta a => a -> [String]
metaTypes x = Map.keys $ _getMeta $ x^.meta

-- | Apply meta-information by combining it with existing meta-information.
applyMeta :: HasMeta a => Meta -> a -> a
applyMeta m = over meta (<> m)

-- | Update a meta attribute.
setMetaAttr :: (AttributeClass b, HasMeta a) => b -> a -> a
setMetaAttr a = applyMeta (wrapMeta a)

-- | Update a meta attribute.
setMetaTAttr :: (TAttributeClass b, HasMeta a) => b -> a -> a
setMetaTAttr a = applyMeta (wrapTMeta a)

-- | Apply a function without affecting meta-data.
preserveMeta :: (HasMeta a, HasMeta b) => (a -> b) -> a -> b
preserveMeta f x = let m = view meta x in set meta m (f x)

-- | Apply a function without affecting meta-data.
preserveMetaF :: (HasMeta a, HasMeta b, Functor f) => (a -> f b) -> a -> f b
preserveMetaF f x = let m = view meta x in fmap (set meta m) (f x)

-- |
-- Annotate an arbitrary type with meta-data, preserving instances of
-- all common type classes. In particular 'Functor' and 'Applicative' is lifted and
-- @'Compose' 'AddMeta'@ is semantically equivalent to 'Identity'.

-- Meta-data is carried along with the annotated value. It defaults to 'mempty'
-- in 'pure'. When composing values using '<*>', 'liftA2' etc, meta-data is composed
-- using 'mappend'.
--
-- Similar to the approach taken in Clojure, meta-data does not contribute to ordering,
-- so both 'Eq' and 'Ord' ignore the meta-data.
--
-- You can access the meta-data using 'meta', and the annotated value using 'annotated'.
--
newtype AddMeta a = AddMeta { getAddMeta :: Meta `Twain` a }
  deriving (Show, Functor, Foldable, Typeable, Applicative, Monad, Comonad,
            Semigroup, Monoid, Num, Fractional, Floating, Enum, Bounded,
            Integral, Real, RealFrac, Eq, Ord)

instance Wrapped (AddMeta a) where
  type Unwrapped (AddMeta a) = Twain Meta a
  _Wrapped' = iso getAddMeta AddMeta

instance Rewrapped (AddMeta a) (AddMeta b)

instance HasMeta (AddMeta a) where
  -- twain, pair, element
  meta = _Wrapped . _Wrapped . _1

instance Traversable AddMeta where
  traverse = annotated

-- instance Eq1 AddMeta where
--   eq1 = (==)

-- instance FunctorWithIndex i AddMeta where
  -- imap f = over annotated $ imap f
--
-- instance FoldableWithIndex Span Score where
--   ifoldMap f (Score (m,x)) = ifoldMap f x
--
-- instance TraversableWithIndex Span Score where
--   itraverse f (Score (m,x)) = fmap (\x -> Score (m,x)) $ itraverse f x

instance Transformable a => Transformable (AddMeta a) where
  transform t = over meta (transform t) . over annotated (transform t)

instance Reversible a => Reversible (AddMeta a) where
  rev = over meta rev . over annotated rev

instance Splittable a => Splittable (AddMeta a) where
  split t = unzipR . fmap (split t)

instance HasPosition a => HasPosition (AddMeta a) where
  _era = _era . extract
  _position = _position . extract

instance HasDuration a => HasDuration (AddMeta a) where
  _duration = _duration . extract

-- |
-- Access the annotated value.
--
-- @
-- over annotated = fmap
-- @
--
annotated :: Lens (AddMeta a) (AddMeta b) a b
annotated = unsafeAnnotated

-- |
-- Access the annotated value.
--
-- @
-- view fromAnnotated = pure
-- @
--
unannotated :: Getter a (AddMeta a)
unannotated = from unsafeAnnotated

-- |
-- Access the annotated value. This is only an isomorphism up to meta-data
-- equivalence. In particular @under unsafeAnnotated@ leads to meta-data being
-- thrown away. See 'annotated' and 'unannotated' for safe (but less general)
-- definitions.
--
-- @
-- over annotated = fmap
-- @
--
unsafeAnnotated :: Iso (AddMeta a) (AddMeta b) a b
unsafeAnnotated = _Wrapped . extracted


-- Nice generalizations
-- TODO move

extracted :: (Applicative m, Comonad m) => Iso (m a) (m b) a b
extracted = iso extract pure

extractedRep :: (Representable m, w ~ Rep m, Monoid w) => Iso (m a) (m b) a b
extractedRep = iso extractRep pureRep
