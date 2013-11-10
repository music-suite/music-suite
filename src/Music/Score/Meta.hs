
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
-- Provides meta-information.
--
-- Each score supports an unlimited number of 'Reactive' meta-values.
--
-- This is more or less based on Diagrams styles, which is in turn based
-- on XMonad.
--
-------------------------------------------------------------------------------------


module Music.Score.Meta (
        -- * Attributes
        IsAttribute,
        Attribute,
        wrapAttr,
        unwrapAttr,

        -- * Meta-values
        Meta,  
        -- addMeta,
        addMetaNote,
        addMetaNoteNP,
        runMeta,
        HasMeta(..),

        -- * Specific meta-values
        Clef(..),
        setClef,
        setClefDuring,

        TimeSignature,
        setTimeSignature,
        setTimeSignatureDuring,

        Fifths,
        KeySignature,
        key,
        setKeySignature,
        setKeySignatureDuring,

        Tempo,
        setTempo,
        setTempoDuring,

        Title,
        mkTitle,
        denoteTitle,
        getTitle,
        getTitleAt,
        title,
        titleDuring,
        subtitle,
        subtitleDuring,
        
        Attribution,
        getAttribution,
        attribution,
        attribution1,
        composer,
        composerDuring,
        
  ) where

import Control.Arrow
import Control.Monad.Plus       
import Data.Void
import Data.Maybe
import Data.Semigroup
import Data.Monoid.WithSemigroup
import Data.Typeable
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
import Music.Score.Util
import Music.Pitch.Literal


type IsAttribute a = (Typeable a, Monoid' a)

-- | An existential wrapper type to hold attributes.
data Attribute :: * where
    Attribute  :: IsAttribute a => a -> Attribute
    -- TAttribute  :: (Transformable a, IsAttribute a) => a -> Attribute

-- | Wrap up an attribute.
wrapAttr :: IsAttribute a => a -> Attribute
wrapAttr = Attribute

unwrapAttr :: IsAttribute a => Attribute -> Maybe a
unwrapAttr (Attribute a)  = cast a

instance Semigroup Attribute where
    (Attribute a1) <> a2 = case unwrapAttr a2 of
        Nothing  -> a2
        Just a2' -> Attribute (a1 <> a2')

instance Delayable Attribute where
    delay _ (Attribute  a) = Attribute a
instance Stretchable Attribute where
    stretch _ (Attribute  a) = Attribute a


-- TODO is Transformable right w.r.t. join?
newtype Meta = Meta (Map String (Reactive Attribute))
    deriving (Delayable, Stretchable)

-- instance HasPart Meta where

inMeta :: (Map String (Reactive Attribute) -> Map String (Reactive Attribute)) -> Meta -> Meta
inMeta f (Meta s) = Meta (f s)


addMetaNoteNP :: forall a b . (IsAttribute a, HasMeta b) => Note a -> b -> b
addMetaNoteNP x = applyMeta $ addMeta' (Nothing::Maybe Int) $ noteToReactive x

-- XXX
addMetaNote :: forall a b . (IsAttribute a, HasMeta b, HasPart' b) => Note a -> b -> b
addMetaNote x y = (applyMeta $ addMeta' (Just y) $ noteToReactive x) y

runMeta :: forall a b . (HasPart' a, IsAttribute b) => Maybe a -> Meta -> Reactive b
runMeta part = fromMaybe mempty . runMeta' part


addMeta' :: forall a b . (HasPart' a, IsAttribute b) => Maybe a -> Reactive b -> Meta
addMeta' part a = Meta $ Map.singleton key $ fmap wrapAttr a
    where                   
        key = ty ++ pt
        pt = show $ fmap getPart part
        ty = show $ typeOf (undefined :: b)

-- runMeta' :: forall a . IsAttribute a => Meta -> Maybe (Reactive a) 
runMeta' :: forall a b . (HasPart' a, IsAttribute b) => Maybe a -> Meta -> Maybe (Reactive b) 
runMeta' part (Meta s) = fmap (fmap (fromMaybe (error "runMeta'") . unwrapAttr)) $ Map.lookup key s
-- Note: unwrapAttr should never fail
    where
        key = ty ++ pt
        pt = show $ fmap getPart part
        ty = show . typeOf $ (undefined :: b)

instance Semigroup Meta where
    Meta s1 <> Meta s2 = Meta $ Map.unionWith (<>) s1 s2

-- | The empty meta contains no attributes; composition of metas is
--   a union of attributes; if the two metas have attributes of the
--   same type they are combined according to their semigroup
--   structure.
instance Monoid Meta where
    mempty = Meta Map.empty
    mappend = (<>)

-- | Type class for things which have meta-information.
class HasMeta a where
    -- | Apply meta-information by combining it (on the left) with the
    --   existing meta-information.
    applyMeta :: Meta -> a -> a

instance HasMeta Meta where
    applyMeta = mappend

instance (HasMeta a, HasMeta b) => HasMeta (a,b) where
    applyMeta s = applyMeta s *** applyMeta s

instance HasMeta a => HasMeta [a] where
    applyMeta = fmap . applyMeta

instance HasMeta b => HasMeta (a -> b) where
    applyMeta = fmap . applyMeta

instance HasMeta a => HasMeta (Map k a) where
    applyMeta = fmap . applyMeta

instance (HasMeta a, Ord a) => HasMeta (Set a) where
    applyMeta = Set.map . applyMeta






data Clef = GClef | CClef | FClef
    deriving (Eq, Ord, Show, Typeable)

setClef :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Clef -> a -> a
setClef c x = setClefDuring (era x) c x

setClefDuring :: (HasMeta a, HasPart' a) => Span -> Clef -> a -> a
setClefDuring s c = addMetaNote (s =: (Option $ Just $ Last c))


newtype TimeSignature = TimeSignature ([Integer], Integer)
    deriving (Eq, Ord, Show, Typeable)

setTimeSignature :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => TimeSignature -> a -> a
setTimeSignature c x = setTimeSignatureDuring (era x) c x

setTimeSignatureDuring :: (HasMeta a, HasPart' a) => Span -> TimeSignature -> a -> a
setTimeSignatureDuring s c = addMetaNoteNP (s =: (Option $ Just $ Last c))


newtype Fifths = Fifths Integer
    deriving (Eq, Ord, Num, Enum, Integral, Real)

instance IsPitch Fifths where
    fromPitch (PitchL (d, fromMaybe 0 -> c, _)) = case (d,c) of
        (0,-1) -> (-7)
        (0, 0) -> 0
        (0, 1) -> 7
        
        (1,-1) -> (-5)
        (1, 0) -> 2
        (1, 1) -> 9
        
        (2,-1) -> (-3)
        (2, 0) -> 4
        (2, 1) -> 11
        
        (3,-1) -> (-8)
        (3, 0) -> (-1)
        (3, 1) -> 6
        
        (4,-1) -> (-6)
        (4, 0) -> 1
        (4, 1) -> 8
        
        (5,-1) -> (-4)
        (5, 0) -> 3
        (5, 1) -> 10

        (6,-1) -> (-2)
        (6, 0) -> 5
        (6, 1) -> 12

        _      -> error "Strange number of Fifths"

newtype KeySignature = KeySignature (Fifths, Bool)
    deriving (Eq, Ord, Typeable)

key :: Fifths -> Bool -> KeySignature
key fifths mode = KeySignature (fifths, mode)

setKeySignature :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => KeySignature -> a -> a
setKeySignature c x = setKeySignatureDuring (era x) c x

setKeySignatureDuring :: (HasMeta a, HasPart' a) => Span -> KeySignature -> a -> a
setKeySignatureDuring s c = addMetaNoteNP (s =: (Option $ Just $ Last c))

-- Tempo _ t means that
--  stretch t notation = sounding
--  1   -> whole = 1   sec (1/1 = 60)
--  0.5 -> whole = 0.5 sec (1/1 = 120)
--  2   -> whole = 2   sec (1/4 = 120)
data Tempo = Tempo (Maybe String) Duration
    deriving (Eq, Ord, Show, Typeable)

-- beats +, tempo -, duration +
-- bpm +,   tempo +  duration -


metronome :: Duration -> Duration -> Tempo
metronome noteVal bpm = Tempo Nothing $ (noteVal * 60) / bpm

setTempo :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Tempo -> a -> a
setTempo c x = setTempoDuring (era x) c x

setTempoDuring :: (HasMeta a, HasPart' a) => Span -> Tempo -> a -> a
setTempoDuring s c = addMetaNoteNP (s =: (Option $ Just $ Last c))





newtype Title = Title (Int -> Option (Last String))
    deriving (Typeable, Monoid, Semigroup)

instance IsString Title where
    fromString x = Title $ \n -> if n == 0 then Option (Just (Last x)) else Option Nothing

instance Show Title where
    show = List.intercalate " " . getTitle

mkTitle :: String -> Title
mkTitle = fromString

denoteTitle :: Title -> Title
denoteTitle (Title t) = Title (t . subtract 1)

getTitle :: Title -> [String]
getTitle t = untilFail . fmap (getTitleAt t) $ [0..]
    where
        untilFail = fmap fromJust . takeWhile isJust

getTitleAt :: Title -> Int -> Maybe String
getTitleAt (Title t) n = fmap getLast . getOption . t $ n

-- withTitle :: (Title -> Score a -> Score a) -> Score a -> Score a
-- withTitle = withMeta

title :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Title -> a -> a
title t x = titleDuring (era x) t x

titleDuring :: (HasMeta a, HasPart' a) => Span -> Title -> a -> a
titleDuring s t = addMetaNoteNP (s =: t)

subtitle :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Title -> a -> a
subtitle t x = subtitleDuring (era x) t x

subtitleDuring :: (HasMeta a, HasPart' a) => Span -> Title -> a -> a
subtitleDuring s t = addMetaNoteNP (s =: denoteTitle t)








newtype Attribution = Attribution (Map String (Option (Last String)))
    deriving (Typeable, Monoid, Semigroup)

instance Show Attribution where
    show (Attribution a) = "attribution " ++ show (Map.toList (fmap (fmap getLast . getOption) $ a))

attribution :: [(String, String)] -> Attribution
attribution = Attribution . fmap (Option . Just . Last) . Map.fromList

attribution1 :: String -> String -> Attribution
attribution1 k v = Attribution . fmap (Option . Just . Last) $ Map.singleton k v

getAttribution :: Attribution -> String -> Maybe String
getAttribution (Attribution a) k = join $ k `Map.lookup` (fmap (fmap getLast . getOption) $ a)


composer :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => String -> a -> a
composer t x = composerDuring (era x) t x

composerDuring :: (HasMeta a, HasPart' a) => Span -> String -> a -> a
composerDuring s c = addMetaNoteNP (s =: attribution1 "composer" c)



