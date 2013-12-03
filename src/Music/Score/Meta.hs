
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
        addGlobalMetaNote,
        runMeta,
        HasMeta(..),

        -- * Specific meta-values
        Clef(..),
        clef,
        clefDuring,

        TimeSignature,
        timeSignature,
        timeSignatureDuring,

        Fifths,
        KeySignature,
        key,
        keySignature,
        keySignatureDuring,

        Tempo,
        tempo,
        tempoDuring,

        Title,
        titleFromString,
        denoteTitle,
        getTitle,
        getTitleAt,    
        
        title,
        titleDuring,
        subtitle,
        subtitleDuring,
        
        Attribution,
        -- attribution,
        attribution1,
        getAttribution,
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


addGlobalMetaNote :: forall a b . (IsAttribute a, HasMeta b) => Note a -> b -> b
addGlobalMetaNote x = applyMeta $ addMeta' (Nothing::Maybe Int) $ noteToReactive x

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

clef :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Clef -> a -> a
clef c x = clefDuring (era x) c x

clefDuring :: (HasMeta a, HasPart' a) => Span -> Clef -> a -> a
clefDuring s c = addMetaNote (s =: (Option $ Just $ Last c))


newtype TimeSignature = TimeSignature ([Integer], Integer)
    deriving (Eq, Ord, Show, Typeable)

timeSignature :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => TimeSignature -> a -> a
timeSignature c x = timeSignatureDuring (era x) c x

timeSignatureDuring :: (HasMeta a, HasPart' a) => Span -> TimeSignature -> a -> a
timeSignatureDuring s c = addGlobalMetaNote (s =: (Option $ Just $ Last c))


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

keySignature :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => KeySignature -> a -> a
keySignature c x = keySignatureDuring (era x) c x

keySignatureDuring :: (HasMeta a, HasPart' a) => Span -> KeySignature -> a -> a
keySignatureDuring s c = addGlobalMetaNote (s =: (Option $ Just $ Last c))

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

tempo :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Tempo -> a -> a
tempo c x = tempoDuring (era x) c x

tempoDuring :: (HasMeta a, HasPart' a) => Span -> Tempo -> a -> a
tempoDuring s c = addGlobalMetaNote (s =: (Option $ Just $ Last c))





newtype Title = Title (Int -> Option (Last String))
    deriving (Typeable, Monoid, Semigroup)

instance IsString Title where
    fromString x = Title $ \n -> if n == 0 then Option (Just (Last x)) else Option Nothing

instance Show Title where
    show = List.intercalate " " . getTitle

-- | Create a title from a string. See also 'fromString'.
titleFromString :: String -> Title
titleFromString = fromString

-- | Denote a title to a lower level, i.e title becomes subtitle, subtitle becomes subsubtitle etc.
denoteTitle :: Title -> Title
denoteTitle (Title t) = Title (t . subtract 1)

-- | Extract the title as a descending list of title levels (i.e. title, subtitle, subsubtitle...).
getTitle :: Title -> [String]
getTitle t = untilFail . fmap (getTitleAt t) $ [0..]
    where
        untilFail = fmap fromJust . takeWhile isJust

-- | Extract the title of the given level. Semantic function.
getTitleAt :: Title -> Int -> Maybe String
getTitleAt (Title t) n = fmap getLast . getOption . t $ n

-- withTitle :: (Title -> Score a -> Score a) -> Score a -> Score a
-- withTitle = withMeta

-- | Set title of the given score.
title :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Title -> a -> a
title t x = titleDuring (era x) t x

-- | Set title of the given part of a score.
titleDuring :: (HasMeta a, HasPart' a) => Span -> Title -> a -> a
titleDuring s t = addGlobalMetaNote (s =: t)

-- | Set subtitle of the given score.
subtitle :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Title -> a -> a
subtitle t x = subtitleDuring (era x) t x

-- | Set subtitle of the given part of a score.
subtitleDuring :: (HasMeta a, HasPart' a) => Span -> Title -> a -> a
subtitleDuring s t = addGlobalMetaNote (s =: denoteTitle t)





-- | 
-- An attribution is a simple key-value store used to gather information such
-- as composer, lycicist, orchestrator, performer, etc.
-- 
newtype Attribution = Attribution (Map String (Option (Last String)))
    deriving (Typeable, Monoid, Semigroup)

instance Show Attribution where
    show (Attribution a) = "attribution " ++ show (Map.toList (fmap (fmap getLast . getOption) $ a))

-- | Make an 'Attribution' from keys and values.
attribution :: [(String, String)] -> Attribution
attribution = Attribution . fmap (Option . Just . Last) . Map.fromList

-- | Make an 'Attribution' a single key and value.
attribution1 :: String -> String -> Attribution
attribution1 k v = Attribution . fmap (Option . Just . Last) $ Map.singleton k v

-- | Extract an the given attribution value. Semantic function.
getAttribution :: Attribution -> String -> Maybe String
getAttribution (Attribution a) k = join $ k `Map.lookup` (fmap (fmap getLast . getOption) $ a)

-- | Set composer of the given score.
composer :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => String -> a -> a
composer t x = composerDuring (era x) t x

-- | Set composer of the given part of a score.
composerDuring :: (HasMeta a, HasPart' a) => Span -> String -> a -> a
composerDuring s c = addGlobalMetaNote (s =: attribution1 "composer" c)




newtype RehearsalMark = RehearsalMark ()
    deriving (Typeable, Monoid, Semigroup)









