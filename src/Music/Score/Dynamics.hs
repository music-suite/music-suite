                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
    GeneralizedNewtypeDeriving #-} 

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
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------


module Music.Score.Dynamics (
        HasDynamic(..),
        DynamicT(..),

        -- ** Dynamics over time
        Levels(..),
        cresc,
        dim,

        -- ** Application
        dynamicSingle,
        dynamics,
  ) where

import Control.Monad
import Data.Semigroup
import Data.Ratio
import Data.Foldable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Part
import Music.Score.Score
import Music.Score.Duration
import Music.Score.Time
import Music.Score.Voice
import Music.Score.Combinators

import Music.Dynamics.Literal

class HasDynamic a where
    setBeginCresc   :: Bool -> a -> a
    setEndCresc     :: Bool -> a -> a
    setBeginDim     :: Bool -> a -> a
    setEndDim       :: Bool -> a -> a
    setLevel        :: Double -> a -> a

-- end cresc/dim, level, begin cresc/dim
newtype DynamicT a = DynamicT { getDynamicT :: (Bool, Bool, Maybe Double, a, Bool, Bool) }
    deriving (Eq, Show, Ord, Functor, Foldable)



--------------------------------------------------------------------------------
-- Dynamics
--------------------------------------------------------------------------------

-- Apply a constant level over the whole score.
-- dynamic :: (HasDynamic a, HasVoice a, Ord v, v ~ Voice a) => Double -> Score a -> Score a
-- dynamic n = mapSep (setLevel n) id id 


dynamics :: (HasDynamic a, HasVoice a, Ord v, v ~ Voice a) => Score (Levels Double) -> Score a -> Score a
dynamics d a = (duration a `stretchTo` d) `dyns` a

dynamicSingle :: HasDynamic a => Score (Levels Double) -> Score a -> Score a
dynamicSingle d a  = (duration a `stretchTo` d) `dyn` a

-- | Apply a variable level over the score.
dyns :: (HasDynamic a, HasVoice a, Ord v, v ~ Voice a) => Score (Levels Double) -> Score a -> Score a
dyns ds = mapVoices (fmap $ applyDynSingle (fmap fromJust $ scoreToPart ds))

-- | Apply a variable level over a single-part score.
dyn :: HasDynamic a => Score (Levels Double) -> Score a -> Score a
dyn ds = applyDynSingle (fmap fromJust . scoreToPart $ ds)



-- |
-- Represents dynamics over a duration.
--
data Levels a
    = Level  a
    | Change a a
    deriving (Eq, Show)

instance Fractional a => IsDynamics (Levels a) where
    fromDynamics (DynamicsL (Just a, Nothing)) = Level (toFrac a)
    fromDynamics (DynamicsL (Just a, Just b))  = Change (toFrac a) (toFrac b)
    fromDynamics x = error $ "fromDynamics: Invalid dynamics literal " {- ++ show x-}

cresc :: IsDynamics a => Double -> Double -> a
cresc a b = fromDynamics $ DynamicsL ((Just a), (Just b))

dim :: IsDynamics a => Double -> Double -> a
dim a b = fromDynamics $ DynamicsL ((Just a), (Just b))


-- end cresc, end dim, level, begin cresc, begin dim
type Levels2 a = (Bool, Bool, Maybe a, Bool, Bool)

dyn2 :: Ord a => [Levels a] -> [Levels2 a]
dyn2 = snd . List.mapAccumL g (Nothing, False, False) -- level, cresc, dim
    where
        g (Nothing, False, False) (Level b)     = ((Just b,  False, False), (False, False, Just b,  False, False))
        g (Nothing, False, False) (Change b c)  = ((Just b,  b < c, b > c), (False, False, Just b,  b < c, b > c))

        g (Just a , cr, dm) (Level b) 
            | a == b                            = ((Just b,  False, False), (cr,    dm,    Nothing, False, False))
            | a /= b                            = ((Just b,  False, False), (cr,    dm,    Just b,  False, False))
        g (Just a , cr, dm) (Change b c) 
            | a == b                            = ((Just b,  b < c, b > c), (cr,    dm,    Nothing, b < c, b > c))
            | a /= b                            = ((Just b,  b < c, b > c), (cr,    dm,    Just b,  b < c, b > c))



transf :: ([a] -> [b]) -> Part a -> Part b
transf f = Part . uncurry zip . second f . unzip . getPart

applyDynSingle :: HasDynamic a => Part (Levels Double) -> Score a -> Score a
applyDynSingle ds as = applySingle' ds3 as
    where
        -- ds2 :: Part (Dyn2 Double)
        ds2 = transf dyn2 ds
        -- ds3 :: Part (Score a -> Score a)
        ds3 = (flip fmap) ds2 g
        
        g (ec,ed,l,bc,bd) = id
                . (if ec then map1 (setEndCresc     True) else id)
                . (if ed then map1 (setEndDim       True) else id)
                . (if bc then map1 (setBeginCresc   True) else id)
                . (if bd then map1 (setBeginDim     True) else id)
                . (maybe id (\x -> map1 (setLevel x)) $ l)
        map1 f = mapSepPart f id id


-- applySingle :: Part (a -> b) -> Score a -> Score b
-- applySingle fs = applySingle' (fmap fmap fs)




-- TODO move these
-- FIXME work with infinite parts
applySingle' :: Part (Score a -> Score b) -> Score a -> Score b
applySingle' fs as = notJoin $ fmap (\(f,s) -> f s) $ sampled
    where            
        -- This is not join; we simply concatenate all inner scores in parallel
        notJoin = mconcat . toList
        sampled = sampleSingle (partToScore fs) as

-- |
-- Get all notes that start during a given note.
--
sampleSingle :: Score a -> Score b -> Score (a, Score b)
sampleSingle as bs = Score . fmap (\(t,d,a) -> (t,d,g a (onsetIn t d bs))) . getScore $ as
    where
        g Nothing  z = Nothing
        g (Just a) z = Just (a,z)


-- | Filter out events that has its onset in the given time interval (inclusive start).
--   For example, onset in 1 2 filters events such that (1 <= onset x < 3)
onsetIn :: Time -> Duration -> Score a -> Score a
onsetIn a b = Score . mfilter (\(t,d,x) -> a <= t && t < a .+^ b) . getScore 
                                                                              
                                                                              
                                                                              







-- FIXME consolidate

-- | 
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
-- 
mapSepL :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapSepL f g h []      = []
mapSepL f g h [a]     = [f a]
mapSepL f g h [a,b]   = [f a, h b]
mapSepL f g h xs      = [f $ head xs] ++ (map g $ tail $ init xs) ++ [h $ last xs]

mapSep :: (HasVoice a, Ord v, v ~ Voice a) => (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSep f g h sc = fixDur . mapVoices (fmap $ mapSepPart f g h) $ sc
    where
        fixDur a = padAfter (duration sc - duration a) a

mapSepPart :: (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSepPart f g h sc = mconcat . mapSepL (fmap f) (fmap g) (fmap h) . fmap toSc . perform $ sc
    where             
        fixDur a = padAfter (duration sc - duration a) a
        toSc (t,d,x) = delay (t .-. 0) . stretch d $ note x
        third f (a,b,c) = (a,b,f c)

padAfter :: Duration -> Score a -> Score a
padAfter d a = a |> (rest^*d)       




second :: (a -> b) -> (c,a) -> (c,b)
second f (a,b) = (a,f b)

toFrac :: (Real a, Fractional b) => a -> b
toFrac = fromRational . toRational

fromJust (Just x) = x
