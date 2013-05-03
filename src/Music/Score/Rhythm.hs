
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,     
    GeneralizedNewtypeDeriving,
    ScopedTypeVariables #-} 

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

module Music.Score.Rhythm (
        -- * Rhythm type
        Rhythm(..), 

        -- * Quantization
        quantize,
        dotMod,
        -- dotMods,
        -- tupletMods,
  ) where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Control.Applicative
import Control.Monad (ap, join, MonadPlus(..))
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio
import Data.VectorSpace

import Text.Parsec hiding ((<|>))
import Text.Parsec.Pos

import Music.Score.Time
import Music.Score.Duration
import Music.Score.Ties


data Rhythm a 
    = Beat       Duration a                    -- d is divisible by 2
    | Dotted     Int (Rhythm a)                -- n > 0.
    | Tuplet     Duration (Rhythm a)           -- d is an emelent of 'tupletMods'.
    | Bound      Duration (Rhythm a)           -- tied from duration
    | Rhythms    [Rhythm a]                    -- normal note sequence
    deriving (Eq, Show, Functor, Foldable)
    -- RInvTuplet  Duration (Rhythm a)

instance Semigroup (Rhythm a) where
    (<>) = mappend

-- Catenates using 'Rhythms'
instance Monoid (Rhythm a) where
    mempty = Rhythms []
    Rhythms as `mappend` Rhythms bs   =  Rhythms (as <> bs)
    r          `mappend` Rhythms bs   =  Rhythms ([r] <> bs)
    Rhythms as `mappend` r            =  Rhythms (as <> [r])

instance AdditiveGroup (Rhythm a) where
    zeroV   = error "No zeroV for (Rhythm a)"
    (^+^)   = error "No ^+^ for (Rhythm a)"
    negateV = error "No negateV for (Rhythm a)"

instance VectorSpace (Rhythm a) where
    type Scalar (Rhythm a) = Duration
    a *^ Beat d x = Beat (a*d) x

Beat d x `subDur` d' = Beat (d-d') x

instance HasDuration (Rhythm a) where
    duration (Beat d _)        = d
    duration (Dotted n a)      = duration a * dotMod n
    duration (Tuplet c a)      = duration a * c
    duration (Bound d a)       = duration a + d
    duration (Rhythms as)      = sum (fmap duration as)    

quantize :: [(Duration, a)] -> Either String (Rhythm a)
quantize = quantize' (atEnd rhythm)


-- Internal...

testQuantize :: [Duration] -> Either String (Rhythm ())
testQuantize = quantize' (atEnd rhythm) . fmap (\x->(x,()))

dotMod :: Int -> Duration
dotMod n = dotMods !! (n-1)

-- [3/2, 7/4, 15/8, 31/16 ..]
dotMods :: [Duration]
dotMods = zipWith (/) (fmap pred $ drop 2 times2) (drop 1 times2)
    where
        times2 = iterate (*2) 1

tupletMods :: [Duration]
tupletMods = [2/3, 4/5, {-4/6,-} 4/7, 8/9]


-- 3/2 for dots
-- 2/3, 4/5, 4/6, 4/7, 8/9, 8/10, 8/11  for ordinary tuplets
-- 3/2,      6/4                        for inverted tuplets

data RState = RState {
        timeMod :: Duration, -- time modification; notatedDur * timeMod = actualDur
        timeSub :: Duration, -- time subtraction (in bound note)
        tupleDepth :: Int
    }

instance Monoid RState where
    mempty = RState { timeMod = 1, timeSub = 0, tupleDepth = 0 }
    a `mappend` _ = a

modifyTimeMod :: (Duration -> Duration) -> RState -> RState
modifyTimeMod f (RState tm ts td) = RState (f tm) ts td

modifyTimeSub :: (Duration -> Duration) -> RState -> RState
modifyTimeSub f (RState tm ts td) = RState tm (f ts) td

modifyTupleDepth :: (Int -> Int) -> RState -> RState
modifyTupleDepth f (RState tm ts td) = RState tm ts (f td)

-- | 
-- A @RhytmParser a b@ converts (Part a) to b.
type RhythmParser a b = Parsec [(Duration, a)] RState b

quantize' :: RhythmParser a b -> [(Duration, a)] -> Either String b
quantize' p = left show . runParser p mempty ""

-- Matches a (duration, value) pair iff the predicate matches, returns beat
match :: (Duration -> a -> Bool) -> RhythmParser a (Rhythm a)
match p = tokenPrim show next test
    where
        show x        = ""
        next pos _ _  = updatePosChar pos 'x'
        test (d,x)    = if p d x then Just (Beat d x) else Nothing

-- Matches any rhythm
rhythm :: RhythmParser a (Rhythm a)
rhythm = Rhythms <$> Text.Parsec.many1 (rhythm' <|> bound)

rhythmNoBound :: RhythmParser a (Rhythm a)
rhythmNoBound = Rhythms <$> Text.Parsec.many1 rhythm'

rhythm' :: RhythmParser a (Rhythm a)
rhythm' = mzero
    <|> beat
    <|> dotted
    <|> tuplet

-- Matches a beat divisible by 2 (notated)
beat :: RhythmParser a (Rhythm a)
beat = do
    RState tm ts _ <- getState
    (\d -> (d^/tm) `subDur` ts) <$> match (\d _ -> 
        d - ts > 0 
        &&
        isDivisibleBy 2 (d / tm - ts)) -- TODO or is it (d - ts) / tm

-- | Matches a dotted rhythm
dotted :: RhythmParser a (Rhythm a)
dotted = msum . fmap dotted' $ [1..2]               -- max 2 dots

dotted' :: Int -> RhythmParser a (Rhythm a)
dotted' n = do
    modifyState $ modifyTimeMod (* dotMod n)
    a <- beat
    modifyState $ modifyTimeMod (/ dotMod n)
    return (Dotted n a)


-- | Matches a bound rhythm
bound :: RhythmParser a (Rhythm a)
bound = bound' (1/2)


bound' :: Duration -> RhythmParser a (Rhythm a)
bound' d = do
    modifyState $ modifyTimeSub (+ d)
    a <- rhythm'
    modifyState $ modifyTimeSub (subtract d)
    return $ Bound d a

-- | Matches a tuplet
tuplet :: RhythmParser a (Rhythm a)
tuplet = msum . fmap tuplet' $ tupletMods

-- tuplet' 2/3 for triplet, 4/5 for quintuplet etc
tuplet' :: Duration -> RhythmParser a (Rhythm a)
tuplet' d = do
    RState _ _ depth <- getState
    onlyIf (depth < 1) $ do                         -- max 1 nested tuplets
        modifyState $ modifyTimeMod (* d) 
                    . modifyTupleDepth succ
        a <- rhythmNoBound        
        modifyState $ modifyTimeMod (/ d) 
                    . modifyTupleDepth pred
        return (Tuplet d a)



-- |
-- Succeed only if the entire input is consumed.
--
atEnd :: RhythmParser a b -> RhythmParser a b
atEnd p = do
    x <- p
    notFollowedBy' anyToken' <?> "end of input"
    return x
    where
        notFollowedBy' p = try $ (try p >> unexpected "") <|> return ()
        anyToken'        = tokenPrim (const "") (\pos _ _ -> pos) Just
    
-- |
-- Like 'notFollowedBy', but not requiring 'Show'.
--
-- notFollowedBy' :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()Source
-- notFollowedBy' p = try $ (try p >> unexpected "") <|> return ()

-- |
-- Like 'anyToken', but not requiring 'Show'.
--
-- anyToken' :: Stream s m t => ParsecT s u m tSource
-- anyToken'        = tokenPrim (const "") (\pos _ _ -> pos) Just






onlyIf :: MonadPlus m => Bool -> m b -> m b
onlyIf b p = if b then p else mzero

logBaseR :: forall a . (RealFloat a, Floating a) => Rational -> Rational -> a
logBaseR k n 
    | isInfinite (fromRational n :: a)      = logBaseR k (n/k) + 1
logBaseR k n 
    | isDenormalized (fromRational n :: a)  = logBaseR k (n*k) - 1
logBaseR k n                         = logBase (fromRational k) (fromRational n)

-- As it sounds
isDivisibleBy :: Duration -> Duration -> Bool
isDivisibleBy n = (== 0.0) . snd . properFraction . logBaseR (toRational n) . toRational

propFrac :: Duration -> Duration -> (Int, Double)
propFrac n = properFraction . logBaseR (toRational n) . toRational


single x = [x]            

left f (Left x)  = Left (f x)
left f (Right y) = Right y
