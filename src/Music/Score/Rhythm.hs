
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    ViewPatterns,
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
-------------------------------------------------------------------------------------

module Music.Score.Rhythm (
        -- * Rhythm type
        Rhythm(..),

        -- * Quantization
        quantize,
        rewrite,
        dotMod,
  ) where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Control.Applicative
import Control.Monad (ap, join, MonadPlus(..))
import Control.Arrow hiding (left)
import qualified Data.List as List
import Data.Tree
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

import Music.Time
import Music.Score.Ties
import Music.Score.Voice

{-



-}

data Rhythm a
    = Beat       Duration a                    -- d is divisible by 2
    | Group      [Rhythm a]                    -- 
    | Dotted     Int (Rhythm a)                -- n > 0.
    | Tuplet     Duration (Rhythm a)           -- d is an emelent of 'konstTuplets'.
    deriving (Eq, Show, Functor, Foldable)
    -- RInvTuplet  Duration (Rhythm a)

getBeatValue :: Rhythm a -> a
getBeatValue (Beat d a) = a
getBeatValue _          = error "getBeatValue: Not a beat"

getBeatDuration :: Rhythm a -> Duration
getBeatDuration (Beat d a) = d
getBeatDuration _          = error "getBeatValue: Not a beat"

realize :: Rhythm a -> [(Duration, a)]
realize (Beat d a)      = [(d, a)]
realize (Group rs)      = rs >>= realize
realize (Dotted n r)    = dotMod n `stretch` realize r
realize (Tuplet n r)    = n `stretch` realize r 

-- rhythmToTree :: Rhythm a -> Tree (String, Maybe a)
-- rhythmToTree = go
--     where
--         go (Beat d a)     = Node ("beat "  ++ showD d, Just a) []
--         go (Group rs)     = Node ("group", Nothing) (fmap rhythmToTree rs)
--         go (Dotted n r)   = Node ("dotted " ++ show n, Nothing) [rhythmToTree r]
--         go (Tuplet n r)   = Node ("tuplet " ++ showD n, Nothing) [rhythmToTree r]
--         showD = show . toRational
-- 
-- drawRhythm :: Show a => Rhythm a -> String
-- drawRhythm = drawTree . fmap (uncurry (++) <<< (++ " ") *** show) . rhythmToTree

rhythmToTree :: Rhythm a -> Tree String
rhythmToTree = go
    where
        go (Beat d a)     = Node ("" ++ showD d) []
        go (Group rs)     = Node ("") (fmap rhythmToTree rs)
        go (Dotted n r)   = Node (replicate n '.') [rhythmToTree r]
        go (Tuplet n r)   = Node ("*^ " ++ showD n) [rhythmToTree r]
        showD = (\x -> show (numerator x) ++ "/" ++ show (denominator x)) . toRational

drawRhythm :: Show a => Rhythm a -> String
drawRhythm = drawTree . rhythmToTree

instance Semigroup (Rhythm a) where
    (<>) = mappend

-- Catenates using 'Group'
instance Monoid (Rhythm a) where
    mempty = Group []
    Group as `mappend` Group bs   =  Group (as <> bs)
    r        `mappend` Group bs   =  Group ([r] <> bs)
    Group as `mappend` r          =  Group (as <> [r])
    a        `mappend` b          =  Group [a, b]

instance HasDuration (Rhythm a) where
    duration (Beat d _)        = d
    duration (Dotted n a)      = duration a * dotMod n
    duration (Tuplet c a)      = duration a * c
    duration (Group as)        = sum (fmap duration as)

instance AdditiveGroup (Rhythm a) where
    zeroV   = error "No zeroV for (Rhythm a)"
    (^+^)   = error "No ^+^ for (Rhythm a)"
    negateV = error "No negateV for (Rhythm a)"

instance VectorSpace (Rhythm a) where
    type Scalar (Rhythm a) = Duration
    a *^ Beat d x = Beat (a*d) x
    -- FIXME how does this preserve the invariant?

Beat d x `subDur` d' = Beat (d-d') x


{-
    Rhythm rewrite laws (all up to realization equality)
    
    Note: Just sketching, needs more formal treatment.


    Group [Group xs ...] = Group [xs ...]
        [JoinGroup]

    Tuplet m (Tuplet n x) = Tuplet (m * n) x
        [NestTuplet]

    Tuplet m (Group [a,b ...]) = Group [Tuplet m a, Tuplet m b ...]
        [DistributeTuplet]
        This is only OK in certain contexts! Which?
    
-}

rewrite :: Rhythm a -> Rhythm a

rewrite = rewriteR . rewrite1

rewriteR = go where
    go (Beat d a)     = Beat d a
    go (Group rs)     = Group (fmap (rewriteR . rewrite1) rs)
    go (Dotted n r)   = Dotted n ((rewriteR . rewrite1) r)
    go (Tuplet n r)   = Tuplet n ((rewriteR . rewrite1) r)

rewrite1 = splitTuplet . tupletDot . singleGroup
    

singleGroup :: Rhythm a -> Rhythm a
singleGroup orig@(Group [x]) = x
singleGroup orig             = orig

-- | Removes dotted notes in 2/3 tuplets.
tupletDot :: Rhythm a -> Rhythm a
tupletDot orig@(Tuplet (unratio -> (2,3)) (Dotted 1 x)) = x
tupletDot orig                                          = orig

-- | Splits a tuplet iff it contans a group which can be split into two halves of exactly the same size.
splitTuplet :: Rhythm a -> Rhythm a
splitTuplet orig@(Tuplet n (Group xs)) = case trySplit xs of
    Nothing       -> orig
    Just (as, bs) -> Tuplet n (Group as) <> Tuplet n (Group bs)
splitTuplet orig = orig

-- TODO bad instance
instance HasDuration a => HasDuration [a] where
    duration = sum . fmap duration

trySplit :: [Rhythm a] -> Maybe ([Rhythm a], [Rhythm a])
trySplit = firstJust . fmap g . splits
    where           
        g (part1, part2)
            | duration part1 == duration part2 = Just (part1, part2)
            | otherwise                        = Nothing
        splits xs = List.inits xs `zip` List.tails xs
        firstJust = listToMaybe . fmap fromJust . List.dropWhile isNothing


quantize :: Tiable a => [(Duration, a)] -> Either String (Rhythm a)
quantize = quantize' (atEnd rhythm)

testQuantize :: [Duration] -> IO ()
testQuantize x = case fmap rewrite $ quantize' (atEnd rhythm) $ fmap (\x -> (x,())) $ x of
    Left e -> error e
    Right x -> putStrLn $ drawRhythm x


konstNumDotsAllowed :: [Int]
konstNumDotsAllowed = [1..2]

konstBounds :: [Duration]
konstBounds = [ 1/2, 1/4, 1/8 ]

konstTuplets :: [Duration]
konstTuplets = [ 2/3, 4/5, 4/7, 8/9, 8/11, 8/13, 8/15, 16/17, 16/18, 16/19, 16/21, 16/23 ]

konstMaxTupletNest :: Int
konstMaxTupletNest = 1


data RhythmContext = RhythmContext {

        -- Time scaling of the current note (from dots and tuplets).
        timeMod :: Duration,

        -- Time subtracted from the current rhythm (from ties).
        timeSub :: Duration,

        -- Number of tuplets above the current note (default 0).
        tupleDepth :: Int
    }

instance Monoid RhythmContext where
    mempty = RhythmContext { timeMod = 1, timeSub = 0, tupleDepth = 0 }
    a `mappend` _ = a

modifyTimeMod :: (Duration -> Duration) -> RhythmContext -> RhythmContext
modifyTimeMod f (RhythmContext tm ts td) = RhythmContext (f tm) ts td

modifyTimeSub :: (Duration -> Duration) -> RhythmContext -> RhythmContext
modifyTimeSub f (RhythmContext tm ts td) = RhythmContext tm (f ts) td

modifyTupleDepth :: (Int -> Int) -> RhythmContext -> RhythmContext
modifyTupleDepth f (RhythmContext tm ts td) = RhythmContext tm ts (f td)






-- |
-- A @RhytmParser a b@ converts (Voice a) to b.
type RhythmParser a b = Parsec [(Duration, a)] RhythmContext b

quantize' :: Tiable a => RhythmParser a b -> [(Duration, a)] -> Either String b
quantize' p = left show . runParser p mempty ""






rhythm :: Tiable a => RhythmParser a (Rhythm a)
rhythm = Group <$> many1 (rhythm' <|> bound)

rhythmNoBound :: Tiable a => RhythmParser a (Rhythm a)
rhythmNoBound = Group <$> many1 rhythm'

rhythm' :: Tiable a => RhythmParser a (Rhythm a)
rhythm' = mzero
    <|> beat
    <|> dotted
    <|> tuplet

-- Matches a beat divisible by 2 (notated)
-- beat :: Tiable a => RhythmParser a (Rhythm a)
-- beat = do
--     RhythmContext tm ts _ <- getState
--     (\d -> (d^/tm) `subDur` ts) <$> match (\d _ ->
--         d - ts > 0  &&  isDivisibleBy 2 (d / tm - ts))

beat :: Tiable a => RhythmParser a (Rhythm a)
beat = do
    RhythmContext tm ts _ <- getState
    match' $ \d x ->
        let d2 = d / tm - ts
        in (d2, x) `assuming` (d - ts > 0 && isDivisibleBy 2 d2)


-- | Matches a dotted rhythm
dotted :: Tiable a => RhythmParser a (Rhythm a)
dotted = msum . fmap dotted' $ konstNumDotsAllowed

-- | Matches a bound rhythm
bound :: Tiable a => RhythmParser a (Rhythm a)
bound = msum . fmap bound' $ konstBounds

-- | Matches a tuplet
tuplet :: Tiable a => RhythmParser a (Rhythm a)
tuplet = msum . fmap tuplet' $ konstTuplets





dotted' :: Tiable a => Int -> RhythmParser a (Rhythm a)
dotted' n = do
    modifyState $ modifyTimeMod (* dotMod n)
    a <- beat
    modifyState $ modifyTimeMod (/ dotMod n)
    return (Dotted n a)

-- | Return the scaling applied to a note with the given number of dots (i.e. 3/2, 7/4 etc).
dotMod :: Int -> Duration
dotMod n = dotMods !! (n-1)

-- [3/2, 7/4, 15/8, 31/16 ..]
dotMods :: [Duration]
dotMods = zipWith (/) (fmap pred $ drop 2 times2) (drop 1 times2)
    where
        times2 = iterate (*2) 1

bound' :: Tiable a => Duration -> RhythmParser a (Rhythm a)
bound' d = do
    modifyState $ modifyTimeSub (+ d)
    a <- beat
    modifyState $ modifyTimeSub (subtract d)
    let (b,c) = toTied $ getBeatValue a

    -- FIXME doesn't know order
    return $ Group [Beat (getBeatDuration a) b, Beat d c]

-- tuplet' 2/3 for triplet, 4/5 for quintuplet etc
tuplet' :: Tiable a => Duration -> RhythmParser a (Rhythm a)
tuplet' d = do
    RhythmContext _ _ depth <- getState
    onlyIf (depth < konstMaxTupletNest) $ do
        modifyState $ modifyTimeMod (* d)
                    . modifyTupleDepth succ
        a <- rhythmNoBound
        modifyState $ modifyTimeMod (/ d)
                    . modifyTupleDepth pred
        return (Tuplet d a)


-------------------------------------------------------------------------------------

-- | Similar to 'many1', but tries longer sequences before trying one.
-- many1long :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
-- many1long p = try (many2 p) <|> fmap return p

-- | Similar to 'many1', but applies the parser 2 or more times.
-- many2 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
-- many2 p = do { x <- p; xs <- many1 p; return (x : xs) }

-- Matches a (duration, value) pair iff the predicate matches, returns beat
match :: Tiable a => (Duration -> a -> Bool) -> RhythmParser a (Rhythm a)
match p = tokenPrim show next test
    where
        show x        = ""
        next pos _ _  = updatePosChar pos 'x'
        test (d,x)    = if p d x then Just (Beat d x) else Nothing

-- Matches a (duration, value) pair iff the predicate matches, returns beat
match' :: Tiable a => (Duration -> a -> Maybe (Duration, b)) -> RhythmParser a (Rhythm b)
match' f = tokenPrim show next test
    where
        show x        = ""
        next pos _ _  = updatePosChar pos 'x'
        test (d,x)    = case f d x of
            Nothing     -> Nothing
            Just (d,x)  -> Just $ Beat d x

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

onlyIf :: MonadPlus m => Bool -> m b -> m b
onlyIf b p = if b then p else mzero

-- | Just x or Nothing
assuming :: a -> Bool -> Maybe a
assuming x b = if b then Just x else Nothing

logBaseR :: forall a . (RealFloat a, Floating a) => Rational -> Rational -> a
logBaseR k n
    | isInfinite (fromRational n :: a)      = logBaseR k (n/k) + 1
logBaseR k n
    | isDenormalized (fromRational n :: a)  = logBaseR k (n*k) - 1
logBaseR k n                         = logBase (fromRational k) (fromRational n)


divides     = isDivisibleBy
divisibleBy = flip isDivisibleBy

-- As it sounds, do NOT use infix
-- Only works for simple n such as 2 or 3, TODO determine
isDivisibleBy :: Duration -> Duration -> Bool
isDivisibleBy n = (== 0.0) . snd . properFraction . logBaseR (toRational n) . toRational


left f (Left x)  = Left (f x)
left f (Right y) = Right y

unratio x = (numerator $ realToFrac x, denominator $ realToFrac x)