
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Rhythmical quantization.
--
-------------------------------------------------------------------------------------

module Music.Score.Internal.Quantize (
        -- * Rhythm type
        Rhythm(..),
        mapWithDur,

        -- * Quantization
        quantize,
        rewrite,
        dotMod,

        -- * Utility
        drawRhythm,
        testQuantize,
  ) where

import           Prelude             hiding (concat, concatMap, foldl, foldr,
                                      mapM, maximum, minimum, sum)

import           Control.Applicative
import           Control.Lens        (over, (^.), _Left)
import           Control.Monad       (MonadPlus (..), ap, join)
import           Data.Either
import           Data.Foldable
import           Data.Traversable (Traversable(..))
import           Data.Function       (on)
import qualified Data.List           as List
import           Data.Maybe
import           Data.Ord            (comparing)
import           Data.Ratio
import           Data.Semigroup
import           Data.Traversable
import           Data.Tree
import           Data.VectorSpace

import           Text.Parsec         hiding ((<|>))
import           Text.Parsec.Pos

import           Music.Score.Ties
import           Music.Score.Internal.Util
import           Music.Time

data Rhythm a
  = Beat       Duration a                    -- d is divisible by 2
  | Group      [Rhythm a]                    --
  | Dotted     Int (Rhythm a)                -- n > 0.
                                             -- Can be thought of as a special case of tuplet, where
                                             --  Dotted n x = Tuplet (dotMod n) x
  | Tuplet     Duration (Rhythm a)           -- d is an emelent of 'konstTuplets'.
                                             -- d = (sounding dur/notated dur)
                                             --   Of course this implies (notated * d = sounding)
                                             --   This is the reciprocal of the ratio usually written in a score
                                             -- I.e. for standard triplet, sounding/notated = (1/3)/(1/2) = 2/3
  deriving (Eq, Show, Functor, Foldable, Traversable)
  -- RInvTuplet  Duration (Rhythm a)

instance Transformable (Rhythm a) where
  transform s (Beat d x) = Beat (transform s d) x
  transform s (Group rs) = Group (fmap (transform s) rs)
  transform s (Dotted n r) = Dotted n (transform s r)
  transform s (Tuplet n r) = Tuplet n (transform s r)

getBeatValue :: Rhythm a -> a
getBeatValue (Beat d a) = a
getBeatValue _          = error "getBeatValue: Not a beat"

getBeatDuration :: Rhythm a -> Duration
getBeatDuration (Beat d a) = d
getBeatDuration _          = error "getBeatValue: Not a beat"

-- TODO return voice
realize :: Rhythm a -> [Note a]
realize (Beat d a)      = [(d, a)^.note]
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

mapWithDur :: (Duration -> a -> b) -> Rhythm a -> Rhythm b
mapWithDur f = go
  where
    go (Beat d x)            = Beat d (f d x)
    go (Dotted n (Beat d x)) = Dotted n $ Beat d (f (dotMod n * d) x)
    go (Group rs)            = Group $ fmap (mapWithDur f) rs
    go (Tuplet m r)          = Tuplet m (mapWithDur f r)

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
  _duration (Beat d _)        = d
  _duration (Dotted n a)      = a^.duration * dotMod n
  _duration (Tuplet c a)      = a^.duration * c
  _duration (Group as)        = sum (fmap (^.duration) as)

instance AdditiveGroup (Rhythm a) where
  zeroV   = error "No zeroV for (Rhythm a)"
  (^+^)   = error "No ^+^ for (Rhythm a)"
  negateV = error "No negateV for (Rhythm a)"

instance VectorSpace (Rhythm a) where
  type Scalar (Rhythm a) = Duration
  a *^ Beat d x = Beat (a*d) x
  -- TODO how does this preserve the invariant?

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

{-
  TODO Many (which?) rewrite rules can be replaced with parsing constraints
-}

rewrite :: Rhythm a -> Rhythm a
rewrite = rewriteR . rewrite1

rewriteR = go where
  go (Beat d a)     = Beat d a
  go (Group rs)     = Group (fmap (rewriteR . rewrite1) rs)
  go (Dotted n r)   = Dotted n ((rewriteR . rewrite1) r)
  go (Tuplet n r)   = Tuplet n ((rewriteR . rewrite1) r)

rewrite1 = tupletDot . {-splitTupletIfLongEnough .-} singleGroup

-- | Removes single-note groups
singleGroup :: Rhythm a -> Rhythm a
singleGroup orig@(Group [x]) = x
singleGroup orig             = orig

-- | Removes dotted notes in 2/3 tuplets.
tupletDot :: Rhythm a -> Rhythm a
tupletDot orig@(Tuplet ((unRatio.realToFrac) -> (2,3)) (Dotted 1 x)) = x
tupletDot orig                                                       = orig

splitTupletIfLongEnough :: Rhythm a -> Rhythm a
splitTupletIfLongEnough r = if r^.duration > (1/2) then splitTuplet r else r
-- TODO should compare against beat duration, not just (1/4)

-- | Splits a tuplet iff it contans a group which can be split into two halves whose
--   duration have the ratio 1/2, 1 or 1/2.
splitTuplet :: Rhythm a -> Rhythm a
splitTuplet orig@(Tuplet n (Group xs)) = case trySplit xs of
  Nothing       -> orig
  Just (as, bs) -> Tuplet n (Group as) <> Tuplet n (Group bs)
splitTuplet orig = orig

trySplit :: [Rhythm a] -> Maybe ([Rhythm a], [Rhythm a])
trySplit = firstJust . fmap g . splits
  where
    g (part1, part2)
      | (sum . fmap (^.duration)) part1 `rel` (sum . fmap (^.duration)) part2 = Just (part1, part2)
      | otherwise = Nothing
    rel x y
      | x   == y   = True
      | x   == y*2 = True
      | x*2 == y   = True
      | otherwise  = False

-- |
-- Given a list, return a list of all possible splits.
--
-- >>> splits [1,2,3]
-- [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
--
splits :: [a] -> [([a],[a])]
splits xs = List.inits xs `zip` List.tails xs


-- | Return the first @Just@ value, if any.
firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . fmap fromJust . List.dropWhile isNothing

{-
For now, try both strategies (first original, then quSimp).
-}
quantize :: Tiable a => [(Duration, a)] -> Either String (Rhythm a)
quantize xs = case quOrig xs of
  Right x -> Right x
  Left e1 -> case quSimp xs of
    Right x -> Right x
    Left e2 -> Left $ e1 ++ ", " ++ e2

testQuantize :: [Duration] -> IO ()
testQuantize x = case fmap rewrite $ qAlg $ fmap (\x -> (x,())) $ x of
  Left e -> error e
  Right x -> putStrLn $ drawRhythm x
  where
    -- qAlg = quOrig
    -- qAlg = quSimp
    qAlg = quantize

quOrig :: Tiable a => [(Duration, a)] -> Either String (Rhythm a)
quOrig = quantize' (atEnd rhythm)

konstNumDotsAllowed :: [Int]
konstNumDotsAllowed = [1..2]

konstBounds :: [Duration]
konstBounds = [ 1/2, 1/4, 1/8, 1/16 ]

konstTuplets :: [Duration]
konstTuplets = [ 2/3, 4/5, 4/7, 8/9, 8/11, 8/13, 8/15, 16/17, 16/18, 16/19, 16/21, 16/23 ]

konstMaxTupletNest :: Int
konstMaxTupletNest = 1


data RhythmContext = RhythmContext {

      -- Time scaling of the current note (from dots and tuplets).
      timeMod    :: Duration,

      -- Time subtracted from the current rhythm (from ties).
      timeSub    :: Duration,

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
quantize' p rh = over _Left show . runParser p mempty ("Rhythm pattern: '" ++ show (fmap fst rh) ++ "'") $ rh






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
--         d - ts > 0  &&  isPowerOf 2 (d / tm - ts))

beat :: Tiable a => RhythmParser a (Rhythm a)
beat = do
  RhythmContext tm ts _ <- getState
  match' $ \d x ->
      let d2 = d / tm - ts
      in (d2, x) `assuming` (d - ts > 0 && isPowerOf2 d2)


-- | Matches a dotted rhythm
dotted :: Tiable a => RhythmParser a (Rhythm a)
dotted = msum . fmap dotted' $ konstNumDotsAllowed

-- | Matches a bound rhythm
-- TODO this should be abolished, see below!
bound :: Tiable a => RhythmParser a (Rhythm a)
bound = msum $ fmap bound' $ konstBounds
-- bound = msum $ fmap bound' $ (konstBounds <> fmap (*(3/2)) konstBounds)

{-
What this should really do is to split the rhythm into two rhythms where the first have the bound duration...
-}

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

  -- TODO doesn't know order
  -- Need to have relative phase in the parser context!
  return $ Group [Beat (getBeatDuration a) b,
    Beat d c
    -- if isPowerOf2 d then Beat d c else if isPowerOf2 (d*(2/3)) then Dotted 1 (Beat (d*(2/3)) c) else (error "Bad bound rhythm")
      ]

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


{-
isDivisibleBy2 :: RealFrac a => a -> Bool
isDivisibleBy2 x = isInt x && even (round x)

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)
-}

logBaseR :: forall a . (RealFloat a, Floating a) => Rational -> Rational -> a
logBaseR k n | isInfinite (fromRational n :: a)      = logBaseR k (n/k) + 1
logBaseR k n | isDenormalized (fromRational n :: a)  = logBaseR k (n*k) - 1
logBaseR k n | otherwise                             = logBase (fromRational k) (fromRational n)


-- divides     = isDivisibleBy
-- divisibleBy = flip isDivisibleBy

-- As it sounds, do NOT use infix
-- Only works for simple n such as 2 or 3, TODO determine
-- isPowerOf :: Duration -> Duration -> Bool
isPowerOf n = (== 0.0) . snd . properFraction . logBaseR (toRational n) . toRational

-- isPowerOf2 :: Duration -> Bool
isPowerOf2 = isPowerOf 2

greatestSmallerPowerOf2 :: Integer -> Integer
greatestSmallerPowerOf2 x
  | x < 0        = error "greatestSmallerPowerOf2: Must be > 0"
  | isPowerOf2 x = x
  | otherwise    = greatestSmallerPowerOf2 (x - 1)


{-
An "emergency" quantizer that ignores input durations and outputs a single beat sequence
with all notes in the same length.
-}
quSimp :: Tiable a => [(Duration, a)] -> Either String (Rhythm a)
quSimp = Right . qu1 1
-- quSimp xs = if isPowerOf2 totDur then (Right $ qu1 totDur xs)
--   else Left ("This strategy only works for total duration that are powers of two (i.e. 4/4, 2/4), given"++show totDur)
  where
--     totDur = sum $ fmap fst xs
    qu1 totDur xs = if isPowerOf2 n then
                  Group (fmap (\(_,a) -> Beat (totDur/fromIntegral n) a) xs) else
      Tuplet (q) (Group (fmap (\(_,a) -> Beat (totDur/fromIntegral p) a) xs))
      where
        q = fromIntegral p / fromIntegral n :: Duration
        p = greatestSmallerPowerOf2 n :: Integer
        n = fromIntegral $ length xs
    -- (1/n) = q(1/p)
    -- (1/n) = (p/n)(1/p)
    -- (1/n) = (p/np)
    -- (1/n) = (1/n)
