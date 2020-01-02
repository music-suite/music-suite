
{-# LANGUAGE FlexibleInstances #-}

import Music.Prelude hiding (
    flutes1,    flutes2,
    oboes1,     oboes2,
    clarinets1, clarinets2,
    bassoons1,  bassoons2,
    trumpets1,  trumpets2,
    trombones1, trombones2
    )
import Data.Foldable (Foldable)
import Data.Tree (Tree(..), unfoldTree, drawTree)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Music.Score
import qualified Data.List
import qualified Data.Ord
import qualified Data.Either
import qualified Data.Maybe
import qualified Control.Comonad
import qualified Debug.Trace
import qualified Data.Text
import qualified Music.Time.Internal.Convert
import Music.Score.Export2.StandardNotation (test3)
import Music.Time.Internal.Util (rotate)
import Util
import System.IO.Unsafe (unsafePerformIO)
-- import Text.ParserCombinators.Parsec.Lisp(LispVal(Atom,List,DottedList,Number,String,Bool))
import Data.AttoLisp(Lisp(..))
{-
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
-}
toLisp2 :: Lisp -> Lisp2
toLisp2 x = case x of
  Symbol xs -> case Data.Text.unpack xs of
    (':':xs)   -> LKeyword xs
    xs         -> LSymbol xs
  String xs       -> LString (Data.Text.unpack xs)
  Number x        -> LNumber (toRational x)
  List xs         -> LList (fmap toLisp2 xs)
  DotList xs x -> LList (fmap toLisp2 $ xs++[x])
  
data Lisp2
  = LSymbol String
  | LKeyword String
  | LString String
  | LNumber Rational
  | LList [Lisp2]
  deriving (Eq, Ord)
instance Show Lisp2 where
  show x = case x of
    LSymbol x -> x
    LKeyword x -> ':':x
    LString x -> show x
    LNumber x -> "("++show x++")"
    LList xs -> "("++ Data.List.intercalate " " (fmap show xs)++")"

toMusic = mcatMaybes . toMusic'
toMusic' :: Lisp2 -> Score (Maybe StandardNote)
toMusic' x = case x of
  LSymbol x -> symbolToNote x
  LList xs -> stretchTo 1 $ scat (fmap toMusic' xs) -- TODO not if 1st elem is kw
  _ -> mempty -- TODO

symbolToNote x = case x of
  "c" -> c
  "-" -> rest
  "d" -> d
  "e" -> e
  "f" -> f
  "g" -> g
  "a" -> a
  "b" -> b

  "cs" -> cs
  "ds" -> ds
  "es" -> es
  "fs" -> fs
  "gs" -> gs
  "as" -> as
  "bs" -> bs

  "cb" -> cb
  "db" -> db
  "eb" -> eb
  "fb" -> fb
  "gb" -> gb
  "ab" -> ab
  "bb" -> bb
-- TODO parser
  "d!" -> d'
  "c!" -> c'

  "c2" -> c |* 2
  "d2" -> d |* 2
  "e2" -> e |* 2
  "f2" -> f |* 2
  "g2" -> g |* 2
  "a2" -> a |* 2
  "b2" -> b |* 2

  "cs2" -> cs |* 2
  "ds2" -> ds |* 2
  "es2" -> es |* 2
  "fs2" -> fs |* 2
  "gs2" -> gs |* 2
  "as2" -> as |* 2
  "bs2" -> bs |* 2

  "cb2" -> cb |* 2
  "db2" -> db |* 2
  "eb2" -> eb |* 2
  "fb2" -> fb |* 2
  "gb2" -> gb |* 2
  "ab2" -> ab |* 2
  "bb2" -> bb |* 2
  
-- Unrelated test of melodic trees
testMel = Branch 1 [mot2, mot2, mot3]
  where
    mot2 = Leaf (-1/4) mot
    mot3 = Leaf (-1/4) (mot|*2)
--     >>> display $ startAt (3/4) $  (renderMel testMel )


{-
Basics: Treat the score as a grid of (100 x numParts) bars.
Every bar is generated from its position (x,y) and a basic 1-bar motive (which may or may not be used).

There may be vertical/horizontal procedures a play, but the basic generation is still barwise (so these functions has
to be called by the main ((Int,Int) -> Music function)).

Use a couple of global behaviors ("density" and the like) to control this.
  Density
  Loudness
  Dissonance
  Transformation of original cell
  Pitch relations (original, transposed, more advanced?)

Possibly using some notion of "tiles" for representing harmonic/registral layers?

-}

data Activity
  = PlayingMotive
  | PlayingEffect
  | PlayingFigure
  | PlayingDrone
  | PlayingScale
  | Tacet
  deriving (Eq, Ord, Show, Enum, Bounded)

fromActivity :: Activity -> Score (Maybe Asp1)
fromActivity = go
  where
    go PlayingMotive = fmap Just $ stretchTo 1 $ mot'
    go PlayingEffect = fmap Just $ stretchTo 1 $ staccato $ times 16 $ d |/16
    go PlayingFigure = fmap Just $ stretchTo 1 $ scat [c,d]
    go PlayingDrone  = fmap Just $ d
    go PlayingScale  = fmap Just $ stretchTo 1 $ legato $ scat $ palindrome [a_..a']
    go Tacet         = rest


-- The motive
mot :: Melody
mot = fromJust $ unsafePerformIO $ beginning 1 <$> getMelody  "mel/simple/fiskeskar"


grid :: [[Score (Maybe Asp1)]]
-- grid = undefined
grid = repeat $ bars
  where
    bars = take 100 $ fmap fromActivity $ cycle $ enumFromTo minBound maxBound

mot' :: Asp
mot' = stretchTo 1 $ inspectableToAsp $ mcatMaybes mot



example = id
  $ title "Variations on a Swedish Folk-Tune" 
  $ composer "Hans Jacob Hoeglund (2015)"
  -- $ over phrases' (cresc pp ff)
  $ pcat $ zipWith (set parts') orchParts (fmap (mcatMaybes . scat) grid)

test a b ps = bm $ op $ (if ps == [] then id else filterParts ps) $ filterEventsInSpan (a<->b) example
  where
    -- op = openLilypond' LyLargeScoreFormat
    -- op = openMusicXml
    op = test3
-- test 0 10



filterParts :: (HasPart' a, PartOf a ~ Part) => [Part] -> Score a -> Score a
filterParts ps = over events (filter (\x -> x^.part `elem` ps))

isStringPart p = isStringInstr $ p^._instrument

-- orchParts = []

-- Orchestra
strings   = filter (isStringInstrument . view _instrument) orchParts
woodwinds = filter (isWoodwindInstrument . view _instrument) orchParts
brass     = filter (isBrassInstrument . view _instrument) orchParts

orch = level mf $ flip doubleParts c $ orchParts
orchParts :: [Part]
orchParts = [flutes1,flutes2,flutes3,oboes1,oboes2,{-"tutti"-}tutti corAnglais,clarinets1,clarinets2,clarinets3,bassoons1,bassoons2,bassoons3]
  <> divide 4 horns <> divide 3 trumpets <> divide 3 trombones <> [tutti tuba]
  <> [timp,hrp,cel]
  <> divide 2 violins <> [violas] <> [cellos] <> [doubleBasses]
[flutes1,flutes2,flutes3] = divide 3 flutes
[oboes1,oboes2,oboes3] = divide 3 oboes
[clarinets1,clarinets2,clarinets3] = divide 3 clarinets
[bassoons1,bassoons2,bassoons3] = divide 3 bassoons
[horns1,horns2,horns3,horns4] = divide 4 horns
[trumpets1,trumpets2,trumpets3] = divide 3 trumpets
[trombones1,trombones2,trombones3] = divide 3 trombones
basses = tutti doubleBass
timp = tutti timpani
[timp1,timp2] = divide 2 timp
cel = tutti celesta
vib = tutti vibraphone
hrp = harp
tub = tutti tuba
-- 
_ = undefined


inspectableToAsp = toAspects . inspectableToMusic
-- instance Inspectable [[Asp]] where
  -- inspectableToMusic = inspectableToMusic . fmap2 inspectableToMusic



