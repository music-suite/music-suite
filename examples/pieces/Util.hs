
{-# LANGUAGE TypeFamilies, OverloadedStrings, ConstraintKinds, FlexibleContexts, FlexibleInstances, TypeFamilies, DeriveFunctor,
  GeneralizedNewtypeDeriving, ViewPatterns, MultiParamTypeClasses, RankNTypes, ConstraintKinds, StandaloneDeriving,
  DeriveTraversable, DeriveDataTypeable, TupleSections #-}

module Util
where

import Control.Comonad (Comonad, extract, duplicate)
import Data.Set (Set)
import Data.Monoid.Average
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first, second)
import Data.Traversable (sequenceA)
import Data.Functor.Adjunction (unzipR)
import Numeric.Natural (Natural)
import Data.Map (Map)
-- import qualified Database.Stash
import qualified Data.Char
import qualified Data.Text
import qualified Data.Monoid.Average
import qualified Data.Ratio
import qualified Data.List
import qualified Data.Ord
import qualified Data.Either
import qualified Data.Ratio
import qualified Data.Map
import           Data.Map(Map(..))
import qualified Control.Monad.State
import           Control.Monad.State (StateT(..), State)
import           Control.Monad.List (ListT(..), runListT)
import           Control.Monad.Reader (ReaderT(..), runReaderT)
import qualified Data.Maybe
import qualified System.Process
import qualified System.Random
import qualified Debug.Trace
import qualified Numeric.Natural
import qualified System.Process
import qualified System.IO.Unsafe
import qualified Codec.Midi as Midi
import qualified Data.Map as Map
import qualified Data.Aeson
-- import qualified Text.Regex.Posix
import Data.Typeable
import Data.Aeson (ToJSON(..), FromJSON(..))


import Control.Lens (Iso', Iso, Getter)
import Music.Prelude hiding (chord)
import Music.Parts.Division
import Music.Pitch.Common.Types (ChromaticSteps, DiatonicSteps) -- TODO export
import Music.Score.Internal.Util (rotated)
import qualified Music.Score.Import.Sibelius
import qualified Music.Score
import qualified Music.Time.Internal.Convert
-- import qualified Diagrams.Prelude as D
-- import qualified Diagrams.Backend.SVG as DS
-- import qualified Text.Blaze.Svg.Renderer.Utf8 as SVG
import qualified Data.ByteString.Lazy as ByteString


type Melody = Voice Pitch


-- openDiagram :: (
--   Transformable a,
--   HasPart' a, PartOf a ~ Part,
--   HasPitch' a, Music.Score.Pitch a ~ Pitch
--   )
--   => Score a -> IO ()
-- openDiagram x = do
--   writeDiagram x
--   System.Process.system "open -a 'Google Chrome' test.svg"
--   return ()
--
-- -- Other hack to work with StandardNote
-- openDiagram' :: (
--   Transformable a,
--   HasPart' a, PartOf a ~ Part,
--   HasPitches' a, Music.Score.Pitch a ~ Pitch
--   )
--   => Score a -> IO ()
-- openDiagram' x = openDiagram$ fmap(\x -> PartT (x^.part,x^?!pitches))$ x
--
-- writeDiagram :: (
--   Transformable a,
--   HasPart' a, PartOf a ~ Part,
--   HasPitch' a, Music.Score.Pitch a ~ Pitch
--   )
--   => Score a -> IO ()
-- writeDiagram x = do
--   writeRawDiagram "test.svg" $ toDiagram x
--   return ()
--
-- toDiagram :: (
--   Transformable a,
--   HasPart' a, PartOf a ~ Part,
--   HasPitch' a, Music.Score.Pitch a ~ Pitch
--   -- TODO use dynamics, parts, articulation
--   )
--   => Score a -> D.Diagram DS.SVG
-- toDiagram s = D.scaleY 10 $ addOctaveLines $ mconcat $ reverse $ flip map es $ \ev ->
--     move2 ev $
--     move ev $
--     sca ev $ 
--     D.alignL $
--     transpLC $ addColor ev $
--     D.square 1
--   where
--     addColor ev
--       | isStringInstr   (ev^.eventee.part._instrument) = redFC
--       | isBrassInstr    (ev^.eventee.part._instrument) = greenFC
--       | isWoodwindInstr (ev^.eventee.part._instrument) = blueFC
--       | otherwise                                      = brownFC
--
--     addOctaveLines x = (x <> manyLines 0 (oneLine x) <> manyLines 6 (oneLine' x))
--     oneLine  x = (transpLC $ blueFC $ D.alignL $ D.rect (D.width x) 0.1)
--     oneLine' x = (transpLC $ greenFC $ D.alignL $ D.rect (D.width x) 0.1)
--
--     brownFC  = D.fillColor (D.brown :: D.Colour Double)
--     greenFC  = D.fillColor (D.green :: D.Colour Double)
--     blueFC   = D.fillColor (D.blue :: D.Colour Double)
--     redFC    = D.fillColor (D.red :: (D.Colour Double))
--     transpLC = D.lineColor (D.transparent :: (D.AlphaColour Double))
--     manyLines m x = mconcat $ fmap (\n -> D.translateY (n*12+m) x) [-3..3]
--     move2 ev = D.translateY (realToFrac $ semitones $ (ev^.eventee.pitch') .-. (asPitch c))
--     move ev = D.translateX (realToFrac $ ev^.onset)
--     sca ev = D.scaleX (realToFrac $ ev^.duration)
--     es = s2^..events.each
--     s2 = stretch 8 s -- Arbitrarily make an 1/8 note have the same size as a semitone
-- writeRawDiagram path dia = DS.renderSVG path (D.mkHeight 200) dia
--


-- isBrassInstr x = case toMusicXmlSoundId x of
--   Nothing -> False
--   Just i  -> Data.List.isPrefixOf "brass" i
-- isStringInstr x = case toMusicXmlSoundId x of
--   Nothing -> False
--   Just i  -> Data.List.isPrefixOf "strings" i
-- isWoodwindInstr x = case toMusicXmlSoundId x of
--   Nothing -> False
--   Just i  -> Data.List.isPrefixOf "wind" i



type Weight = Double -- 0 <= x <= 1

-- | A very simple random weight function.
weightRands
  :: [(Weight, a)]    -- ^ Weighted mvalues
  -> [Double]         -- ^ Random supply (0 <= x <= 1)
  -> [a]
  -- -> [(Weight, a)]
weightRands weights supply = fmap (snd . head) $ res
  where
    totalWeight       = sum (fmap fst weights)
    normalizedWeights = fmap (first (/ totalWeight)) weights
    offsetWeights     = snd $ Data.List.mapAccumL f 0 normalizedWeights
    f prevOffset (normWeight, x) = (prevOffset+normWeight, (prevOffset+normWeight, x))

    res = fmap (\dice -> dropWhile (\(offsetWeight,_) -> offsetWeight < dice) offsetWeights) supply

-----



----

-- crescendo
mapWithIndex :: (Int -> b -> c) -> [b] -> [c]
mapWithIndex f = zipWith f [0..]




-- Simulate pedal throughout in piano part
-- applyPedal :: Score StandardNote -> Score StandardNote
applyPedal :: (HasPart' a, PartOf a ~ Part) => Score a -> Score a
applyPedal = over (events.each.filtered (\x->x^.part == mempty)) (stretchRelativeOnset 100)


-- scoreToVoice :: Transformable a => Score a -> Maybe [Either Duration (Voice a)]
-- scoreToVoice s =
--   if hasOverlappingEvents s then Nothing
--     else Just (scoreToVoice2 s)
--   where
--     scoreToVoice2 :: Transformable a => Score a -> [Either Duration (Voice a)]
--     scoreToVoice2 = view mVoicePVoice . Music.Time.Internal.Convert.scoreToVoice

-- openBook = openLilypond' LyScoreFormat . asScore

openAudacity :: Score StandardNote -> IO ()
openAudacity x = do
    void $ writeMidi "test.mid" $ x
    void $ System.Process.system "timidity -Ow test.mid 2>/dev/null >/dev/null"
    void $ System.Process.system "open -a Audacity test.wav"

pseqPad :: Duration -> [Score a] -> Score a
pseqPad t xs = removeRests $ pseq $ map (\x -> fmap Just x |> stretch t rest) xs

pseqPaused :: [Duration] -> [Score a] -> Score a
pseqPaused ts xs = removeRests $ pseq $ zipWith (|>) (fmap (`stretch` rest) ts) (map (fmap Just) xs)


{-
Generate 100 new numbers to _rands.hs
-}
newRands = fmap show (mapM (const $ System.Random.randomIO) [1..200] :: IO [Double]) >>= writeFile "_rands.hs"

-- Doubles in (0 < x < 1)
-- CAREFUL repeats rather quickly
rands = cycle [7.429257318751614e-2,0.2912059078902136,0.31316463977497133,0.20826641302553717,0.9252094596705723,0.5549124288611756,0.9459849915609237,0.2539982820601603,0.8193232924218949,0.7662724751120842,0.4599122669271656,0.3907047410083274,0.5853175935378737,0.9146050127586565,0.8207504452236798,0.36722274208620387,0.17174466481871775,0.6345812736975377,0.1781840788432929,2.2964727413095076e-3,0.36980527335856606,0.6926664558433664,0.19074319803533468,5.0637055843367085e-2,0.6998123042913051,0.19441792139541858,0.17501322559912125,0.7747553835066961,0.7223025502427426,0.5405834509795854,0.3012583155102816,0.96117512132372,0.23442457134378847,0.5441792677875034,0.12892485538545817,0.14715625510331498,0.3917895927601821,0.3188506503016785,0.19465030205841105,0.925972977719653,0.7365806906896263,0.35974614942184024,0.16677233081944642,0.20375549696064932,0.5083925641653336,0.2609166117739342,0.86022096597922,0.33861470238404656,0.5502777269718722,0.889235544017378,0.6192809742452946,0.15999096482447772,0.6951403956336047,0.20751491355173346,0.155827092255028,4.9524380192758066e-2,0.6569146760869068,0.31629168218832326,0.7768175928893872,0.6072666119093576,0.7798397242602304,0.587196771856074,0.4944814778402107,0.11339655339344068,0.4344921882652599,0.5890631055003754,6.902428418997575e-2,0.9028181830228169,0.5251483954386853,7.637096319366798e-2,2.208206575913485e-2,0.9874986048405298,0.24554111651687283,0.788529131285573,0.9586959460272685,0.2835197340167279,0.22368697664440051,1.3808239165837954e-2,0.92804380334872,0.8991598134103386,0.4601663876720137,0.3380183056213647,0.9160893452479851,5.087015561015551e-2,0.6146588191005711,0.20435285724643404,0.39067391443070465,0.3018070032861324,7.20230777280163e-2,0.44442392578468903,0.6627283687478381,0.1349687075146485,0.8572690692269058,0.6901211700608283,0.5870546068441326,0.7618199810169137,0.11054218108040792,0.7202179857503165,0.4851205236565409,0.33865326166614496,0.44252894128447606,0.2981660375118682,0.12304603358274102,0.9723347883834453,0.32934721777660136,0.9655082674733565,0.7159432547700425,0.13469674088984707,0.327998596581796,0.22232138773877386,0.1529627302617378,0.2595354151088708,0.8918741759873379,0.5874563438650867,0.4111224413709862,0.12953581023416283,9.972169155295718e-2,0.5985090981476547,0.3745528360091088,0.9041782861602056,0.3797743365922063,0.6088536963716557,0.6467188524655093,0.1694348026517104,0.42162830616184976,7.390587752884803e-2,0.43922809245180683,0.9852679692902845,0.21243657675645078,0.1263045213030769,0.9035994653838951,0.8107990341000378,0.48036268304228713,0.2050444604903917,9.872781889509485e-2,0.3046361844269062,0.5320425880817639,0.2120554864489842,0.7786683785420414,0.4623125989885445,0.4016694960206635,8.076134539937829e-2,0.6634982395409994,0.973444365088678,7.889841611542592e-2,0.33838227603391924,0.9772571216273699,0.9518179123312442,0.5762269847162659,0.2214678055625472,3.6513639430700406e-2,0.8046926041612403,0.6831026792434226,0.9694283676285113,0.8976488890080049,0.22966999840929636,0.6120451525100346,0.7710526455673758,6.360326837104602e-2,0.4583933945481846,0.19584261975013784,0.28369743337551645,2.109268474175252e-4,0.10003518366844599,0.28196737420826457,0.40326576685247595,0.6962118581985136,8.68063205289007e-2,0.432513253581985,0.4500688887334541,0.9158064405362926,0.9255192335428079,0.527412423802402,0.682542158991272,0.2927410266549708,0.3090004396249467,0.8860179900312615,0.8064212225829261,0.4936523693374626,0.13089072026882165,0.9155837586628148,0.3976721685961464,0.3936454773155089,0.16132255677964236,0.63543775220042,0.8616304614749221,0.4001080997466532,6.0934911654278445e-2,0.7707254331377812,0.9330000804824173,0.6963242352075858,0.9317808816173841,0.1148351548972667,0.4440004721000297,0.5737132022206706,3.4776036927188336e-2,9.304020505086674e-2,0.6696153826910349,0.9482111963793334,0.6834996823597761]

(<<>) x y = rcat [y,x]
sppar = pseq . fmap ppar
ppseq = ppar . fmap pseq








-- sustainAtIndex :: Voice a -> Int -> a
-- sustainAtIndex v n = extract $ (v^.notes) !! n

consV :: Note a -> Voice a -> Voice a
consV n v = (n,v)^.re _Cons

takeV :: Int -> Voice a -> Voice a
takeV n = over notes (take n)
dropV n = over notes (drop n)






-- trimB :: Monoid a => Span -> Behavior a -> Behavior a
-- trimB (view onsetAndOffset -> (on,off)) x = (\t -> if on <= t && t <= off then x ! t else mempty)^.behavior
--
-- -- Treat each event as a behavior in the onsetAndOffset (0<->1) and compose.
-- concatB :: Monoid a => Score (Behavior a) -> Behavior a
-- concatB = mconcat . toListOf traverse . mapWithSpan transform . fmap (trimB mempty)


-- fromPitch'' :: IsPitch a => Music.Prelude.Pitch -> a
-- fromPitch'' x = let i = x .-. c in
--   fromPitch $ PitchL ((fromIntegral $ i^._steps) `mod` 7, Just (fromIntegral (i^._alteration)), fromIntegral $ octaves i)


-- captureSibelius = do
--   s <- fmap asScore $ captureSibelius'
--   -- Ignore all but pitches for now as it doesn't print correctly yet
--   return $ fmap ((^?!pitches)) $ s
--
-- captureSibelius' :: IO (Score StandardNote)
-- captureSibelius' = Music.Score.Import.Sibelius.readSibelius "/Users/hans/Desktop/test.sib_json"
--
-- captureSibeliusVoice :: IO Melody
-- captureSibeliusVoice = fmap2 (^?pitches) $ captureSibeliusVoice'
--
-- captureSibeliusVoice' :: IO (Voice StandardNote)
-- captureSibeliusVoice' = do
--   s <- captureSibelius'
--   return $ (s::Score StandardNote)^?!phrases
--
-- captureSibeliusRhythm :: IO Rhythm
-- captureSibeliusRhythm = fmap2 (const ()) $ captureSibeliusVoice
--
-- -- TODO capture whole sequence
-- captureSibeliusChord :: IO [Pitch]
-- captureSibeliusChord = fmap (reverse . (^..pitches)) $ captureSibelius'
--
-- captureSibeliusChords = fmap (
--   fmap Data.List.sort . fmap (view (from event._2)) . (^.events) . simultaneous . fmap (:[])) $ captureSibelius
--
-- interactSibelius :: (Score Pitch -> Score Pitch) -> IO ()
-- interactSibelius f = captureSibelius >>= (openMusicXml . inspectableToMusic . f)
--
-- interactSibelius' :: (Music -> Music) -> IO ()
-- interactSibelius' f = captureSibelius' >>= (openMusicXml . f)
--
--


-- TODO can't fix with rendered as we can not update the initial...
applyB :: Reactive a -> b -> Behavior (a -> b) -> Reactive b
applyB r z b = mapWithTimeR (\t x -> case t of { Nothing -> z ; Just t -> (b ! t) x}) r

undrenderR :: (a, [(Time, a)]) -> Reactive a
undrenderR (init, updates) = foldl (\a (t,b) -> switchR t a (pure b)) (pure init) updates

renderR x = (initial x, updates x)

rendered :: Iso (Reactive a) (Reactive b) (a, [(Time, a)]) (b, [(Time, b)])
rendered = iso renderR undrenderR

-- Time a value starts (if applicable)
mapWithTimeR :: (Maybe Time -> a -> b) -> Reactive a -> Reactive b
mapWithTimeR f' = let f = uncurry f' in over rendered (bimap (\x -> f (Nothing,x)) (over (mapped) (\(t,a) -> (t, f (Just t,a)))))


widenAmbitus :: (Ord a, Num a) => a -> Ambitus v a -> Ambitus v a
widenAmbitus p = under ambitus (\(m,n) -> (m `min` p, n `max` p))




-- TODO these should be the show instances...
showDynamicT :: Show a => DynamicT Dynamics a -> String
showDynamicT (DynamicT (l,x))
  | l == mempty = show x
  | otherwise   = "(level " ++ showDynamics l ++ " " ++ show x ++ ")"
-- TODO prec

showDynamics :: Dynamics -> String
showDynamics x
  | x == mempty = "mempty"
  | x >= ffffff = "ffffff"
  | x >= fffff  = "fffff"
  | x >= ffff   = "ffff"
  | x >= fff    = "fff"
  | x >= ff     = "ff"
  | x >= _f     = "_f"
  | x >= mf     = "mf"
  | x >= mp     = "mp"
  | x >= _p     = "_p"
  | x >= pp     = "pp"
  | x >= ppp    = "ppp"
  | x >= pppp   = "pppp"
  | x >= ppppp  = "ppppp"
  | otherwise   = "pppppp"

roundDuration, ceilingDuration, floorDuration :: Duration -> Duration
roundDuration = fromInteger . round
ceilingDuration = fromInteger . ceiling
floorDuration = fromInteger . floor

newtype Floater a = Floater { getFloater :: [Aligned (Note a)] }
  deriving (Eq, Ord, Show, Functor, Semigroup, Monoid, Transformable, Alignable)
-- Delay-invariant Transformable


instance IsPitch a => IsPitch (Floater a) where
  fromPitch x = Floater [aligned 0 0 (fromPitch x)]

instance HasDuration (Floater a) where
  _duration x = x^.offset .-. x^.onset

instance HasPosition (Floater a) where
  -- TODO problematic with (mempty^.era), same problem as score
  _era (Floater ns) = foldr largestSpan (0<->0) $ toListOf era ns
-- Do nothing when splitting Floaters (as they are localized)
instance Splittable (Floater a) where
  split _ x = (x,mempty)

-- TODO move
instance Inspectable (Floater Pitch) where
  inspectableToMusic = inspectableToMusic . asScore . renderFloater . fmap fromPitch
instance Inspectable (Floater StandardNote) where
  inspectableToMusic = inspectableToMusic  . asScore . renderFloater

type instance Music.Score.Pitch (Floater a) = PitchOf a
type instance SetPitch b (Floater a) = Floater (SetPitch b a)

instance HasPitches a b => HasPitches (Floater a) (Floater b) where
  pitches = traverse . pitches

type instance Music.Score.Part (Floater a) = PartOf a
type instance SetPart b (Floater a) = Floater (SetPart b a)

instance HasParts a b => HasParts (Floater a) (Floater b) where
  parts = _Wrapped.parts

type instance Music.Score.Dynamic (Floater a) = DynamicOf a
type instance SetDynamic b (Floater a) = Floater (SetDynamic b a)

instance HasDynamics a b => HasDynamics (Floater a) (Floater b) where
  dynamics = _Wrapped.dynamics
instance ToJSON a => ToJSON (Floater a) where
  toJSON = toJSON . getFloater



-- TODO name
-- TODO behavior for non-forward spans?
largestSpan :: Span -> Span -> Span
largestSpan a b = (aOn `min` bOn, aOff `max` bOff)^.from onsetAndOffset
  where
    (aOn,aOff) = a^.onsetAndOffset
    (bOn,bOff) = b^.onsetAndOffset

renderFloater :: (HasParts' a, PartOf a ~ Part) => Floater a -> Score a
renderFloater = rcat . fmap renderAlignedVoice . reverse . fmap (fmap $ (^.voice) . return) . getFloater


left, center, right :: Alignment
left = 0
center = 1/2
right = 1

{-
TODO think about various floater shapes i.e. ([Alignment],[Duration])
How to do this regardless or number of parts? Function of number of parts?

We don't need *that* many shapes, as pitch material, scaling, number of parts and context is also important params.

Don't forget the basic ones such as (const (repeat 0, repeat 0))
-}
newtype Shape = Shape { getShape :: Int -> ([Alignment],[Duration]) } -- both lists infinite!

-- Delay-invariant
instance Transformable Shape where
  transform s = over (_Wrapped'.mapped._2) (transform s)

instance Inspectable Shape where
  inspectableToMusic shape = pseq $ fmap (renderFloater.makeFloater shape) $ fmap dummyPitches [{-3,6,10,-}16{-,40-}]
  -- It is not specified how many pitches a shape should contain, so we render it with some different
  -- numbers and draw that in sequence.
    where
      dummyPitches x = (^.chord) $ rev (take (x`div`2) dw) <> take (x`div`2) uw
      uw = enumChromaticFromTo c (octavesUp 100 c)
      dw = enumDownChromaticFromTo c (octavesDown 100 c)

-- Take alignment from L and duration from R
combineShape :: Shape -> Shape -> Shape
combineShape (Shape f) (Shape g) = Shape $ \n -> let (as,_) = f n; (_,ds) = g n in (as,ds)

-- Like alerp, i.e.
-- > interpShape a b 0 = a
-- > interpShape a b 1 = b
interpShape :: Shape -> Shape -> Duration -> Shape
interpShape (Shape f) (Shape g) x = Shape (\n -> let (afn,dfn) = f n; (agn,dgn) = g n in
  (zipWith (\f g -> lerp f g x) afn agn, zipWith (\f g -> lerp f g x) dfn dgn))

-- | Combine shape and pitches into a floater.
makeFloater :: Shape -> Chord Pitch -> Floater StandardNote
makeFloater (Shape shape) (view (from chord) -> pitches) = makeFloater' (shape (length pitches)) pitches
  where
    makeFloater' (as,ds) ps = set (mapped.parts'._instrument) violin $
      mconcat $ zipWith3 (\a d p -> align a $ stretch d $ fromPitch p) as ds ps

-- | Combine shape and pitches into a floater.
makeFloater' :: Shape -> Chord Pitch -> [Part] -> Floater StandardNote
makeFloater' (Shape shape) (view (from chord) -> pitches) = makeFloater' (shape (length pitches)) pitches
  where
    makeFloater' (as,ds) ps rs =
      mconcat $ Data.List.zipWith4 (\a d p r -> set (mapped.parts') r $ align a $ stretch d $ fromPitch p) as ds ps rs


-- >>> overtone 6
-- bb
overtone    = ([c_,c,g,c',e',g',bb',c'',d'',e'',fs''::Pitch] !!)
fundamental = overtone 0


type StringNumber = Int
type HarmonicNumber = Int

stringPitch :: Instrument -> StringNumber {-[0..3]-} -> Pitch
stringPitch i n
  | i == violin = [g_,d,a,e] !! n
  | i == viola  = [c_,g_,d,a] !! n
  | i == cello  = [c__,g__,d_,a_] !! n
  | i == doubleBass = _8vb $ [e__,a__,d_,g_] !! n

harmonicPitch :: Instrument -> StringNumber -> HarmonicNumber -> Pitch
harmonicPitch i n o = stringPitch i n .+^ (overtone o .-. overtone 0)

printNotesOutOfRange :: Score StandardNote -> IO ()
printNotesOutOfRange x = do
  forM_ (x^.events) $ \e -> do
    if (not $ inAmbitus (playableRange $ e^.part._instrument) (e^?!pitches)) then
      putStrLn $ "Out of onsetAndOffset for " ++ red (show (e^.part._instrument)) ++ "\n\t" ++ yellow (show (e^?!pitches))
      else
      return ()
  return ()
    where
      red    s = "\x1b[31m" <> s <> "\x1b[0m"
      green  s = "\x1b[32m" <> s <> "\x1b[0m"
      yellow s = "\x1b[33m" <> s <> "\x1b[0m"
--
-- printOverlappingNotes :: Score StandardNote -> IO ()
-- printOverlappingNotes x = do
--   forM_ (extractPartsWithInfo x) $ \(partName, y) ->
--     forM_ (allOverlapEras y) $ \(s,t) ->
--       putStrLn $ "Notes 's' and 't' in part "++show partName++" overlap!"
--   return ()
--     where
--       red    s = "\x1b[31m" <> s <> "\x1b[0m"
--       green  s = "\x1b[32m" <> s <> "\x1b[0m"
--       yellow s = "\x1b[33m" <> s <> "\x1b[0m"
--
-- -- TODO wrong because of self! See definition of hasOverlappingEvents!
-- allOverlapEras :: Score a -> [(Span, Span)]
-- allOverlapEras = over (mapped.both) (^.era) . allOverlaps
--
-- allOverlaps :: Score a -> [(Event a, Event a)]
-- allOverlaps s = mcatMaybes $ 
--   [if (x^.era) `overlaps` (y^.era) then Just (x,y) else Nothing | x <- s^.events, y <- s^.events ]

-- standardInstrumentRanges :: Music
-- standardInstrumentRanges = pseq $ map (\i -> addText (show i) $ set parts' (tutti i) $ onsetAndOffsetToMus (playableRange i))
--   [piccoloFlute,flute,altoFlute,
--    oboe,corAnglais,
--    ebClarinet,clarinet,bassClarinet,
--    bassoon,contraBassoon,
--    horn,horn,horn,horn,
--    trumpet,trumpet,trumpet,
--    trombone,trombone,bassTrombone,
--    tuba,
--    piano,
--    harp^._instrument,
--    celesta,
--    vibraphone,
--    timpani,
--    violin,
--    viola,
--    cello,
--    doubleBass
--   ]
--   where
--     -- onsetAndOffsetToMus = renderForDisplay
--     onsetAndOffsetToMus x = let (m,n) = x^.from ambitus in ppar $ map fromPitch'' $ enumChromaticFromTo m n
--

{-
What the Show for instuments should eventually generate...
-}
showPartE :: Part -> String
showPartE p
  | p == mempty = "mempty"
  | otherwise = showSubpartE (showInstrumentE (p^._instrument)) (p^._subpart)

showSubpartE e (Subpart [])    = e++"s"
showSubpartE e (Subpart (d:_)) = "(divide "++show n++" "++e++"s !! "++show m++")"
  where
    (m,n) = getDivision d
showInstrumentE i
  | i == cello = "cello"
  | i == horn = "horn"
  | i == clarinet = "clarinet"
  | otherwise  = fmap Data.Char.toLower $ show i

newtype Sym = Sym String
  deriving (Transformable)
instance Show Sym where
  show (Sym x) = x

-- Convert a score to something that shows pitches and parts correctly (a hack until Show instances are correct)
showPitchAndPart :: Score StandardNote -> Score Sym
showPitchAndPart = fmap (Sym . \x -> "set parts' " ++ showPartE (x^.part) ++ " " ++ show (x^?!pitches))

type PitchOf a = Music.Score.Pitch a
type P a = Music.Score.Pitch a
type PartOf a = Music.Score.Part a
type R a = Music.Score.Part a
type DynamicOf a = Music.Score.Dynamic a
type D a = Music.Score.Dynamic a
type ArticulationOf a = Music.Score.Articulation a
type A a = Music.Score.Articulation a


showAmbitus :: (IsPitch a, HasColor a) => Ambitus Interval Pitch -> Score a
showAmbitus a = let (m,n) = a^.from ambitus in ppar $ fmap (pure . colorBlue . fromPitch'') [m, n]

-- -- | The number of whole octaves in an ambitus.
-- ambitusOctaves :: Ambitus Pitch -> Int
-- ambitusOctaves = fromIntegral . octaves . ambitusInterval
--
-- -- | The lowest octave (relative middle C) in present a given ambitus.
-- ambitusLowestOctave :: Ambitus Pitch -> Int
-- ambitusLowestOctave = fromIntegral . octaves . (.-. c) . ambitusLowest
--
-- -- | Interpolate between the highest and lowest points in an ambitus.
-- --
-- -- Can be used as a primitive contour-based melody generator.
-- --
-- interpolateAmbitus :: (Ord a, Num a, AffinePair (Diff a) a) => Ambitus a -> Scalar (Diff a) -> a
-- interpolateAmbitus a = let (m,n) = a^.from ambitus in alerp m n
--
-- -- |
-- -- Same as @interpolateAmbitus@ but allow continous interpolation of standard pitch
-- -- (as @Scalar (Diff Pitch) ~ Integer@).
-- --
-- interpolateAmbitus' :: Ambitus Pitch -> Double -> Pitch
-- interpolateAmbitus' a x = (^.from pitchDouble) $ interpolateAmbitus (mapAmbitus (^.pitchDouble) a) x
--   where
--     -- We can't interpolate an (Ambitus Pitch) using fractions because of music-pitch/issues/16
--     -- Work around by converting pitches into doubles and back
--     -- Use Double rather than Hertz due to the latter's suspect affine space instance
--     -- Only an Iso up to enharmonic equivalence.
--     pitchDouble :: Iso' Pitch Double
--     pitchDouble = iso (\x -> fromIntegral (semitones (x.-.c))) (\x -> c .+^ spell usingSharps (round x::Semitones))


----


--------

i :: Int -> Interval
i = spell {-s-}modally . (`asTypeOf` (0::Semitones)) . fromIntegral
  where
    -- s = fromJust . flip lookup (zip [0..] [1,2,2,3,4,4,5,5,6,6,7,7])
    -- fromJust (Just x) = x

--------


--------

addDuration :: Score a -> Score (Duration, a)
addDuration = mapWithSpan (\s x -> (s^.duration, x))

uncurry3 :: (a -> b -> c -> d) -> (a, (b, c)) -> d
uncurry3 f (a,(b,c)) = f a b c

--------

-- -- >>> 2 `inside` (3<->4)
-- -- False
-- -- >>> 3 `inside` (3<->4)
-- -- True
-- --
-- -- >>> 2 `strictlyInside` (3<->4)
-- -- False
-- -- >>> 3 `strictlyInside` (3<->4)
-- -- False
-- -- >>> 3.5 `strictlyInside` (3<->4)
-- -- True
-- --
-- strictlyInside :: Time -> Span -> Bool
-- t `strictlyInside` s = t `inside` s && (t > s^.onset) && (t < s^.offset)
--
-- -- | If the given time is outside the given span, return the closest point inside.
-- --   Otherwise return the given time.
-- --
-- -- >>> closestPointInside (1 <-> 3) 0
-- -- 1
-- -- >>> closestPointInside (1 <-> 3) 55
-- -- 3
-- -- >>> closestPointInside (1 <-> 3) 2
-- -- 2
-- closestPointInside :: Span -> Time -> Time
-- closestPointInside ((^.onsetAndOffset) -> (m,n)) t
--   | t < m     = m
--   | t > n     = n
--   | otherwise = t

--------

-- >>> accumUntil (\s a -> if s < 345 then Left (s + a) else Right s) 0 [1..]
-- Just 351
accumUntil :: (s -> a -> Either s b) -> s -> [a] -> Maybe b
accumUntil f z xs = Data.Maybe.listToMaybe $ fmap fromRight $ dropWhile Data.Either.isLeft $ scanl (f . fromLeft) (Left z) xs
    where
      fromRight (Right x) = x
      fromLeft (Left x) = x
--------

{-
TEXTURES
  Arpeggio
  Style brise (a permutation of arpeggio)
  Alberti bass and other partially repetitive structures (as in the Bach C major prel)
  Repeated single notes (subdivision of pitches into orchestra)
-}
{-
Splittable cuts a vector-like value into along a point at its middle, analogous to 'take' and 'drop' on lists.
The given duration is interpolated along the vector, so 0 means split at start, and 'duration' means split at end.
For relative splitting, see 'splitRel'.
-}

-- | Split a value relative its duration.
splitRel :: (Splittable a, Transformable a) => Alignment -> a -> (a, a)
splitRel v a = split (v*(a^.duration)) a


-- | Like 'durationsAsVoice' but generating alphabetic note names, thus giving slightly more readable expression.
--
-- >>> [2,4,2]^.durationsAsVoice'
-- [(2,'a')^.note,(4,'b')^.note,(2,'c')^.note]^.voice
--
durationsAsVoice' :: Getter [Duration] (Voice Char)
durationsAsVoice' = to $ \ds -> (^.voice) $ zipWith (curry (^.note)) ds (cycle ['a'..'z'])

voiceAsDurations = from durationsAsVoice

showOrigin = (<> (set parts' (tutti timpani) $ c^/4))

--
-- -- Useful natural transformations
--
noteToVoice :: Note a -> Voice a
noteToVoice = view voice . pure
--
eventToScore :: Event a -> Score a
eventToScore = view score . pure



-- | Move all onsets or offsets to a point on a grid of the given distance centered around zero.
quantizeScore :: Duration -> Score a -> Score a
quantizeScore d = over (events.each.era) (quantizeSpan d)

-- | Move onset and offset to a point on a grid of the given distance centered around zero.
quantizeSpan :: Duration -> Span -> Span
quantizeSpan d = over (onsetAndOffset . both) (snapToGridBackward d)

-- | Return the last point before the given point, whose distance from zero is an even multiple of the given duration.
-- Effectively places any point on a grid centered around the origin.
--
-- >>> snapToGridBackward 3 1
-- 0
-- >>> snapToGridBackward 3 5
-- 3
-- >>> snapToGridBackward 3 (-1)
-- -3
snapToGridBackward :: Duration -> Time -> Time
snapToGridBackward d t = 0 .+^ (d^*fromIntegral n)
 Querying and traversing scores  where
     (n,rho) = cycleAndPhase t d






hull :: Span -> Span -> Span
hull (view onsetAndOffset -> (a,b)) (view onsetAndOffset -> (c,d)) = (minimum[a,b,c,d],maximum[a,b,c,d])^.from onsetAndOffset

----------------------------------------------------------------------------------------------------
-- Patterns

----------------------------------------------------------------------------------------------------

-- rcat :: (Monoid a, Semigroup a, HasParts a a, PartOf a ~ Part) => [a] -> a
-- rcat xs = if allDistinct ps
--     then ppar xs
--     else ppar $ zipWith (set parts') (divide (length xs) p) xs
--   where
--     ps = concatMap (toListOf parts') xs
--     p  = foldr1 largestPart ps

----------------------------------------------------------------------------------------------------
-- Pitch maps

-- | A way of transforming pitches, i.e. to replacing pitches in a texture.
type PitchMap = Pitch -> Pitch
defPitchMap = id

-- | A way of transforming pitches, i.e. to replacing pitches in a texture.
type PartialPitchMap = Pitch -> Maybe Pitch

-- | Create a function mapping diatonic steps to indexes in an infinitely repeating chord.
chordToPitchMap :: [Pitch] -> PitchMap
chordToPitchMap chord p = chord !! (n `mod` length chord)
  where
    n = fromIntegral ((p.-.c)^._steps)

chordToPitchMapWithOctaves :: [Pitch] -> PitchMap
chordToPitchMapWithOctaves chord p = octavesUp (fromIntegral o) $ chord !! (fromIntegral r `mod` length chord)
  where
    (o,r) = ((p.-.c)^._steps) `divMod` 7

-- TODO
chordToPartialPitchMapWithOctaves :: [Pitch] -> PartialPitchMap
chordToPartialPitchMapWithOctaves chord p = if length chord > fromIntegral r then Just p2 else Nothing
  where
    p2    = octavesUp (fromIntegral o) $ chord !! (fromIntegral r `mod` length chord)
    (o,r) = ((p.-.c)^._steps) `divMod` 7

applyPitchMap :: (HasPitches' a, PitchOf a ~ Pitch) => PitchMap -> Pattern a -> Pattern a
applyPitchMap pitchMap = fmap (over pitches' pitchMap)

applyPartialPitchMap :: (HasPitches' a, PitchOf a ~ Pitch) => PartialPitchMap -> Pattern a -> Pattern (Maybe a)
applyPartialPitchMap pitchMap = fmap go
  where
    go x = case (x^?pitches) of
      Nothing -> Nothing
      Just p -> case pitchMap p of
        Nothing -> Nothing
        Just p2 -> Just (set pitches' p2 x)


applyChordPitches :: (HasPitches' a, PitchOf a ~ Pitch) => [Pitch] -> Pattern a -> Pattern a
applyChordPitches = applyPitchMap . chordToPitchMap

applyChordPitchesWithOctaves :: (HasPitches' a, PitchOf a ~ Pitch) => [Pitch] -> Pattern a -> Pattern a
applyChordPitchesWithOctaves = applyPitchMap . chordToPitchMapWithOctaves

applyChordPitchesWithOctavesAndRests :: (HasPitches' a, PitchOf a ~ Pitch) => [Pitch] -> Pattern a -> Pattern (Maybe a)
applyChordPitchesWithOctavesAndRests = applyPartialPitchMap . chordToPartialPitchMapWithOctaves

applyChordPitchesWithOctavesAndRests2 :: (HasPitches' a, PitchOf a ~ Pitch) => [Pitch] -> Pattern (Maybe a) -> Pattern (Maybe a)
applyChordPitchesWithOctavesAndRests2 ch = fmap join . applyChordPitchesWithOctavesAndRests ch


catPartWise :: (HasPart' a, Eq (PartOf a)) => Score a -> Score a -> Score a
catPartWise x y = x <> delay (foldr max 0 $ fmap negateV distances) y
  where
    -- distances =
    px = toListOf parts' x
    py = toListOf parts' y
    distances = groupSame (\p -> (extractPart p y)^.onset .-. (extractPart p x)^.offset) px py

-- | Process elements present in /both/ xs and ys.
groupSame :: Eq a => (a -> b) -> [a] -> [a] -> [b]
groupSame f xs ys = mcatMaybes $ map (\y -> if y `elem` xs then Just (f y) else Nothing) ys


replaceParts :: (HasParts' a, Eq (PartOf a)) => [(PartOf a, PartOf a)] -> a -> a
replaceParts xs = over parts' (`lookupPos` xs)
  where
    lookupPos x ys = Data.Maybe.fromMaybe x $ lookup x ys

replacePitches :: (HasPitches' a, Eq (PitchOf a)) => [(PitchOf a, PitchOf a)] -> a -> a
replacePitches xs = over pitches' (`lookupPos` xs)
  where
    lookupPos x ys = Data.Maybe.fromMaybe x $ lookup x ys

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- Chords for Bach WTC I C major prelude (last 3 bars simplified)
bachCMajChords :: IsPitch a => Score [a]
bachCMajChords =
  [(0 <-> (1/2),[c,e,g,c',e'])^.event,((1/2) <-> 1,[c,e,g,c',e'])^.event,(1 <-> (3/2),[c,d,a,d',f'])^.event,((3/2) <->
  2,[c,d,a,d',f'])^.event,(2 <-> (5/2),[b_,d,g,d',f'])^.event,((5/2) <-> 3,[b_,d,g,d',f'])^.event,(3 <->
  (7/2),[c,e,g,c',e'])^.event,((7/2) <-> 4,[c,e,g,c',e'])^.event,(4 <-> (9/2),[c,e,a,e',a'])^.event,((9/2) <->
  5,[c,e,a,e',a'])^.event,(5 <-> (11/2),[c,d,fs,a,d'])^.event,((11/2) <-> 6,[c,d,fs,a,d'])^.event,(6 <->
  (13/2),[b_,d,g,d',g'])^.event,((13/2) <-> 7,[b_,d,g,d',g'])^.event,(7 <-> (15/2),[b_,c,e,g,c'])^.event,((15/2) <->
  8,[b_,c,e,g,c'])^.event,(8 <-> (17/2),[a_,c,e,g,c'])^.event,((17/2) <-> 9,[a_,c,e,g,c'])^.event,(9 <->
  (19/2),[d_,a_,d,fs,c'])^.event,((19/2) <-> 10,[d_,a_,d,fs,c'])^.event,(10 <-> (21/2),[g_,b_,d,g,b])^.event,((21/2) <->
  11,[g_,b_,d,g,b])^.event,(11 <-> (23/2),[g_,bb_,e,g,cs'])^.event,((23/2) <-> 12,[g_,bb_,e,g,cs'])^.event,(12 <->
  (25/2),[f_,a_,d,a,d'])^.event,((25/2) <-> 13,[f_,a_,d,a,d'])^.event,(13 <-> (27/2),[f_,ab_,d,f,b])^.event,((27/2) <->
  14,[f_,ab_,d,f,b])^.event,(14 <-> (29/2),[e_,g_,c,g,c'])^.event,((29/2) <-> 15,[e_,g_,c,g,c'])^.event,(15 <->
  (31/2),[e_,f_,a_,c,f])^.event,((31/2) <-> 16,[e_,f_,a_,c,f])^.event,(16 <-> (33/2),[d_,f_,a_,c,f])^.event,((33/2) <->
  17,[d_,f_,a_,c,f])^.event,(17 <-> (35/2),[g__,d_,g_,b_,f])^.event,((35/2) <-> 18,[g__,d_,g_,b_,f])^.event,(18 <->
  (37/2),[c_,e_,g_,c,e])^.event,((37/2) <-> 19,[c_,e_,g_,c,e])^.event,(19 <-> (39/2),[c_,g_,bb_,c,e])^.event,((39/2) <->
  20,[c_,g_,bb_,c,e])^.event,(20 <-> (41/2),[f__,f_,a_,c,e])^.event,((41/2) <-> 21,[f__,f_,a_,c,e])^.event,(21 <->
  (43/2),[fb__,c_,a_,c,eb])^.event,((43/2) <-> 22,[fb__,c_,a_,c,eb])^.event,(22 <-> (45/2),[ab__,f_,b_,c,d])^.event,((45/2)
  <-> 23,[ab__,f_,b_,c,d])^.event,(23 <-> (47/2),[g__,f_,g_,b_,d])^.event,((47/2) <-> 24,[g__,f_,g_,b_,d])^.event,(24 <->
  (49/2),[g__,e_,g_,c,e])^.event,((49/2) <-> 25,[g__,e_,g_,c,e])^.event,(25 <-> (51/2),[g__,d_,g_,c,f])^.event,((51/2) <->
  26,[g__,d_,g_,c,f])^.event,(26 <-> (53/2),[g__,d_,g_,b_,f])^.event,((53/2) <-> 27,[g__,d_,g_,b_,f])^.event,(27 <->
  (55/2),[g__,eb_,a_,c,fs])^.event,((55/2) <-> 28,[g__,eb_,a_,c,fs])^.event,(28 <-> (57/2),[g__,e_,g_,c,g])^.event,((57/2)
  <-> 29,[g__,e_,g_,c,g])^.event,(29 <-> (59/2),[g__,d_,g_,c,f])^.event,((59/2) <-> 30,[g__,d_,g_,c,f])^.event,(30 <->
  (61/2),[g__,d_,g_,b_,f])^.event,((61/2) <-> 31,[g__,d_,g_,b_,f])^.event,(31 <-> (63/2),[c__,c_,g_,bb_,e])^.event,((63/2)
  <-> 32,[c__,c_,g_,bb_,e])^.event,(32 <-> 33,[c__,c_,f_,a_,e])^.event,(33 <-> 34,[c__,b__,f,g,d'])^.event,(34 <->
  35,[c__,c_,e,g,c'])^.event]^.score

-- Pattern used in Bach C Major prelude
bachCMajPattern :: (Reversible a, Num a) => Pattern a
bachCMajPattern = newPattern $ stretchTo 1 $ (^.voice) $ fmap pure [0,1,2,3,4,2,3,4]

-- TODO generalize to any vector
-- > v*^n ^+^ r = x where (n,r) = divModDur x v
divModDur :: Duration -> Duration -> (Integer, Duration)
divModDur x v = (n,r)
  where
    n = floor (x / v)
    -- r follows from (v*^n ^+^ r = x)
    r = x ^-^ (v*^fromIntegral n)
{-# INLINE divModDur #-}

-- XXX use proper math terminology here!
-- | Given a duration, return the cycle number and phase of a given time point relative origo.
--
-- >>> (2::Time) `cycleAndPhase` 3
-- (0,2)                              -- The point in time 2 happens in the 0-th 3-cycle at phase 2
-- >>> (-2::Time) `cycleAndPhase` 3
-- (-1,1)                             -- The point in time -1 happens in the (-1)-th 3-cycle at phase 1
--
-- cycleAndPhase :: Time -> Duration -> (Integer, Duration)
-- cycleAndPhase t d = (t .-. 0) `divModDur` d
-- {-# INLINE cycleAndPhase #-}


-- >>> divBy 2 4
-- True
-- >>> divBy 3 4
-- False
-- >>> divBy (1/6) (1/3)
-- True
divBy :: RealFrac a => a -> a -> Bool
divBy x a = floor (a / x) == ceiling (a / x)

fromChords :: Score [Pitch] -> Music
fromChords = join . fmap ppar . (fmap.fmap) fromPitch''

toChords :: Music -> Score [Pitch]
toChords = fmap (^..pitches)

printPitches :: (HasPitches' a, PitchOf a ~ p, Show p, Ord p) => a -> IO ()
printPitches x = mapM_ print $ Data.List.sort $ Data.List.nub $ toListOf pitches x

printParts :: (HasParts' a, PartOf a ~ p, Show p, Ord p) => a -> IO ()
printParts x = mapM_ print $ Data.List.sort $ Data.List.nub $ toListOf parts x

-- printPitches :: (HasPitches' a, PitchOf a ~ p, Show p, Ord p) => a -> IO ()
-- printPitches x = mapM_ print $ Data.List.sort $ Data.List.nub $ toListOf pitches x
--
printDynamics :: (HasDynamics' a, DynamicOf a ~ p, Show p, Ord p) => a -> IO ()
printDynamics x = mapM_ print $ Data.List.sort $ Data.List.nub $ toListOf dynamics x

-- getPattern p _ = fmap (set parts' p) $ mconcat [c,d,e,f,g,a]

-- | Retain only notes falling into the given duration grid.
--
-- >>> filterOnsetOffsetDivisibleBy (1/4) $ compress 4 $ pseq [c,d^*2,e::Score Pitch]
-- [(0 <-> (1/4),c)^.event,((3/4) <-> 1,e)^.event]^.score
--
filterOnsetDurationDivisibleBy :: Duration -> Score a -> Score a
filterOnsetDurationDivisibleBy d = filterWithSpan (\s _ -> divBy d (s^.duration) && divBy (0.+^d) (s^.onset))

-- >>> voiceToBehaviorFirst 0 0 (pure 1 <> pure 2) ! (-1)
-- Nothing
-- >>> voiceToBehaviorFirst 0 0 (pure 1 <> pure 2) ! (0)
-- Just 1
-- >>> voiceToBehaviorFirst 0 0 (pure 1 <> pure 2) ! (1)
-- Just 1
-- >>> voiceToBehaviorFirst 0 0 (pure 1 <> pure 2) ! (2)
-- Just 2
--
-- >>> voiceToBehaviorLast 0 0 (pure 1 <> pure 2) ! (-1)
-- Nothing
-- >>> voiceToBehaviorLast 0 0 (pure 1 <> pure 2) ! (0)
-- Just 1
-- >>> voiceToBehaviorLast 0 0 (pure 1 <> pure 2) ! (1)
-- Just 2
-- >>> voiceToBehaviorLast 0 0 (pure 1 <> pure 2) ! (2)
-- Just 2

voiceToBehaviorLast :: Time -> Alignment -> Voice a -> Behavior (Maybe a)
voiceToBehaviorLast a b x = fmap (fmap getLast.getOption) $ voiceToBehavior $ aligned a b $ fmap (Option . Just . Last) x

voiceToBehaviorFirst :: Time -> Alignment -> Voice a -> Behavior (Maybe a)
voiceToBehaviorFirst a b x = fmap (fmap getFirst.getOption) $ voiceToBehavior $ aligned a b $ fmap (Option . Just . First) x

voiceToBehaviorLastDef :: Time -> Alignment -> a -> Voice a -> Behavior a
voiceToBehaviorLastDef a b z x = fmap (Data.Maybe.fromMaybe z) $ voiceToBehaviorLast a b x


-- FIXME unsafe
-- Switchpoint belongs to latter value
voiceAtDuration :: Voice a -> Alignment -> Maybe a
voiceAtDuration v = fmap getLast . getOption . voiceAtDuration' (fmap (Option . Just . Last) v)

-- Switchpoint belongs to former value
voiceAtDurationFirst :: Voice a -> Alignment -> Maybe a
voiceAtDurationFirst v = fmap getFirst . getOption . voiceAtDuration' (fmap (Option . Just . First) v)

voiceAtDurationBoth :: Voice a -> Alignment -> [a]
voiceAtDurationBoth v = voiceAtDuration' (fmap pure v)

voiceAtDuration' :: Monoid a => Voice a -> Alignment -> a
voiceAtDuration' v d = voiceToBehavior (aligned o a v) ! (o .+^ d)
  where
    o = 0 -- Does not matter
    a = 0 -- Align to onset

-- Turn a voice into a behavior by aligning.
-- Value at switchpoint is determined by the monoid (see voiceAtDurationFirst etc).
voiceToBehavior :: Monoid a => Aligned (Voice a) -> Behavior a
voiceToBehavior = scoreToBehavior . renderAlignedVoice

scoreToBehavior :: Monoid a => Score a -> Behavior a
scoreToBehavior = concatB . fmap pure


-- [(0,'a'),(2,'b')]^.behaviorLast

behavior' :: Monoid a => Getter [(Duration, a)] (Behavior a)
behavior' = to (map (view note)) . voice . to (voiceToBehavior . aligned 0 0)

behaviors :: Monoid a => Getter [(Duration, Behavior a)] (Behavior a)
-- behaviors = behavior' . to join
behaviors = to (j 0) where
  j t []         = mempty
  j t ((d,x):xs) = switch (t .+^ d) x (j (t.+^d) xs)

behaviorLast :: Getter [(Duration, a)] (Behavior (Maybe a))
behaviorLast = to (map (view note)) . voice . to (voiceToBehaviorLast 0 0)

behaviorLastDef :: a -> Getter [(Duration, a)] (Behavior a)
behaviorLastDef z = to (map (view note)) . voice . to (voiceToBehaviorLastDef 0 0 z)






cc3 :: [[Pitch]]
cc3 = fmap2 (\n -> c .+^ spell modally (fromInteger n :: Semitones)) raw3
  where
    raw3 = [
      [0,4,8],
      [0,3,6],
      [0,2,4],
      [0,1,2],
      [0,2,7],
      [0,3,7],
      [0,1,6],
      [0,2,6],
      [0,1,5],
      [0,2,5],
      [0,1,4],
      [0,1,3]
      ]

cc4 :: [[Pitch]]
cc4 = fmap2 (\n -> c .+^ spell modally (fromInteger n :: Semitones)) raw4
  where
    raw4 = [
      [0,1,2,3],
      [0,1,6,7],
      [0,2,3,5],
      [0,2,5,7],
      [0,3,6,9],
      [0,1,2,7],
      [0,1,3,6],
      [0,1,4,5],
      [0,1,3,4],
      [0,1,5,6],
      [0,2,4,6],
      [0,2,6,8],
      [0,3,4,7],
      [0,3,5,8],
      [0,1,5,8],
      [0,2,4,8],
      [0,1,2,4],
      [0,1,4,6],
      [0,1,5,7],
      [0,1,2,5],
      [0,1,4,7],
      [0,1,2,6],
      [0,1,3,7],
      [0,3,4,8],
      [0,2,3,7],
      [0,1,3,5],
      [0,2,4,7],
      [0,2,3,6],
      [0,2,5,8]
      ]

-- >>> upwardCompressor _f 0.6         -- increase soft dynamics
-- >>> downwardCompressor _f 0.2       -- decrease loud dynamics
-- >>> downwardCompressor ff 0.0000001 -- limiter
-- >>> upwardCompressor pp 100000000   -- noise gate
downwardCompressor :: (Attenuable a, Ord (Level a), Num (Level a)) => Dynamic a -> Scalar (Level a) -> a -> a
downwardCompressor th r = over dynamics $ relative th (\x -> if x > (0) then x^*r else x)

upwardCompressor :: (Attenuable a, Ord (Level a), Num (Level a)) => Dynamic a -> Scalar (Level a) -> a -> a
upwardCompressor th r = over dynamics $ relative th (\x -> if x < (0) then x^*r else x)

-- -- TODO bad?
-- instance Reversible a => Reversible (Note a) where
--   rev = fmap rev


toDouble :: Real a => a -> Double
toDouble = realToFrac

fmap2 = fmap . fmap

-- >>> take 10 (cycle (palindr [1,2,3]))
palindr :: [a] -> [a]
palindr []  = error "palindromeL: empty list"
palindr [x] = [x]
palindr xs@(_:_)  = begin <> middle <> end <> reverse middle
  where
    begin  = pure (head xs)
    middle = init (tail xs)
    end    = pure (last xs)

palindr2 :: [a] -> ([a], [a])
palindr2 []  = error "palind2: empty list"
palindr2 [x] = error "palindr2: singleton list"
palindr2 xs@(_:_)  = (begin <> middle, end <> reverse middle)
  where
    begin  = pure (head xs)
    middle = init (tail xs)
    end    = pure (last xs)

md5 :: String -> String
md5 x = reverse $ drop 1 $ reverse
  $ System.IO.Unsafe.unsafePerformIO $ System.Process.readProcess "md5" ["-q"] x


-- Last time the given key was pressed but not released (non-existant means it is not pressed)
type ChanMap = Map Int Time

readMidi' pa = do
  Right m <- Midi.importFile pa
  let s = asScore $ fromMidi' m
  return s

-- |
-- Convert a score from a Midi representation.
--
fromMidi' :: (HasParts' a, PartOf a ~ Part, DynamicOf a ~ Dynamics, IsPitch a) => Midi.Midi -> Score a
fromMidi' = fmap (\(r,p,d) -> set parts' r $ {-level d $-} fromPitch'' p) . (^.score) . toAspectsMidi . getMidi
  where


toAspectsMidi :: [[Event (Midi.Channel,Midi.Key,Midi.Velocity)]] -> [Event (Part,Pitch,Dynamics)]
toAspectsMidi = concat . mapWithIndex
  (\trackN -> fmap2 (\((ch,key,vel)) -> (getPart trackN ch,getPitch key,getDyn vel)))
  where
    getPart tr ch = (fmap (divide 16) (divide 16 mempty) !! tr) !! ch
    getPitch key = c.+^(spell usingSharps ((fromIntegral key) - 60::Semitones))
    getDyn vel = mempty -- TODO for now


-- pucko :: [(Midi.Ticks, a)] -> [(Midi.Ticks, a)]
-- pucko xs = zip (toAbs ts) ys
--   where
--     (ts,ys) = unzip xs
--     toAbs x = tail $ offsetPoints (0::Int) x
pucko = id

getMidi :: Midi.Midi -> [[Event (Midi.Channel,Midi.Key,Midi.Velocity)]]
getMidi (Midi.Midi fileType timeDiv tracks) = id
      $ compress (ticksp timeDiv * 4)
      $ fmap mcatMaybes
      $ fmap snd
      $ fmap (Data.List.mapAccumL g mempty)
      $ fmap (Data.List.sort) -- TODO comapre just time
      -- FIXME midi times are relative...
      $ fmap pucko
      $ fmap mcatMaybes
      $ fmap2 getMsg tracks
  where
    g keyStatus (t,(onOff,c,p,v)) =
      ( updateKeys onOff p (fromIntegral t) keyStatus
      , (if onOff then Nothing else Just (
        (Data.Maybe.fromMaybe 0 (Map.lookup (fromIntegral t) keyStatus)<->fromIntegral t,(c,p,60))^.event))
      )
    -- TODO also store dynamics in pitch map (to use onset value rather than offset value)
    -- For now just assume 60
    updateKeys True  p t = Map.insert p t
    updateKeys False p _ = Map.delete p

    -- Amount to compress time (after initially treating each tick as duration 1)
    ticksp (Midi.TicksPerBeat n)     = fromIntegral n
    ticksp (Midi.TicksPerSecond _ _) = error "fromMidi: Can not parse TickePerSecond-based files"

getMsg (t, Midi.NoteOn c p 0)  = Just (t,(False,c,p,0))
getMsg (t, Midi.NoteOn c p v)  = Just (t,(True,c,p,v))
getMsg (t, Midi.NoteOff c p v) = Just (t,(False,c,p,v))
-- getMsg (t, _) = Nothing

-- TODO key pressure
-- control change
-- program change
-- channel pressure
-- pitch wheel
-- etc.
getMsg _ = Nothing


unRatio x = (Data.Ratio.numerator x, Data.Ratio.denominator x)


lcms = foldr lcmG 1
gcdG :: RealFrac a => a -> a -> a
lcmG :: RealFrac a => a -> a -> a
lcmG a b = let f = (unRatio.toRational); (a1,a2)=f a; (b1,b2)=f b in fromIntegral (lcm a1 b1)/fromIntegral (gcd a2 b2)
gcdG a b = let f = (unRatio.toRational); (a1,a2)=f a; (b1,b2)=f b in fromIntegral (gcd a1 b1)/fromIntegral (lcm a2 b2)
lcmm :: Integral a => [a] -> a
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

asPitch :: Pitch -> Pitch
asPitch = id

sys  = System.Process.system
bm x = sys "date" >> x >> sys "date" >> return ()

timesN' :: Monoid a => Int -> a -> a
timesN' x = timesN (fromIntegral (abs x) :: Natural)


type Aspects a =
    (IsPitch a, HasParts' a, HasDynamics' a, Transposable a, Reversible a,
     PartOf a ~ Part, PitchOf a ~ Pitch, DynamicOf a ~ Dynamics,
     HasTremolo a)

durInMin x = toDouble $ tempoToDuration ((1/4) `metronome` 120) * (x^.duration) / 60

unmaybe = maybe mempty id


-- A pitch-hertz iso assuming standard intonation.
pitchHertz :: Iso' Pitch Hertz
pitchHertz = iso p2h h2p
  where
    p2h   = getIntonation standardIntonation
    h2p x = view (from pitchDouble) $ (+ 9) $ realToFrac $ (/100) $ cents $ (/440) x

    -- Use Double rather than Hertz due to the latter's suspect affine space instance
    -- Only an Iso up to enharmonic equivalence.
    pitchDouble :: Iso' Pitch Double
    pitchDouble = iso (\x -> fromIntegral (semitones (x.-.c))) (\x -> c .+^ spell usingSharps (round x::Semitones))

hertzPitch :: Iso' Hertz Pitch
hertzPitch = from pitchHertz


merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = (x:y:merge xs ys)



justT :: Tuning Interval
justT = Tuning justT'

justT' i = 2**(fromIntegral o) * go (spell usingSharps s)
  where
    (o,s) = separate i
    go i | i == _P1 = 1
         | i == _M2 = 9/8
         | i == _M3 = 5/4
         | i == _P4 = 4/3
         | i == _P5 = 3/2
         | i == _M6 = 5/3
         | i == _M7 = 15/8
         | i == _A1 = (5/3)*(5/4)/2
         | i == _A2 = (15/8)*(5/4)/2 -- or minor third
         | i == _A4 = (9/8)*(5/4)
         | i == _A5 = (5/4)*(5/4)
         | i == _A6 = (7/4)
         | otherwise = error $ "justT got" ++ show i


-- Util: play freqs in SuperCollider (with correct intonation)
-- playP :: [Hertz] -> IO ()
-- play2P :: [Hertz] -> IO ()
-- playS :: [Hertz] -> IO ()
-- playP = (\x -> openSuperCollider (x::Score (PartT Part {-Cents-}Double))) . rcat . fmap (pure . pure . toMidiP)
-- playS = (\x -> openSuperCollider (x::Score (PartT Part {-Cents-}Double))) . compress 12 . pseq . fmap (pure . pure . toMidiP)
-- toMidiP = \f -> toDouble $ cents (f/440)/100 + 9
-- play2P xs = playUsingTim $ ppar $ map fromPitch'' $ map (^.from pitchHertz) $ xs

-- playUsingTim x = do
--   writeMidi "/Users/hans/Desktop/test.mid" $ asScore x
--   System.Process.system "timidity /Users/hans/Desktop/test.mid 2>/dev/null >/dev/null"
--   return ()

-- Sort a chord based on dissonance, in C major just intonation
-- Higher value means more dissonant
chordDiss :: [Pitch] -> Hertz
chordDiss = diss . fmap inton
  where
    inton = (getIntonation $ intone (c,264) justT)
    -- inton = standardIntonation

-- Calculate spectral dissonance.
-- Only works as exp for freqs > 1
diss :: RealFrac a => [a] -> a
diss xs = lcms xs / minimum xs


fromJust (Just x) = x




-- Like alerp, i.e.
-- > interpPitches a b 0 = a -- up to enharmonic eq
-- > interpPitches a b 1 = b -- up to enharmonic eq
interpPitches :: [Pitch] -> [Pitch] -> Double -> [Pitch]
interpPitches as bs x = zipWith (\a b -> interpPitch a b x) as bs

-- Like alerp, i.e.
-- > interpPitch a b 0 = a -- up to enharmonic eq
-- > interpPitch a b 1 = b -- up to enharmonic eq
interpPitch :: Pitch -> Pitch -> Double -> Pitch
interpPitch a b x = (^.from pitchDouble) $ alerp (a^.pitchDouble) (b^.pitchDouble) x
  where
    -- Use Double rather than Hertz due to the latter's suspect affine space instance
    -- Only an Iso up to enharmonic equivalence.
    pitchDouble :: Iso' Pitch Double
    pitchDouble = iso (\x -> fromIntegral (semitones (x.-.c))) (\x -> c .+^ spell usingSharps (round x::Semitones))


chordFromList :: (IsInterval (Diff p), AffineSpace p) => [p] -> Chord p
chordFromList []     = error "chordFromList: Empty list"
chordFromList (p:ps) = functionToChord p (functionFromSteps (p `po` ps) _P8)
  where
    po p = tail.pointOffsets p

scaleFromList :: (IsInterval (Diff p), AffineSpace p) => [p] -> Scale p
scaleFromList []     = error "scaleFromList: Empty list"
scaleFromList (p:ps) = modeToScale p (modeFromSteps (p `po` ps) _P8)
  where
    po p = tail.pointOffsets p

-- TODO a better iso would be ([a],Diff a)
chord :: (IsInterval (Diff a), AffineSpace a) => Iso' [a] (Chord a)
chord = iso chordFromList chordToList

-- TODO a better iso would be ([a],Diff a)
scale :: (IsInterval (Diff a), AffineSpace a) => Iso' [a] (Scale a)
scale = iso scaleFromList scaleToList

-- Problematic for the same reason as the isos
instance (a ~ Pitch) => Monoid (Chord a) where
  mempty = []^.chord
  a `mappend` b = (Data.List.sort $ a^.from chord <> b^.from chord)^.chord
instance (a ~ Pitch) => Semigroup (Chord a) where
  (<>) = mappend







zipNotesWith :: (a -> b -> c) -> Voice (Maybe a) -> Voice b -> Voice (Maybe c)
zipNotesWith f as bs = (^.voice) $ zipWith g (as^.notes) (bs^.notes)
  where
    g (view (from note) -> (da,Nothing)) (view (from note) -> (db,xb)) = (da*db,Nothing)^.note
    g (view (from note) -> (da,Just xa)) (view (from note) -> (db,xb)) = (da*db,Just $ xa `f` xb)^.note



trimChord :: (IsInterval (Diff a), AffineSpace a, Ord a, Num a) => Ambitus v a -> Chord a -> Chord a
trimChord a x = under chord (filter $ inAmbitus $ a) x

-- instance (Show a, a ~ Pitch) => Show (Chord a) where
--   show x = show (x^.from chord) ++ "^.chord"











rhythm = durationsAsVoice

extractPartNamed :: (HasPart' a, Show (PartOf a), Eq (PartOf a)) => String -> Score a -> Maybe (Score a)
extractPartNamed  n x = fmap (`extractPart` x) . Data.List.find (isPrefixOfLowerCase n . show) $ (x^..parts)
  where
    {-
    Approximate string matching
    -}
    -- eqAnyCase :: String -> String -> Bool
    -- eqAnyCase x y = fmap Data.Char.toLower x == fmap Data.Char.toLower y

    isPrefixOfLowerCase :: String -> String -> Bool
    isPrefixOfLowerCase x y = x1 `Data.List.isPrefixOf` y1
      where
        x1 = fmap Data.Char.toLower x
        y1 = fmap Data.Char.toLower y

-- >>> :au compress 2 $ extractPartNamed' "fl" s
extractPartNamed' :: (HasPart' a, Show (PartOf a), Eq (PartOf a)) => String -> Score a -> Score a
extractPartNamed' n = Data.Maybe.fromMaybe (error $ "No part named " ++ show n) . extractPartNamed n
















instance AdditiveGroup a => AdditiveGroup [a] where
  zeroV = pure zeroV
  (^+^) = liftA2 (^+^)
  negateV = fmap negateV
instance VectorSpace a => VectorSpace [a] where
  type Scalar [a] = Scalar a
  s *^ v = fmap (s *^) v
----------------------------------------------------------------------
-- Suspect:
-- FIXME remove
instance HasDuration a => HasPosition (TieT a) where
  _era = mempty
instance (Splittable a, Transformable a, HasPosition a) => Splittable [a] where
  split d = unzipR . fmap (split d)
    where
      unzipR x = (fmap fst x, fmap snd x)
instance AdditiveGroup (Note a) where
  zeroV   = error "Note.mempty"
  (^+^)   = error "Note.(^+^)"
  negateV = error "Note.negateV"
instance VectorSpace (Note a) where
  type Scalar (Note a) = Duration
  s *^ v = stretch s v
instance (HasDuration a) => HasDuration (Maybe a) where
  _duration Nothing = 0
  _duration (Just x) = _duration x
-- instance (Splittable a) => Splittable (Maybe a) where
--   split d = unzipR . fmap (split d)
--     where
--       unzipR x = (fmap fst x, fmap snd x)
----------------------------------------------------------------------


instance HasDynamics Dynamics Dynamics where
  dynamics = id
instance HasDynamic Dynamics Dynamics where
  dynamic = id
type instance Music.Score.Dynamic Dynamics = Dynamics
type instance Music.Score.SetDynamic a Dynamics = a
-- type instance Diff Dynamics = Dynamics
instance ToJSON Dynamics where
  toJSON = toJSON . Data.Monoid.Average.maybeAverage


-- instance HasTremolo a => HasTremolo (Maybe a) where
--   setTrem n = fmap (setTrem n)

-- TODO remove
-- instance AffineSpace Int where
--   type Diff Int = Int
--   (.+^) = (+)
--   (.-.) = (-)

-- type instance Music.Score.Pitch (Ambitus a) = PitchOf a
-- type instance SetPitch b (Ambitus a) = Ambitus (SetPitch b a)
--
-- instance (HasPitches a b, Ord b, Num b, a ~ b, b ~ PitchOf (Ambitus b)) => HasPitches (Ambitus a) (Ambitus b) where
--   pitches = from ambitus.both

instance (ToJSON a, ToJSON b) => ToJSON (DynamicT b a) where
  toJSON (DynamicT (b,a)) = mergeJSON (Data.Aeson.object [("dynamics", toJSON b)]) (toJSON_NoArray a)
    where
      -- toJSON_NoArray = toJSON -- eventually should be this
      -- Workaround for the fact that StandardNote presently contains a list
      toJSON_NoArray = f . toJSON
      f (Data.Aeson.Array xs) = head $ toListOf traverse xs
      f x                     = x
instance (ToJSON a, ToJSON b) => ToJSON (PartT b a) where
  toJSON (PartT (b,a)) = mergeJSON (Data.Aeson.object [("part", toJSON b)]) (toJSON a)
instance (ToJSON a, ToJSON b) => ToJSON (ArticulationT b a) where
  toJSON (ArticulationT (b,a)) = mergeJSON (Data.Aeson.object [("articulation", toJSON b)]) (toJSON a)

instance ToJSON a => ToJSON (TieT a) where
  toJSON x = toJSON (extract x)
instance ToJSON a => ToJSON (ColorT a) where
  toJSON x = toJSON (extract x)
instance ToJSON a => ToJSON (TextT a) where
  toJSON x = toJSON (extract x)
instance ToJSON a => ToJSON (TremoloT a) where
  toJSON x = toJSON (extract x)
instance ToJSON a => ToJSON (HarmonicT a) where
  toJSON x = toJSON (extract x)
instance ToJSON a => ToJSON (SlideT a) where
  toJSON x = toJSON (extract x)

instance (FromJSON a, FromJSON b, Monoid b) => FromJSON (ArticulationT b a) where
  parseJSON y@(Data.Aeson.Object x) = do
    p <- x Data.Aeson..: "part"
    r <- parseJSON y
    return $ ArticulationT (p,r)
instance (FromJSON a, FromJSON b, Monoid b) => FromJSON (PartT b a) where
  parseJSON y@(Data.Aeson.Object x) = do
    p <- x Data.Aeson..: "part"
    r <- parseJSON y
    return $ PartT (p,r)
instance (FromJSON a, FromJSON b, Monoid b) => FromJSON (DynamicT b a) where
  parseJSON y@(Data.Aeson.Object x) = do
    p <- x Data.Aeson..: "part"
    r <- parseJSON y
    return $ DynamicT (p,r)

instance FromJSON (Average Double) where
  parseJSON x = pure mempty -- TODO
instance FromJSON (Average Double, Average Double) where
  parseJSON x = pure mempty -- TODO


instance FromJSON a => FromJSON (TieT a) where
  parseJSON = fmap pure . parseJSON
instance FromJSON a => FromJSON (ColorT a) where
  parseJSON = fmap pure . parseJSON
instance FromJSON a => FromJSON (TextT a) where
  parseJSON = fmap pure . parseJSON
instance FromJSON a => FromJSON (TremoloT a) where
  parseJSON = fmap pure . parseJSON
instance FromJSON a => FromJSON (HarmonicT a) where
  parseJSON = fmap pure . parseJSON
instance FromJSON a => FromJSON (SlideT a) where
  parseJSON = fmap pure . parseJSON

instance FromJSON a => FromJSON (Aligned a) where
  parseJSON = error "Util: No parseJSON: Aligned a"
-- TODO move
deriving instance Typeable Aligned

deriving instance Typeable ColorT

-- TODO assure obj
mergeJSON (Data.Aeson.Object x) (Data.Aeson.Object y)   = Data.Aeson.Object (x <> y)
mergeJSON x@(Data.Aeson.Object _) y                     = mergeJSON x (toObject y)
  where
    toObject x = Data.Aeson.object [("_", x)]
mergeJSON x                     y@(Data.Aeson.Object _) = mergeJSON (toObject x) y
  where
    toObject x = Data.Aeson.object [("_", x)]









{-
{-
Extra stash ops:
-}

-- List key and type of all values in the stash.
listAll :: IO ()
listAll = listMatching (const True)

{-
>>> listR "^rh"
>>> listR "^ch"
-}
listR :: String -> IO ()
listR k = withStashR print k >> return ()

deleteR :: String -> IO ()
deleteR k = withStashR (Database.Stash.delete . fst) k >> return ()

withStashR :: ((Database.Stash.Key, String) -> IO b) -> String -> IO [b]
withStashR act s = withStashMathing act (\x -> case fromAesonStr x of
  Just k -> k Text.Regex.Posix.=~ (s::String)
  Nothing -> False
  )
  where
    fromAesonStr (Data.Aeson.String x) = Just (Data.Text.unpack x)
    fromAesonStr _ = Nothing

listMatching :: (Database.Stash.Key -> Bool) -> IO ()
listMatching p = withStashMathing print p >> return ()

withStashMathing :: ((Database.Stash.Key,String) -> IO a) -> (Database.Stash.Key -> Bool) -> IO [a]
withStashMathing act p = do
  ts <- Database.Stash.findType' p
  mapM act (Data.List.sortBy g ts)
  where
    g (k,t) (k2,t2) = case compare t t2 of
      EQ -> f k k2
      LT -> LT
      GT -> GT

    f (Data.Aeson.String x) (Data.Aeson.String y) = compare x y
    f _ _ = EQ


-- | Version of 'get' that works with most inspectable types.
get_ :: Database.Stash.Key -> IO Music
get_ k = do
  -- TODO no such key
  t <- fmap (Data.Maybe.fromMaybe (error "get_: No such key")) $ Database.Stash.getType k
  case t of
    "Score (PartT Part (ColorT (TextT (TremoloT (HarmonicT (SlideT (ArticulationT ((Average Double),(Average Double)) (DynamicT (Average Double) [TieT Pitch]))))))))" -> fmap inspectableToMusic $ fmap (Data.Maybe.fromMaybe (error "No such value")) $ (Database.Stash.get k :: S (Score StandardNote))
    "Score (PartT Part (ArticulationT ((Average Double),(Average Double)) (DynamicT (Average Double) Pitch)))"        -> fmap inspectableToMusic $ fmap (Data.Maybe.fromMaybe (error "No such value")) $ (Database.Stash.get k :: S (Score (PartT Part (ArticulationT ((Average Double),(Average Double)) (DynamicT (Average Double) Pitch)))))
    "Score Pitch"        -> fmap inspectableToMusic $ fmap (Data.Maybe.fromMaybe (error "No such value")) $ (Database.Stash.get k :: S (Score Pitch))
    "Voice Pitch"        -> fmap inspectableToMusic $ fmap (Data.Maybe.fromMaybe (error "No such value")) $ (Database.Stash.get k :: S (Voice Pitch))
    "Voice (Maybe Pitch)"        -> fmap inspectableToMusic $ fmap (Data.Maybe.fromMaybe (error "No such value")) $ (Database.Stash.get k :: S (Voice (Maybe Pitch)))
    "Voice ()"           -> fmap inspectableToMusic $ fmap (Data.Maybe.fromMaybe (error "No such value")) $ (Database.Stash.get k :: S (Voice ()))
    "[Pitch]"           -> fmap inspectableToMusic $ fmap (Data.Maybe.fromMaybe (error "No such value")) $ (Database.Stash.get k :: S ([Pitch]))
    -- TODO unknown value

putAsp :: Database.Stash.Key -> Music -> IO ()
putAsp k = Database.Stash.put k . toAspects
getAsp :: Database.Stash.Key -> IO (Maybe Music)
getAsp k = fmap2 aspectsToMusic $ Database.Stash.get k
-}

type Asp1 = (PartT Part (ArticulationT Articulation (DynamicT Dynamics Pitch)))
type Asp = Score Asp1
toAspects :: (HasArticulation' a, HasDynamic' a, HasPart' a, HasPitches' a,
  ArticulationOf a ~ (Average Double, Average Double),
  DynamicOf a ~ Average Double,
  PartOf a ~ Part, PitchOf a ~ Pitch) => Score a -> Asp
toAspects = fmap g
  where
    g x = set part' r $ set articulation' a $ set dynamic' d $ fromPitch p
      where
        p = x^?!pitches
        d = x^.dynamic
        a = x^.articulation
        r = x^.part

aspectsToMusic :: Asp -> Music
aspectsToMusic = fmap g
  where
    g x = set part' r $ set articulation' a $ set dynamic' d $ fromPitch p
      where
        p = x^?!pitches
        d = x^.dynamic
        a = x^.articulation
        r = x^.part

instance Inspectable Asp where
  inspectableToMusic = aspectsToMusic
instance Inspectable (Voice (Maybe Pitch)) where
  inspectableToMusic = inspectableToMusic . renderAlignedVoice . aligned 0 0
instance Inspectable (Score (Maybe Pitch)) where
  inspectableToMusic = inspectableToMusic . mcatMaybes
instance Inspectable (Score (DynamicT Dynamics Pitch)) where
  inspectableToMusic = inspectableToMusic . toAspects . fmap (PartT . pure . ArticulationT . pure)

--
--
-- -- >>> get "full" :: S (Score Pitch)
-- type S a = IO (Maybe a)
--
-- type Rhythm = Voice ()
-- type Melody = Voice (Maybe Pitch)
-- type Put a = Database.Stash.Key -> a -> IO ()
-- type Get a = Database.Stash.Key -> IO (Maybe a)
-- type Accompaniment = Score Pitch
-- listRhythms        = listR "^rh"
-- listMelodies       = listR "^mel"
-- listAccompaniments = listR "^accomp"
-- listExamples       = listR "^ex"
-- listRhythms'       = openLilypond' LyScoreFormat =<< (fmap rcat $ withStashR (\k -> (fmap (text (show k)) . get_) $ fst k) "^rh")
-- listMelodies'      = display =<< (fmap rcat $ withStashR (\k -> (fmap (text (show k)) . get_) $ fst k) "^mel")
-- listAccompaniments'= display =<< (fmap rcat $ withStashR (\k -> (fmap (text (show k)) . get_) $ fst k) "^accomp")
-- getExample         = getAsp             :: Get Music
-- getRhythm          = Database.Stash.get :: Get Rhythm
-- getMelody          = Database.Stash.get :: Get Melody
-- getAccompaniment   = Database.Stash.get :: Get Accompaniment
--
-- putExample         = putAsp             :: Put Music
-- putRhythm          = Database.Stash.put :: Put Rhythm
-- putMelody          = Database.Stash.put :: Put Melody
-- putAccompaniment   = Database.Stash.put :: Put Accompaniment

{-
Heuristic voice-separation

First part tries to split the score into a list/tree? of Overlap-Free Chunks (aligned voices)
Second part tries to merge chunks that doesn't overlap based on register/continuity/whatever
  It may require us to break up OFCs if their register separation is too large

1) SEPARATION

a) Split score at all onset/offset points that does NOT overlap with another note (split-safe points)
b) Notes that stretch for the WHOLE of this becomes a separate OFC
   Always split OFS that are to registrally wide (i.e. larger gaps than an octave)
c) If there are more SSPs, recurse
   If there are no more SSPs, we have a PHASED-OVERLAP, handle separately as below
      Take *lowest* note, grab all other notes within some range (octave?) that doesn't overlap with it, this becomes an OFC
      Recur to SSP check (c)

2) MERGING
  Grab first OFC, find a later chunks within the max register gap (median) – if many possibilities choose 1 – merge with this
  Repeat until no more OFCs
-}

{-
Simpler version:
  Just take out all eras (with one or more notes)
  Grab era with lowest median, search forward for non-overl chords within the range limit
    Store multiple choices (backtracking logic monad or similar)
  Repeat until all notes partitioned

-}

-- Ord instance is used to compare "pitch", which may affect separation

data HVSConf a = HVSConf {
  _hvsPitchCompare        ::  (a -> a -> Ordering), -- function to compare pitches
  _hvsMaxRegisterGapInOFC :: Diff a
  }
defHVSConf :: HVSConf Pitch
defHVSConf = HVSConf compare _M9

hvs :: HVSConf a -> Score a -> [Aligned (Voice a)]
hvs c x = undefined
--   where
--     -- xx :: [(Aligned (Voice [a]), ())]
--     xx = fmap (\e -> processNonOLChunk e $ sepScore e x) nonOverlappingChunkEras
--
--     processNonOLChunk :: Span -> Score a -> (Aligned (Voice [a]), ())
--     processNonOLChunk e x = let (ok, rest) = partitionEventsSpanningExactly e x
--       in (avAtSpan e ok, undefined)
--     -- TODO break up the OFC based on register
--     nonOverlappingChunkEras :: [Span]
--     nonOverlappingChunkEras = getNonOverlappingChunks (x^.era) (splitSafePoints (x^..eras))
--
-- avAtSpan :: Span -> a -> Aligned (Voice a)
-- avAtSpan e x = set era e $ aligned 0 0 $ pure x
--
-- -- TODO from splitSafePoints ([Time]), get a list of all intermediate [Span] (then check input extremes to generate spans for begin and end)
-- getNonOverlappingChunks :: {-extremes-}Span -> [Time] -> [Span]
-- getNonOverlappingChunks = undefined
--
-- -- All notes that touches but does not cross edges of the span and are not outside
-- sepScore :: Span -> Score a -> Score a
-- sepScore = undefined
--
-- partitionEventsSpanningExactly :: Span -> Score a -> ([a], Score a)
-- partitionEventsSpanningExactly = undefined
--
-- splitSafePoints :: [Span] -> [Time]
-- splitSafePoints xs = Data.List.sort $ Data.List.nub $ filter (\p -> not $ any (p `strictlyInside`) xs) allPoints
--   where
--     allPoints :: [Time]
--     allPoints = fmap (^.onset) xs ++ fmap (^.offset) xs
--





-- goldenRatio = 1.61803398875

-- doveTail2 :: Pitch -> Pitch -> Music
-- doveTail2 x y = replacePitches [(g,x),(b,y)] (System.IO.Unsafe.unsafePerformIO (get_ "ex/orchestration/dt3-4"))
--
-- doveTail3 :: Pitch -> Pitch -> Pitch -> Music
-- doveTail3 x y z = replacePitches [(g,x),(b,y),(d',z)] (System.IO.Unsafe.unsafePerformIO (get_ "ex/orchestration/dt3-4"))
--

--
-- copy :: Database.Stash.Stashable dummy => dummy -> Database.Stash.Key -> Database.Stash.Key  -> IO ()
-- copy typeEx src dst = Database.Stash.get src >>= Database.Stash.put dst . fmap (`asTypeOf` typeEx)
--
-- update :: Database.Stash.Stashable a => (a -> a) -> Database.Stash.Key -> IO ()
-- update f k = do
--   v <- Database.Stash.get k
--   case v of
--     Nothing -> return ()
--     Just x  -> Database.Stash.put k (f x)
--
--



instance AdditiveGroup Decibel where
  zeroV   = 0
  negateV = negate
  (^+^) = (+)
instance VectorSpace Decibel where
  type Scalar Decibel = Decibel
  (*^) = (*)
instance AffineSpace Decibel where
  type Diff Decibel = Decibel
  (.+^) = (+)
  (.-.) = (-)
type instance Music.Score.Dynamic Decibel = Decibel
type instance SetDynamic a Decibel = a
instance Transformable Decibel where transform _ = id
instance HasDynamics Decibel Decibel where dynamics = id
instance HasDynamic Decibel Decibel where dynamic = id

instance AdditiveGroup Amplitude where
  zeroV   = 0
  negateV = negate
  (^+^) = (+)
instance VectorSpace Amplitude where
  type Scalar Amplitude = Amplitude
  (*^) = (*)
instance AffineSpace Amplitude where
  type Diff Amplitude = Amplitude
  (.+^) = (+)
  (.-.) = (-)
type instance Music.Score.Dynamic Amplitude = Amplitude
type instance SetDynamic a Amplitude = a
instance Transformable Amplitude where transform _ = id
instance HasDynamics Amplitude Amplitude where dynamics = id
instance HasDynamic Amplitude Amplitude where dynamic = id





type Label = Int
newtype Choose a = Choose { runChoose :: [a] } deriving (Functor, Applicative, Monad, Show)
choose = Choose
type BusyMap = Map Label Time

-- separateVoices_test = do
--   let n = 2
--   choices <- runChoose <$> separateVoices n <$> captureSibelius
--   forM_ choices $ \choice -> do
--     putStr "display $ fmap asScore $ ["
--     mapM_ (putStr.("  "++).(++",").show) $ {-fmap (toListOf pitches')-} choice
--     putStr "  mempty]\n\n"
--
--   putStrLn ("Number of voices: "++ show n)
--   putStrLn (("Number of alternatives: "++) $ show $ length $ choices)

  -- mapM_ print . fmap (toListOf pitches') . firstChoice =<< f6 [1,2] <$> captureSibelius

-- | Return all possible separations (in no particular order) of the given score into n voices.
--   If the given score can not be separated, returns mzero.
--   Note that the complexity for this implemntation grows significantly for n > 3.
separateVoices :: Int -> Score a -> Choose [Score a]
separateVoices n = separateVoices_6 [1..n]

separateVoices_6 :: [Label] -> Score b -> Choose [Score b]
separateVoices_6 ls sc = do
  sc2 <- separateVoices_2 ls sc
  return $ fmap (\n -> mmapMaybe (\(l,x) -> if l == n then Just x else Nothing) sc2) ls

separateVoices_2 :: [Label] -> Score a -> Choose (Score (Label, a))
separateVoices_2 ls sc = set meta (view meta sc) <$> (view score) <$> separateVoices_1 ls (view events sc)

separateVoices_1 :: [Label] -> [Event a] -> Choose [Event (Label, a)]
separateVoices_1 allLabels = fmap snd . mapAccumM (pickLabel allLabels) mempty
  where
    -- Could also be done with state monad (how?)
    mapAccumM :: (Monad m, Functor m) => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
    mapAccumM f a l = swap <$> runStateT (mapM go l) a
        where
            swap (a,b) = (b,a)
            go i = do
                s <- Control.Monad.State.get
                (s', r) <- Control.Monad.State.lift $ f s i
                Control.Monad.State.put s'
                return r

    -- Choose any available label with backtracking inside Choose
    pickLabel :: [Label] -> BusyMap -> Event a -> Choose (BusyMap, Event (Label, a))
    pickLabel allLabels busyMap event = do
      -- Only use labels that become available before the current onset
      let badLabels = (Data.Map.keys $ Data.Map.filter (> event^.onset) busyMap)
      -- The value we pick is inside Choose which means all alternatives will be propagated to the top level
      -- If there are no options, the result will me empty
      label <- choose (allLabels Data.List.\\ badLabels)
      -- The chosen label is now unavailable until the offset of this event
      let newBusy = Data.Map.insert label (event^.offset) busyMap
      return (newBusy, (fmap (label,) event))


-- TODO move
instance Inspectable [Music] where
  inspectableToMusic = rcat . fmap inspectableToMusic

instance Inspectable [Duration] where
  inspectableToMusic = inspectableToMusic . view rhythm


-- TODO name?
filterEventsInSpan :: Span -> Score a -> Score a
filterEventsInSpan sp = filterWithSpan (\sp2 _ -> sp `overlaps` sp2)


{-
Possibly unrelated idea: masking functions.
  https://helpx.adobe.com/illustrator/using/clipping-masks.html

  TODO how to handle overlapping
  Which arguments determine output era?
  Exhaust these oppertunities?
  mask :: (a -> c) -> (b -> c) -> (a -> b -> c) -> Event a -> Event b -> Event c

-}

{-
  Melody as a tree of voices
    Each being offset by t (generally t < 0, i.e. for pickups)
    Onset for each phrase/phrase group is separated by s

  Can (always) be rendered to a score.
  Can also be rendered to a (Voice (Maybe a)), assuming phrases don't overflow their alloted durations.
    How to check this?
  Alternatively: Can be renderered to a voice, by multiplying offsets until no overflow occurs.

  I.e. a typical classical phrase-group:
    (4 (2 (-1/4 a) (-1/4 a)) (-1/4 b))
-}
data MTree s t a = Branch s [MTree s t a] | Leaf t a

instance (Transformable t, Transformable s, Transformable a) => Transformable (MTree t s a) where
  transform t' (Leaf t x)   = Leaf (transform t' t) (transform t' x)
  transform t (Branch s xs) = Branch (transform t s) (fmap (transform t) xs)

renderMel :: MTree Duration Duration Melody -> Score Pitch
renderMel xs = mcatMaybes $ ppar $ fmap renderAlignedVoice $ go xs
  where
    go (Leaf t x)    = [aligned (0.+^t) 0 x]
    -- TODO add check that that phrases are of an appropriate length (i.e. returned values should not overlap)
    go (Branch s xs) = mapWithIndex (\n x -> delay (s^*fromIntegral n) x) $ concatMap go xs -- todo offset each by s



instance Inspectable a => Inspectable (Either b a) where
  inspectableToMusic (Left _) = mempty
  inspectableToMusic (Right x) = inspectableToMusic x
