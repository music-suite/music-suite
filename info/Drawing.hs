

{-# LANGUAGE CPP, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ConstraintKinds, ViewPatterns #-}

import Diagrams.Prelude hiding (Time, Duration, (|>), (<->), stretch)
import qualified Diagrams.Backend.SVG as SVG

-- test       
import Music.Prelude.Basic hiding (pitch)
import qualified Music.Pitch as P
import qualified Music.Score as S
import qualified Diagrams.Prelude as D


import Data.Default
import Data.VectorSpace
import Data.AffineSpace
import Control.Lens hiding ((|>))
import System.Process (system)
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as ByteString

instance Eq a => Eq (Behavior a) where
    (==) = error "No (==)"
instance Ord a => Ord (Behavior a) where
    (<) = error "No (<)"
    min = liftA2 min
    max = liftA2 max

-- TODO
type instance S.Pitch (ChordT a) = S.Pitch a
instance HasGetPitch a => HasGetPitch (ChordT a) where
    __getPitch (ChordT xs) = __getPitch $ head $ xs

timeToDouble :: Time -> Double
timeToDouble = realToFrac . (.-. origin)
durationToDouble :: Duration -> Double
durationToDouble = realToFrac
pitchToDouble :: P.Pitch -> Double
pitchToDouble = realToFrac . semitones . (.-. c)

showOr = (<> (setEnvelope mempty $ fc red $ circle 0.05))
-- showOr = id




main2 = openG $ {- showOr $ -} (<> bh) $ (<> gridY) $ drawScore . fmap (fmap toSemi) . extractParts $ score1
    where                           
        bh = drawBehavior (fmap (sin.(*(tau/5)).realToFrac) $ varying id)

toSemi = (semitones.(.-. c).__getPitch)

score1 :: Score BasicNote
score1 = compress 4 $ rcat [
    up _P5 $ stretch 2 $ scat $ fmap motive $ [1..5],
    up _M3 $ stretch 3 $ scat $ fmap motive $ [1..4],
    up _P1 $ stretch 4 $ scat $ fmap motive $ [1..3]
    ]
    where
        motive n = legato $ {-pitches %~ id $-} times (n+1) (scat [c..d]) |> times n (scat [d,f,e,ds])


timeB = varying $ realToFrac
sine fq = varying $ sin . (* fq) . (* tau) . realToFrac
fadeOut t = ((1-timeB/t) `max` 0) `min` 1
fadeIn t = ((timeB/t) `min` 1) `max` 0

foo, bar :: Behavior Double                 
foo = sapp (2 <-> 5) $ compress 5 $ fadeIn 2*delay 3 (fadeOut 2) * sine (1/5)*sine(3)*delay 1 (sine 2)

-- bar = varying $ sin . (/ 10) . (* tau) . realToFrac
-- bar = delay 1 $ switchB 0 0 1
bar = sapp (4 <-> 6) $ compress 5 $ fadeIn 2*delay 3 (fadeOut 2) * sine (1/5)*sine(3)*delay 1 (sine 2)

main = openG $ (<> grid) $ 
    drawPart (fmap toSemi $ asScore $ scat [c,d,e^*2] |> scat [c,d,e^*2] |> scat [d,d,d,d,c^*2,c^*2])
    <>
    translateY (5)  (D.text "foo" <> drawBehavior' 10 foo & lc red)
    <>
    translateY (0)  (D.text "bar" <> drawBehavior' 10 bar & lc green)
    <>
#define LAST(EXPR) translateY (-5) (D.text "EXPR" <> drawBehavior' 10 (EXPR) & lc blue)
    LAST(foo+bar)







grid = grid'   20
gridX = gridX' 20
gridY = gridY' 20

grid' ds = {-showOr $ -}moveOriginTo (p2 (realToFrac ds/2,-(realToFrac ds/2))) $ (gridX <> gridY & lc lightblue)

gridY' :: (Renderable (Path R2) b) => Int -> Diagram b R2
gridY' ds = alignTL $ hcat' (def & sep .~ 1) $ replicate (ds+1) $ vrule (realToFrac ds)

gridX' :: (Renderable (Path R2) b) => Int -> Diagram b R2
gridX' ds = alignTL $ vcat' (def & sep .~ 1) $ replicate (ds+1) $ hrule (realToFrac ds)



drawBehavior :: (Renderable (Path R2) b, Real a) =>  Behavior a -> Diagram b R2
drawBehavior = drawBehavior' 10 

drawBehavior' count b = cubicSpline False points & lw 0.05
    where
        points = take (samplesPerCell*count) $ fmap (\x -> p2 (x, realToFrac $ b ? realToFrac x)) [0,1/samplesPerCell..]
        samplesPerCell = 40

drawScore :: (Renderable (Path R2) b, Real a) =>        [Score a] -> Diagram b R2
drawScore = vcat' (def & sep .~ 2) . fmap drawPart

drawPart :: (Renderable (Path R2) b, Real a) =>         Score a -> Diagram b R2
drawPart = drawPart' . (^. events)

drawScore' :: (Renderable (Path R2) b, Real a) =>       [[(Time, Duration, a)]] -> Diagram b R2
drawScore' = vcat' (def & sep .~ 2) . fmap drawPart'

drawPart' :: (Renderable (Path R2) b, Real a) =>        [(Time, Duration, a)] -> Diagram b R2
drawPart' = mconcat . fmap drawNote'

drawNote' :: (Renderable (Path R2) b, Real a) => (Time, Duration, a) -> Diagram b R2
drawNote' (timeToDouble -> t, realToFrac -> d, realToFrac -> y) = translateY y $ translateX t $ scaleX d $ noteShape
    where
    noteShape = {-showOr $-} lcA transparent $ fcA (blue `withOpacity` 0.5) $ strokeLoop $ closeLine $ fromOffsets [r2 (1,0), r2 (-0.8,0.2), r2 (-0.2,0.8)]

writeG :: (a ~ SVG.SVG) => FilePath -> Diagram a R2 -> IO ()
writeG path dia = do
    let svg = renderDia SVG.SVG (SVG.SVGOptions (Height 300) Nothing) dia
    let bs  = renderSvg svg
    ByteString.writeFile path bs
        
openG :: (a ~ SVG.SVG) => Diagram a R2 -> IO ()
openG dia = do
    writeG "test.svg" $ dia -- 
    -- FIXME find best reader
    system "echo '<img src=\"test.svg\"></img>' > test.html"
    -- system "open -a 'Firefox' test.html"
    system "osascript -e 'tell application \"Google Chrome\" to tell the active tab of its first window' -e 'reload' -e 'end tell'"
    return ()

