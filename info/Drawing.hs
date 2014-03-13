

{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ConstraintKinds #-}

import Diagrams.Prelude hiding (Time, Duration, (|>), stretch)
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

showOr = (<> (fc red $ circle 0.5))
-- showOr = id




main2 = openG $ showOr $ (<> bh) $ (<> gridY) $ drawScore . fmap (fmap toSemi) . extractParts $ score1
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



foo, bar :: Behavior Double
foo = varying $ sin . (/ 2) . (* tau) . realToFrac
-- bar = varying $ sin . (/ 10) . (* tau) . realToFrac
bar = delay 5 $ switchB 0 0 1

main = openG $ (<> grid) $ 
    translateY (5)  (D.text "foo" <> drawBehavior' 10 foo & lc red)
    <>
    translateY (0)  (D.text "bar" <> drawBehavior' 10 bar & lc green)
    <>
    translateY (-5) (D.text "foo*bar" <> drawBehavior' 10 (foo*bar) & lc blue)






grid = grid'   20
gridX = gridX' 20
gridY = gridY' 20

grid' ds = showOr $ moveOriginTo (p2 (realToFrac ds/2,-(realToFrac ds/2))) $ (gridX <> gridY & lc lightblue)

gridY' :: (Renderable (Path R2) b) => Int -> Diagram b R2
gridY' ds = alignTL $ hcat' (def & sep .~ 1) $ replicate (ds+1) $ vrule (realToFrac ds)

gridX' :: (Renderable (Path R2) b) => Int -> Diagram b R2
gridX' ds = alignTL $ vcat' (def & sep .~ 1) $ replicate (ds+1) $ hrule (realToFrac ds)

drawBehavior :: (Renderable (Path R2) b, Real a) =>  Behavior a -> Diagram b R2
drawBehavior = drawBehavior' 10 

drawBehavior' count b = cubicSpline False points & lw 0.1
    where
        points = take (samplesPerCell*count) $ fmap (\x -> p2 (x, realToFrac $ b ? realToFrac x)) [0,1/samplesPerCell..]
        samplesPerCell = 10

drawScore :: (Renderable (Path R2) b, Real a) =>        [Score a] -> Diagram b R2
drawScore = vcat' (def & sep .~ 2) . fmap drawPart

drawPart :: (Renderable (Path R2) b, Real a) =>         Score a -> Diagram b R2
drawPart = drawPart' . (^. events)

drawScore' :: (Renderable (Path R2) b, Real a) =>       [[(Time, Duration, a)]] -> Diagram b R2
drawScore' = vcat' (def & sep .~ 2) . fmap drawPart'

drawPart' :: (Renderable (Path R2) b, Real a) =>        [(Time, Duration, a)] -> Diagram b R2
drawPart' = (<> alignL (hrule 20)) . alignTL . (<> alignL (hrule 20)) . alignBL . scaleX 1{-TODO-} . mconcat . 
    fmap drawScoreNote . 
    fmap (map1 timeToDouble . map2 durationToDouble . map3 realToFrac)
    where
        map1 f (a,b,c) = (f a,b,c)
        map2 f (a,b,c) = (a,f b,c)
        map3 f (a,b,c) = (a,b,f c)
        drawScoreNote (t,d,x) = translateY x $ translateX (t.+^(d^/2)) $ scaleX d $ noteShape
        noteShape = lcA transparent $ fcA (blue `withOpacity` 0.5) $ square 1

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

