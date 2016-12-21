


module Main where

import System.Process
import Diagrams.Prelude
import Graphics.SVGFonts.ReadFont
import qualified Diagrams.Backend.SVG as SVG
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy         as ByteString

main = openG $ logo -- <> circle 10 <> allGlyphs

allGlyphs = translateY 20 $ text' "\xE0A4 \xE0D6 \x1D160 \xE1F4 \xE246 \xE8C8 \xE8C9 \xE8CA \xE8CB"

logo = mempty
    <> translate (r2 (2.5,1.8)) (scale 1.8    eightNote) 
    -- <> translate (r2 (5.5,1.8)) (scale 1.8 (scale (-1) eightNote))
    <> trig 
    <> translate (r2 (-2,4)) (scaleY 3 $ scaleX 0.45 $ sign) 
    <> (translateX 1.5 $ translateY 2.75 $ lc crimson $ lw 0.1 $ hrule 10)
    <> (translateX 1.5 $ translateY 1.75 $ lc crimson $ lw 0.1 $ hrule 10)
    <> (translateX 1.5 $ translateY 0.75 $ lc crimson $ lw 0.1 $ hrule 10)
    <> (fc white $ strut 25) 

sign = lc steelblue $ drawBehavior (\t -> sin ((t*tau)/2) * sin ((t*tau)/10))
eightNote = fc black $ text' "\x1D160"
trig = alignX 0 $ a ||| (alignY 0 $ vcat [b, strutY 0.5, c])
  where
    a = fc black $ text' "\xE8CA"
    b = fc crimson $ lc crimson $ text' "\xE8C8"
    c = fc black $ text' "\xE8C9"


openG dia = do
  writeG "test.svg" $ dia --
  -- FIXME find best reader
  system "echo '<img src=\"test.svg\"></img>' > test.html"
  -- system "open -a 'Firefox' test.html"
  system "osascript -e 'tell application \"Google Chrome\" to tell the active tab of its first window' -e 'reload' -e 'end tell'"
  return ()

writeG :: FilePath -> Diagram SVG.SVG R2 -> IO ()
writeG path dia = do
  let svg = renderDia SVG.SVG (SVG.SVGOptions (Height 300) Nothing) dia
  let bs  = renderSvg svg
  ByteString.writeFile path bs

-- drawBehavior :: (Renderable (Path R2) b, Real a) => (Double -> a) -> Diagram b R2
drawBehavior = drawBehavior' 0 10

drawBehavior' start count b = draw points & lw 0.05 & fc red
  where
    points = take (samplesPerCell*count) $ fmap (\x -> p2 (x, fromVal (b x))) [start,start+1/samplesPerCell..]
    toTime = realToFrac
    fromVal = realToFrac
    samplesPerCell = 90
    -- draw = cubicSpline False
    -- TODO offset without showing
    draw = fromOffsets . (\xs -> zipWith (.-.) (tail xs) xs) . ((p2 (0,0)) :)


text'  t =  fillRule EvenOdd $ stroke (textSVG' $ TextOpts t bravura INSIDE_H KERN False 10 10 ) # fillRule EvenOdd
bravura = outlMap "../music-docs/logo/Bravura.svg"