

import Prelude hiding (splitAt)
import Music.Prelude
import Control.Lens.Operators

main :: IO ()
main = do
  -- TODO Use standard test framework
  unless testGetCycles $ error "Failed testGetCycles"
  unless testRenderLungaNEW $ error "Failed testRenderLungaNEW"




--
testSplitAt :: Bool
testSplitAt = all id
  [ splitAt (-1) ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([]^.voice, [(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
--
  , splitAt 0 ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([]^.voice, [(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
--
  , splitAt 0.5 ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([((0.5),'a')^.note]^.voice, [((0.5),'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
--
  , splitAt 2.5 ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([(1,'a')^.note,((1.5),'b')^.note]^.voice, [((0.5),'b')^.note,(1,'c')^.note]^.voice)
--
  , splitAt 5 ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice,[]^.voice)
  ]




-- TODO proper test
testGetCycles :: Bool
testGetCycles = all id
  -- At least one cycle
  [ getCycles (0 <-> 2) (0 <-> 4)         == Right (0, 2, 0)
  , getCycles (0 <-> 2) (1 <-> 4)         == Right (1, 1, 0)
  , getCycles (0 <-> 2) ((-1) <-> 5)      == Right (1, 2, 1)
  , getCycles (0 <-> 2) ((-9) <-> (-2.5)) == Right (1, 2, 1.5)
  , getCycles (0 <-> 2) (2 <-> 4)         == Right (0, 1, 0)
-- No full cycles, s properly enclosed by a cycle
  , getCycles (0 <-> 2) (0.5 <-> 1.5)     == Left  (0.5)
  , getCycles (0 <-> 2) (0.6 <-> 1.6)     == Left  (0.6)
  , getCycles (0 <-> 2) ((-0.2) <-> (-0.1)) == Left (1.8)
-- No full cycles, s NOT properly enclosed by a cycle
  , getCycles (1 <-> 3) (1 <-> 2)         == Right (0, 0, 1)
  , getCycles (1 <-> 3) (2 <-> 3)         == Right (1, 0, 0)
  , getCycles (1 <-> 3) (0 <-> 1)         == Right (1, 0, 0)
  , getCycles (1 <-> 3) ((-1) <-> 0)      == Right (0, 0, 1)
  ]
--
testC :: Span -> Aligned (Voice a) -> Either Duration (Duration, Integer, Duration)
testC s l = getCycles (_era1 l) s
--
testRenderLungaNEW :: Bool
testRenderLungaNEW = all id
  [ renderLunga (0 <-> 2) aba
  == [(0 <-> 1,'a')^.event,(1 <-> 2,'b')^.event]^.score
--
  , renderLunga (0 <-> 4) aba
  == [(0 <-> 1,'a')^.event,(1 <-> 3,'b')^.event,(3 <-> 4,'c')^.event]^.score
--
  , renderLunga (0 <-> 1) aba
  == [(0 <-> 1,'a')^.event]^.score
--
  , renderLunga ((-1)<->1) aba
  == [(-1 <-> 0,'c')^.event,(0 <-> 1,'a')^.event]^.score
--
  , renderLunga ((0)<->1) aba1
  == [(0 <-> 1,'c')^.event]^.score
--
  , renderLunga ((0.5)<->1.5) (aligned 0 0 $ stretch 2 $ pure 'c')
  == [(0.5 <-> 1.5,'c')^.event]^.score
--
  , renderLunga (3<->8) aba
  == [(3 <-> 4,'c')^.event,(4 <-> 5,'a')^.event,(5 <-> 7,'b')^.event,(7 <-> 8,'c')^.event]^.score
--
  , renderLunga (0<->2) (aligned 0 0 c :: Aligned (Voice Int))
  == [(0 <-> 1,0)^.event,(1 <-> 2,0)^.event]^.score
--
  , renderLunga (0<->2) (stretch 2 $ aligned 0 0 c :: Aligned (Voice Int))
  == [(0 <-> 2,0)^.event]^.score
--
  , renderLunga (0<->2) (delay 0.3 $ stretch 0.9 $ aligned 0 0 c :: Aligned (Voice Int))
  == [(0 <-> (3/10),0)^.event,((3/10) <-> (6/5),0)^.event,((6/5) <-> 2,0)^.event]^.score
--
  , renderLunga ((-4) <-> 0) aba
  == [((-4) <-> (-3),'a')^.event,((-3) <-> (-1),'b')^.event,((-1) <-> 0,'c')^.event]^.score
  ]
  where
    aba =
      (aligned 0 0 $ [(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    aba1 =
      (aligned 1 0 $ [(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
--

