
import Music.Prelude

{-
Develop tools for basic counterpoint techniques.

- Presentations (offset in time and pitch)
- Transformations
- Inversion, augment/dimin etc.

Try to encode the form of classical fugues in table form, see for example
  http://www2.nau.edu/tas3/introaof.html

-}
subj :: Music
subj = scat [a_|*2,c,d,e|*3,g,
 a|*2,g,a,e|*3,e,
 d|*4,e,a_,b_,c,d]

fugue :: [(Part, Duration, Interval)] -> Music -> Music
fugue exps subj = pcat $ fmap (\(p,t,i) -> set parts' p . delay t . up i $ subj) exps

fugue1 = fugue
  [ (p1, 0, _P1)
  , (p2, 1*4, _P5)
  , (p3, 3*4, _P8)
  , (p4, 4*4, -_P8)

  , (p1, 9+0*4, _P8)
  , (p2, 9+1*4, _P5)
  , (p3, 9+3*4, _P1)
  , (p4, 9+4*4, -_P8)
  
  , (p1, 14+0*4, _P8)
  , (p2, 14+2*4, -_P8)
  , (p3, 14+3*4, _P5)
  , (p4, 14+4*4, _P1)
  
  , (p1, 20+0*4, _P5)
  , (p2, 20+2*4, _P8)
  , (p3, 20+3*4, _P8+_P5)
  , (p4, 20+4*4, _P1)   
  ]
  where [p1,p2,p3,p4] = divide 4 violins

{-
TODO
  - Similar for double/triple fugues etc
  - Functions to get distance between expositions and so on
  - Terminology for the various combinations of subjects (_P4 above delayed 2 etc)
    - Given a subject, list allowed permutations (requires checking obviously)
    - Or: just enumerate the permutations to allow for manual checking
-}

fugue1a  = fugue1 $ scat [c,cs] |> compress 4 (scat [d,b,bb,a] |> scat [gs,cs,d,ds])
fugue1a' = fugue1 $ scat [c,cs] |> compress 4 (scat [d,b,bb,b] |> scat [gs,cs,d,ds])


fugue1b = fugue1 $ scat [c,e] |> compress 4 (scat [f,a,g,a] |> scat [bb,a,g,f])
fugue1c = fugue1 $ scat [c,d,b] |> compress 8 (scat [e,d,e,d,e,g,a,e,d])
fugue1d = fugue1 $ compress 8 (scat 
  [e,d,c|*2,f|*2,e,f,g,b,a|*2]
  )

-- A nice 12-tone style fugue
fugueX = fugue1 $ subjX

subjX =
  [(0<->(1/4),c)^.event,((1/4)<->(1/2),bb)^.event,((1/2)<->(3/4),cs)^.event,((3/4)<->1,a)^.event,(1<->(5/4),d)^.event,((5/4)
  <->(11/8),gs)^.event,((11/8)<->(3/2),g)^.event,((3/2)<->(13/8),g)^.event,((13/8)<->(27/16),f)^.event,((27/16)<->
  (7/4),e)^.event,((7/4)<->2,fs)^.event,(2<->(17/8),fs)^.event,((17/8)<->(35/16),e)^.event,((35/16)<->(9/4),ds)^.event,((9/4)
  <->(5/2),es)^.event,((5/2)<->(21/8),es)^.event,((21/8)<->(43/16),eb)^.event,((43/16)<->(11/4),d)^.event,((11/4)<->
  (23/8),e)^.event,((23/8)<->3,b)^.event,(3<->(25/8),as)^.event,((25/8)<->(13/4),a)^.event,((13/4)<->(27/8),a)^.event,((27/8)
  <->(55/16),g)^.event,((55/16)<->(7/2),fs)^.event,((7/2)<->(15/4),gs)^.event,((15/4)<->(31/8),gs)^.event,((31/8)<->
  (63/16),fs)^.event,((63/16)<->4,es)^.event,(4<->(17/4),g)^.event,((17/4)<->(35/8),g)^.event,((35/8)<->
  (71/16),f)^.event,((71/16)<->(9/2),e)^.event]^.score

