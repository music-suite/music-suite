

-- transform is a monoid morphism
transform mempty   = id
transform (s <> t) = transform s . transform t




onset (delay n a)       = n ^+. onset a
offset (delay n a)      = n ^+. offset a
duration (stretch n a)  = n * duration a
duration (compress n a) = duration a / n

-- More generally
position p (transform s a) = transform s (position p a)
-- or equivalently:
position p . transform s = transform s . position p

-- This does not hold for Score at the moment:
>> [(0<->1,())^.note]
[(0 <-> 1,())^.note]
>>> [(0<->1,())^.note, (1<->2,())^.note]
[(0 <-> 1,())^.note,(1 <-> 2,())^.note]
>>> [(0<->1,())^.note, (1<->2,())^.note]^.score
[(0 <-> 1,())^.note,(1 <-> 2,())^.note]^.score
>>> 
>>> [(0<->1,())^.note, (1<->2,())^.note]^.score
[(0 <-> 1,())^.note,(1 <-> 2,())^.note]^.score
>>> [(0<->1,())^.note, (1<->2,())^.note]^.score.era
0 <-> 2
>>> 
>>> (stretch (-1) $ [(0<->1,())^.note, (1<->2,())^.note]^.score)
[(-1 <-> -2,())^.note,(0 <-> -1,())^.note]^.score
>>> (stretch (-1) $ [(0<->1,())^.note, (1<->2,())^.note]^.score)^.era
-2 <-> 0
>>> stretch (-1) (0 <-> 2)
0 <-> -2





-- Duration and era
duration a = offset - onset a
-- or
duration a = duration (era a)