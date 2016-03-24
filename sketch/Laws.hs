

-- transform is a monoid morphism
Transformable laws
=====
transform mempty   = id
transform (s <> t) = transform s . transform t
  [@transform@ is a monoid homomorphism, mapping @Span@ to @Endo a@]


HasPosition laws
=====
onset (delay n a)       = n ^+. onset a
offset (delay n a)      = n ^+. offset a
duration (stretch n a)  = n * duration a
duration (compress n a) = duration a / n

-- More generally
position p (transform s a) = transform s (position p a)
-- or equivalently:
position p . transform s = transform s . position p


Duration/Position laws
=====
-- Duration and era
duration a = offset - onset a
-- or
duration a = duration (era a)


Reverse laws
=====
rev (rev a) = a
  [rev is an involution]
duration a   = duration (rev a)
  [rev does not affect duration]
[ALT WEAKER]
abs (duration a) = abs (duration (rev a))
  [rev does not affect absolute duration]
[POSSIBLE]
position p a = position p (rev a)
[POSSIBLE]
transform (rev s) = rev . (transform s)


Split laws
=====
1) duration (take t a) + duration (drop t a) = duration a
2) duration (take t a) = t `min` duration a
3) duration (drop t a) = duration a - (t `min` duration a)

Any of these three laws can be derived from the other two.
3 from 1,2)
    duration (take t a) + duration (drop t a) = duration a
    duration (drop t a) = duration a -  duration (take t a)
    duration (drop t a) = duration a - t `min` duration a
2 from 1,3)
    duration (take t a) + duration (drop t a) = duration a
    duration (take t a) = duration a - duration (drop t a)
    duration (take t a) = duration a - (duration a - (t `min` duration a))
    duration (take t a) = duration a - duration a + (t `min` duration a)
    duration (take t a) = t `min` duration a
1 from 2,3)
    duration (drop t a) = duration a - (t `min` duration a)
    duration (drop t a) = duration a - duration (take t a)
    duration (take t a) + duration (drop t a) = duration a

Generalizes list laws:
1) length (take t a) + length (drop t a) = length a
2) length (take t a) = t `min` length a
3) length (drop t a) = length a - (t `min` length a)






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
