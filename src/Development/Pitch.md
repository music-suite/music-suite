

There are some basic design choices particular to music-pitch which I would like to retain:

- We allow intervals to be negative. This is necessary for the VectorSpace/AffineSpace instances to behave, and also makes intervals more mathematically tractable. (To make pitches a proper AffineSpace we must be able to pick any frequency as the origin.)

- All "literals", i.e. (c, cs, m3, _M3 etc) should be overloaded using the classes from 'music-pitch-literal'. This is because programs and musical compositions written using the suite should not be forced to use a particular representation. Thanks to the strength of type classes all such values can be overloaded.

- The pitch representation should be as generic as possible, and all constructs specific to Common/Western music theory should clearly be labeled as such. Most operations should be done in terms of standard type classes such as Semigroup, Monoid, VectorSpace and AffineSpace. 

Then there are other points where I am still undecided. For example I there is the concept of related scales such as the chromatic and diatonic scales (the Semitones and Spelling modules). I feel that this concept is not limited to "semitones" (a purely Western concept) but is echoed in other musical traditions as well, whenever you have two scales types with overlapping steps. However I am not sure how to abstract over it. 

Also I would like to make the representation more generic overall: many things in the 'Music.Pitch.Common...' could be generalized and moved upward  in the hierarchy. 
