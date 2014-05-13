
# Music Suite Releases

## 1.3

* Better pitch representation

* Better documentation

* Adds Lilypond backend

## 1.4

(no changes)

## 1.5

(no changes)

## 1.6

* Adds meta-data

* Allow arbitrary bar length

* New type `Span`

* Improved definition of score, voice and other time types using `Span`

* Better part representation

* Adds literals for dynamics and intervals

* Better midi export

* Adds auto-merging of simultaneous notes into chords

* Adds Sibelius import (experimental)

* Adds heuristic voice separation (experimental)

* Examples

## 1.7


* Adds `.music` file parsing and conversion

* Adds many new time-based types

    * Separates time- and score-types in in `Music.Time` and `Music.Score` hierarchy

    * Adds delayed and stretched values

    * Adds segments
    
    * Improves interface to `Behavior` (and `Segment`) in terms of `Representable` from `adjunctions`

    * Adds chord type

    * Adds time-varying pitch, dynamics and articulation

    * Adds laws for `Reversible` and `Splittable`
    
* Garbage-collect API:

    * Merges classes `Delayable` and `Stretchable` into `Transformable`

    * Merges classes `HasOnset` and `HasOffset` etc. into `HasPosition`

    * Proper implementation of `Span`, as a real abelian group.

    * Replaces all ad-hoc conversion functions with lenses, isos and prisms
    
    * Changes API to use lenses more thoroughly, i.e. `place :: (HasPosition a, Transformable a) => Lens' a Span`.

* Adds polymorphic update of pitch, dynamics and articulation

* Adds phrase-wise traversal

* Adds test suite

* Adds more examples
