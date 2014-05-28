
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

## 1.7.0

* Adds many new time-based types:
    * Separates time- and score-types in in `Music.Time` and `Music.Score` hierarchy
    * Adds `Delayed`, `Stretched` and `Bound`
    * Adds `Segment` for behaviors without a position
    * Improves interface to `Behavior` (and `Segment`)
    * Adds `Chord`
    * Adds laws for, `Transformable`, `Reversible` and `Splittable`
    
* Completely new interface to parts, pitches, dynamics and articulations supporting
  polymorphic updates of all parameters.

* Adds colored noteheads

* Adds `.music` file parsing and conversion: `music2ly`, `music2musicxml` etc.

* Adds phrase-wise traversals (experimental feature)

* Adds regression tests

* Adds more examples

* Garbage-collect API:
    * Merges classes `Delayable` and `Stretchable` into `Transformable`
    * Merges classes `HasOnset` and `HasOffset` etc. into `HasPosition`
    * Proper implementation of `Span`, as a real abelian group.
    * Changes API to use lenses more thoroughly

