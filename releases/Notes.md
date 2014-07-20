
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

- Representation: 

    - API cleaning
        - Proper separation of time containers (now in the `Music.Time` hierarchy) and the rest (in `Music.Score`)
        - Merges classes `Delayable` and `Stretchable` into `Transformable`
        - Merges classes `HasOnset` and `HasOffset` etc. into `HasPosition`
        - Proper implementation of `Span`, as a real abelian group.
        - Changes API to use lenses more thoroughly

    - Adds laws for `Transformable`, `Reversible` and `Splittable`

    - `Behavior` and `Reactive` now use `Representable` from adjunctions as the main interface

    - Adds many new time-based containers: 
        - `Segment` for values varying over an unknown time span
        - `Bound` for restricting time boundaries
        - `Delayed` and `Stretched`, representing values in the context of a delay or stretch transformation
        - `Chord` representing center-aligned parallel compositions (in contrast to `Track`, which represents
           left-aligned parallel compositions).

    - Completely new interface to the *aspect classes* (parts, pitches, dynamics and articulations):
        - Now separates classes with a single pitches/dynamic values etc (i.e. notes) from classes with
          many pitches/dynamics etc (i.e. voices, scores).
        - `HasPitch` et al now uses Lenses and Traversals
        - Polymorphic updates of all parameters
        - New functions `fromPitch` to inject pitch etc. into more complex note types

    - Improves representation of dynamics and articulation
    
    - Adds *phrasewise traversals* of scores and voices (experimental)

    - Adds colored noteheads (`ColorT` transformer)

- Backends and notation:

    - NEW BACKEND: SuperCollider

    - NEW BACKEND: Note lists (a trivial example backend)

    - Restructuring of backend code, adding `HasBackendScore` as a way of exporting musical containers generically.

    - Better notation of dynamics and articulation in Lilypond and MusicXML output

- Utility:
    
    - Adds *converter* programs, for converting `.music` files (i.e. files consisting of a single music expression) to other formats:
        - NEW EXECUTABLE: music2ly
        - NEW EXECUTABLE: music2midi
        - NEW EXECUTABLE: music2musicxml
        - NEW EXECUTABLE: music2pdf
        - NEW EXECUTABLE: music2png
        - NEW EXECUTABLE: music2svg

- Adds regression tests

- Adds more examples


# 1.7.1

- Representation: 

  - Adds Cons and Snoc instances for Voice

  - Now uses Average monoid dynamics, not Sum (when merging chords etc)

  - Generalizes meta-data to arbitrary types

- Backends and notation:

  - Fixes errors in Lilypond output when using negative durations

  - Now supports declarations in music files

- Utility:

  - Fixes miscellaneous quantization problems

  - Fix notation of slurs w.r.t tie splitting

  - Renames note type in Standard prelude to StandardNote
  
  - Renames stretchOnly etc to stretchComponent

  - Renames postOnset (the point between onset and offset) to midpoint

  - Removing obsolete function splitTiesVoice

  - Dependencies: `HCodecs ==> 0.5`, `lens ==> 4.3`, `vector-space-points ==> 0.2`

  - Improves documentation and tests


