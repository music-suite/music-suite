

## What?

* Music Representation
    - Composition (*a → Music*)
    - Transformations (*Music → Music*)
    - Analysis (*Music → a*)

*Not* primarily audio or real-time use (but see last slide!).

## type Music

No closed definition of music/musics!

*But* there are many common elements in musics!

  How can we formalize these common qualities?

## type Music

Musical <a style="color:blue">*theory*</a>, concepts such as: scales, chords, rhythm/metrics, harmony, counterpoint

Musical *aspects*: time, pitch, dynamics, timbre (instrument/articulation), (space..)

Music music theories of the word define these, in *roughly* this order of importance

    music-time
    music-pitch
    music-dynamics
    music-articulation
    music-parts
    music-space
    
    music-score
    music-preludes
    ...

## Structure vs. Flatness

Bars/beats vs free time
Scale (structure) vs frequency
Fixed dynamic levels vs amplitude

Discrete and continuos are complimentary!

For each aspect: Start out with affine space and *add* structure

## Time is special!

  Music take place in time!
  All other aspects are *organized in time*

  (Work pioneered by FRP, Reactive, Euterpea...)
  
  Events take place (discrete)
    Once (onset)
      Time
    During a span (onset and offset)
      Time x Duration
      Span
  Events develop (continous)
    Through a local span (Segment)
    Thoughout (Behavior)
  Structure
    Reactive
    Voice
    Bounds
  
## Compare 2D or 3D graphics!

View time as a one-dimensional vector space.

An affine transformation is simply *(Product~Duration~ ⨯ Sum~Time~)*

Isomorphic to *Time^2^*

We get the linar transformations...

## Musical aspects: Pitch...
  Pitch vs Interval (linear)
  High-level concepts
  Using Behavior
  Spelling/intonation etc

## Musical aspects: ...and the rest

dynamics, parts, articulation, space

## Accessing aspects

The overloaded lenses etc

## Meta-information

Dynamic types, using Reactives a lot (not needed)


## Design principles

- Extensible
    - Only make some *core* assumptions (time is special, other aspect *fetch onto* time)
    - User can change both events and time containers

- Comprehensive, not complete
    - Impossible to include *all* music theories!
    - CMT included
    - Easily extendible

- Use the "latest"
    - MPTCs, TypeFamilies, ContraintKinds
  - lens, vector-space


## Challenges

- Develop high-level theory while staying general
- Difficult error messages
    - Simplified version needed
- Consolidate with other libraries
    - Common time/space/linear algebra core?
- Junk in the backends (as the internal representation is more general), should be moved "higher" up in the libraries

## Backends

Pandoc/Diagrams-style flexibility

  Some import

    MIDI
    Sibelius
    ...
  Lots of export/backends

    MIDI
    OSC
    SuperCollider
    Lilypond
    MusicXML
    ABCNotation
    csound-expression (?)

