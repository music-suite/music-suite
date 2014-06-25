

## What

Music Representation
  Composition (score generation)
  Transformations (score to score)
  Analysis (score searching/matching)

TODO move this to end
Pandoc-like I/O
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

## Philosophy

No closed definition of music/musics!

*But* there are many common elements in musics!

  How can we formalize these common qualities?

## Define/decompose music

Musical *theory*, concepts such as: scales, chords, rhythm/metrics, harmony, counterpoint

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

## Structure vs flatness

Bars/beats vs free time
Scale (structure) vs frequency
Fixed dynamic levels vs amplitude

Discrete and continuos are complimentary!

For each aspect: Start out with affine space and *add* structure

## Time is special!

  Music take place in time!
  All other aspects are *organized in time*
  
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

This is 1D!
We get the linar transformations...

## Other aspects (Pitch as example)
  Pitch vs Interval (linear)
  High-level concepts
  Using Behavior
  Spelling/intonation etc

## Other aspects

dynamics, parts, articulation, space

## Accessing aspects

The overloaded lenses etc

## Meta-information

Dynamic types, using Reactives a lot (not needed)


## Design principles

- Open
  - Parametric when possible
  - Only make some *core* assumptions (time is free, aspects, meta-data)
  - User can change both events and time containers

- Comprehensive
  - CMT included
  - Easily extendible

- Use the "latest"
  - MPTCs
  - TypeFamilies!
  - ContraintKinds!
  - aeson
  - vector-space (chosen to have a common core with Diagrams)


### Cons of the approach

- Tricky internal dependency (some musical concepts are simply recursive)
  Example!
- Forever incomplete
- Difficult error messages


