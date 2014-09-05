% <a style="color:">Music Suite</a>
% <a style="color:crimson">music-suite.github.io</a>
% <a style="color:blue">Hans Höglund</a> <br/><br/> <img src="farm2014/images/logo3.svg"/>

## Music Suite

- What is music?

- Fundamentally it is <a style="color:blue">*sound*</a>, but we want a more structured representation.

## Music Suite

- What is music?

- Fundamentally it is <a style="color:blue">*sound*</a>, but we want a more structured representation.

### Can be used for ... of music

-|-|-|-|-|-
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|*Composition*  |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| <a style="color:crimson">*a*</a>     | &nbsp;➝&nbsp; |  <a style="color:blue">*Music*</a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|*Manipulation* |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| <a style="color:blue">*Music*</a> | &nbsp;➝&nbsp; | <a style="color:blue">*Music*</a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|*Analysis*     |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| <a style="color:blue">*Music*</a> | &nbsp;➝&nbsp; | <a style="color:crimson">*b*</a>


## type Music = <a style="color:crimson">?</a>

- No closed definition of music(s)
    - Music making software <a style="color:blue">*necessarily*</a> impose restrictions and on the user

    - Usually a Western classical/popular music context is assumed.
      <br/><a style="font-size:80%">Diatonic/chromatic scale, 3/4 and 4/4 time signatures, etc.</a>

## type Music = <a style="color:crimson">?</a>

- No closed definition of music(s)
    - Music making software <a style="color:blue">*necessarily*</a> impose restrictions and on the user

    - Usually a Western classical/popular music context is assumed.
      <br/><a style="font-size:80%">Diatonic/chromatic scale, 3/4 and 4/4 time signatures, etc.</a>

- <a style="color:crimson">*But*</a> there are many common elements in music!
    - How can we formalize these common qualities?

## type Music = <a style="color:crimson">?</a>

Musical <a style="color:blue">*theory*</a>

- Concepts such as: scales, chords, rhythm/metrics, harmony, counterpoint

Musical <a style="color:blue">*aspects*</a>

- Time, pitch, dynamics, timbre (instrument/articulation), (space..)

Musical cultures rank these differently!

<!--
- Western classical places emphasis on pitch/harmony.
- Jazz places more emphasis on time/rhythm.
-->

## Basic API layout

### Aspects

- music-dynamics
- music-pitch
- etc.

### Special

- music-score
- music-preludes

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

- View time as a one-dimensional <a style="color:blue">affine space</a>.
    - Separate *Time* (points) and *Duration* (vectors)

- An affine transformation is simply *(Product~Duration~ ⨯ Sum~Time~)*
    - This type is called *Span*
    - Isomorphic to *Time^2^* or *(Time ⨯ Duration)*


## Musical aspects: Pitch...
  Pitch vs Interval (linear)
  High-level concepts
  Using Behavior
  Spelling/intonation etc

## Musical aspects: ...and the rest

dynamics, parts, articulation, space

## Accessing aspects

- Edward Kmetts's `lens` gives us the vocabulary:

Name |&nbsp;| Meaning
-|-|-
Lens' s a      |&nbsp;| (s ➝ a) ➝ (s ➝ a ➝ s) 
Prism' s a     |&nbsp;| (a ➝ s) ➝ (s ➝ Maybe a)
Iso' s a       |&nbsp;| (s ➝ a) (a ➝ s)
Traversal' s a |&nbsp;| Applicative f ⟹ (a ➝ f a) ➝ s ➝ (f s)

Name |&nbsp;| Meaning
-|-|-
Lens s t a b      |&nbsp;| (s ➝ a) ➝ (s ➝ b ➝ t) 
Prism s t a b     |&nbsp;| (b ➝ t) ➝ (s ➝ Either t a)
Iso s t a b       |&nbsp;| (s ➝ a) (b ➝ t)
Traversal s t a b |&nbsp;| Applicative f ⟹ (a ➝ f b) ➝ s ➝ (f t)

## Meta-information

Dynamic types, using Reactives a lot (not needed)


<!--
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
-->

## Backends

### Import

- MIDI
- Sibelius

### Export

- MIDI
- Lilypond
- MusicXML
- SuperCollider


## Challenges

- Develop high-level theory while staying general
- Difficult error messages
    - Simplified version needed
- Consolidate with other libraries
    - Common time/space/linear algebra core?
- Junk in the backends (as the internal representation is more general), should be moved "higher" up in the libraries


<!--
### Wanted

    OSC
    ABCNotation
    csound-expression
-->

