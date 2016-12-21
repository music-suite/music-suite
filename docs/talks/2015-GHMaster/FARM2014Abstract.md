
---
title: "Music Suite: A Family of Musical Representations"
subtitle: FARM 2014 Demo Proposal
...


# Background

Musical representation is one of the most central problems in any branch of computer music: at the core of any computer music library or application is a set of data structures defining the possibilities and limitations of the system, and consequently of the music handled by it. In effect, this structure is not merely a *representation*, but the grammar of the musical *language* understood by the system and its users.

Despite, or perhaps because of its importance, musical representation is widely held to be complex and poorly understood. In part, this reflects on the nature of music theory, which is (understandably) more concerned with flexibility and appropriateness for specific musical problems than with the generality of the underlying theoretical model.

Computer music systems fall into two broad categories: score-based systems which operate on hierarchical structures similar to musical scores, and sound-based systems which operate on audio signals. More generally, we can talk about *discrete* and *continuos* representations of music. The distinction is not merely technical, but conceptual: a discrete representation is necessarily more structured than a single continuous signal. 

<!--
On the other hand, discrete representation tends to give rise to continuous *souding results*. For example, an musical score consists of a discrete set of continuous events, which when player together give rise to a single continuous audio signal.  
-->

Traditionally audio-based systems tend to use a very open approach with few assumptions on the music (or other forms of audio) being processed. Score-based systems, on the other hand, tend to be limited to a small musical vocabulary, often derived from Common Music Notation. This naturally makes it difficult to represent music from traditions other than Western classical music.

The use of a modern functional language such as Haskell suggests a more general approach, in which culture-specific musical concepts are treated parametrically. For example, the concept of *score* can be generalized to not only work on notes in the diatonic–chromatic pitch space, but in any pitch space, or even include non-pitched sounds.

# The Music Suite

The Music Suite attempts to be a general music representation system including both discrete and continuos representations. The design of the suite is informed by the following principles:

- It should describe musical *meaning*, which includes both its sounding *interpretation* and its visual *notation*.

- It should avoid imposing stylistic or theoretical assumptions on the music. In particular we want to treat the core musical aspects of *pitch*, *dynamics* and *articulation* polymorphically.

- It should allow different kinds of *part* and *time* structure. using composable types and operators. These types should a well defined *time semantics* allowing us to reason about time and structure in a generic fashion.

- It should include common music notation as a *special case*.


## Organization

The suite is conceived as a set of Haskell packages developed and released synchronously with each other. Each package focuses on a single musical *aspect* such as *time*, *pitch*, *dynamics* or *articulation*. The type of music represented in the suite is not fixed, but composed out of a set of generic type constructors and combinators. 

Of all the aspects, time is treated somewhat specially. Indeed, many of the types and functions in the suite are not strictly limited to music, but could apply to any time-based medium including animation or real-time performance control.

Most musical aspects can be thought of as points in some affine space, with an associated vector space to represent distance between points. For example, pitches forms an affine space with intervals as the underlying vector space, while time points form an affine space with durations as vectors.

This is similar to the approach taken in Diagrams^[http://projects.haskell.org/diagrams], with some notable differences: while graphical representations tends to span a single multi-dimensional affine space such as `R2`, the musical types each span a particular one-dimensional affine space.

## Time types

The core of the library is a set of *time types*, which define musical time and values with a position in time. These types borrows many concepts from FRP, particularly Reactive^[http://www.haskell.org/haskellwiki/Reactive], which provides an established vocabulary for reasoning about discrete and continuos time structures.

The most basic time types are `Time` and `Duration` which represents points and distances in time, `Span` which represents an era between two time points. From these simple types and ordinary pairs and functions, a rich vocabulary of continuos and discrete time types can be constructed.

The Music Suite does not provide any real-time implementations of these structures: the approach is data-centric and intended to be used in non-realtime applications such as composition and analysis (see also Future Work).

## Musical aspects

Musical aspects such as pitch and dynamics is treated parametrically. While most time types are unary type constructors, musical aspects are represented by concrete types. To handle several musical aspects in a single structure a set of classes and type functions are provided for extracting and updating the appropriate type in a nested structure.

For example the `Music.Score.Pitch` module defines the following type functions:

    type family Pitch (s :: *) :: *
    type family SetPitch (b :: *) (s :: *) :: *

Then `lens` library is used for accessing pitches. Two classes are provided: `HasPitch`, for types with a single pitch, and `HasPitches` for types that provides an arbitrary number of pitches, defining a `Lens` and a `Traversal` respectively.

The overloaded lenses and traversals allow us to define operations such as dynamic fades, pitch transposition and so on over any kind of container that contains the musical aspect in questions, including single notes, voices or scores.

## Meta-information

Purely graphical information is treated *dynamically*, using existential wrappers for `Typeable` values. The approach is similar to Diagram's *styles*. Each time type (including behaviors, voices and scores) provide an instance of the following class:

    class HasMeta a where
        meta :: Lens' a Meta

`Meta` is conceptually a dictionary of dynamic values subject to time transformations, and can be wrapped and unwrapped as follows:

    type IsAttribute a = 
        (Typeable a, 
         Monoid a, 
         Transformable a)
    toMeta :: IsAttribute a => a -> Meta
    fromMeta :: IsAttribute a => Meta -> Maybe a

The notion of meta-data is not limited to constant values: in fact most types of meta-data used in the Music Suite are stored as *time-varying* values. This makes it possible to track information that affects notation but not sound, such as key and time signatures. Backends can recognize meta-information as they see fit.

## Preludes

The libraries provide a large set of representations of common musical aspects such as pitch, dynamics, articulation and so on. The goal is to be *open* and *comprehensive* rather than *complete*. Representations can be combined in many ways to form a suitable representation for almost any kind of music. In a sense, the music suite describes an *family* of embedded domain-spefic languages.

The 'music-preludes' package defines a large set of ready-to-use musical representations which are compatible with standard formats such as MIDI and MusicXML, provided along with a large set of examples and regression tests.


# Status

Released under an "optimistic" versioning scheme, the suite is currently at version 1.7.0, with 2.0.0 set to be the first stable version. The library has undergone steady development since early 2013 and the aim is to release 2.0.0 in early 2015.

The Music Suite is distributed on Hackage and the source code is freely available on Github. It is installed using:

    cabal install music-preludes

For reference, documentation and examples, see:

    http://music-suite.github.io

For the source code, see:

    https://github.com/music-suite

Discussion and suggestions for improvements should take place on the dedicated mailing list:

    music-suite-discuss@googlegroups.com

# Future work

There is still much room for improvement. While the suite currently offers a comprehensive set of musical *primitives*, it lacks many higher-level musical concepts such as scales, rhythms, motives or contraint-based rule systems such as those used in Western classical counterpoint or Indian classical ragas. The main challenge is to implement high-level concepts while still maintaining generality.

The suite requires knowledge of intermediate-to-advanced type system features, and ability to parse possibly intimidating error messages. A *simplified* version, while less general, could provide a valuable pedagogical alternative which could serve as an introduction to the suite, possibly as a standalone compiler or web application. As is often the case, there is a tension between generality and simplicity.

The current implementations of the data structures are general, but not very efficient. Much can be gained by adding minimal restrictions, or developing restricted variations of the core data structures.

The Music Suite shares many concepts with both FRP libraries and graphical libraries such as Diagrams. Although we already share a common core in standard type-classes, and the `vector-space` and `vector-space-points` libraries, there is the possibility of factoring out a larger common core. In particular, the `Behavior` and `Track` types in `music-score` corresponds to behaviors and events in classical FRP. The concepts of points, segments and transformations used in Diagrams also have direct correspondences in the Music Suite.

