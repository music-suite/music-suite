
## TODO name of tutorial

There are many great tutorials for learning Haskell:

http://www.haskell.org/haskellwiki/Tutorials

For beginners the best one is Learn You a Haskell:

http://www.haskell.org/haskellwiki/Tutorials

There are also many books introducing musical composition. Many deal with general properties of melody and harmony, others specifically with writing for classical instruments whether in a traditional or modern styles, some are written with recorded music in mind, and yet other countless tutorials deal with the now ubiquitous art of creating music on  a computer – popular, classical or experimental.

Why is functional programming and musical composition related? For a start they are both (as Paul Graham famously pointed out) about creating things. But functional programming is especially suited to creative arts, a it is a style of writing that encourage us to think in terms of relationships rather than actions. One might go as far as to say that it is not programming at all, at least not in the customary sense of the word, because it is *structural* rather than *technical*. You don't need to know anything about computers to write functional programs, the only thing you need is a desire to express yourself and a rational mind.

The Haskell language that we use here is in part infamous for introducing "difficult" words such as "Monad", "Semigroup" and "Functor". However initially, we should not worry too much about these words, as they are not essential to the language, and because it is generally a bad idea to learn about abstract and general concepts without first developing a un understanding of it practical use.

## (H+M) Musical expressions

TODO really simple way of getting started: write this, listen/look, save the result, go on.

    >>> c |> d

What you just wrote is an *expression*. Expressions are a bit like sentences, in that they are built up from small words, and in that each expression has a *meaning*. The words here are of course just the pitches: *c* and *d*. In constrast to ordinary language, these words are actually complete expressions themselves:

    >>> c

    >>> d
    
The odd-looking thing in the middle is something else: an operator. Operators are not complete expressions in themselves, but can be thought of as a glue used to put other things together, a bit like the conjunctions in English (*and*, *or*, *because*, etc.).

Expressions are also the basis of mathematics, here the operators are simply called *+*, *-*, *\**, etc. Mathematical expressions have different meanings from musical ones, representing things like numbers, sets and so on, but the principle is the same:

    >>> 2

    >>> 3
    
    >>> 2 + 3
    
    >>> 4 * (3 + 1)
    
### Bracketing up expressions

Operators are actually part of a family of more general things called *functions*. A function can be thought of as a machine that takes *one or more things* and gives you back *one single thing*. We classify functions from how many things they take, so 

Number of inputs|Name
-|-
1|Unary
2|Binary
3|Ternary

Operators such as `|>` and `+` are *binary* (takes two things).

Binary functions with text names can be written in two ways: in the middle of the things it is taking as inputs or before them.

    >>> 2 + 3
    
    >>> (+) 2 3
    
    >>> up m3 c
    
    >>> m3 `up` c

    >>> inv c (c |> d |> e)
    
    >>> c `inv` (c |> d |> e)

    >>> compress 2 (c |> d |> e)
    
    >>> 2 `compress` (c |> d |> e)

There are also many unary functions:

    >>> negate 3
    -3
    
    >>> rev (c |> d)

    >>> sharpen

### Recap:

- Expressions are like sentences: they mean something
- Expressions are built from primitives and functions like a tree
- Brackets can be used to clarify the structure of expressions
- Functions transform 1 or more things into one thing
- Binary functions are often written as operators (in the middle) 

### Exercises:

- Write a melody using just |> and the pitch names
- Write a sequence of chords using <>, |> and the pitch names
- Write a melody using stretch, compress, parentheses |> and the ordinary pitch names





## (H+M) Defining things

    >>> let x = 1
    >>> let y = 2
    >>> x + y
    3

Don't bother about 'let' for now. You can think of the *=* sign as meaning "is". The name written to its *left* is given the value to its *right*.

    >>> let melody = c |> d |> e
    >>> let bass   = c |> g |> c
    >>> melody <> bass

### Exercises:

- Define several melodies using `let`
- Define several chord sequences using `let`
- Combine the melodies and chords you have written into a 1-minute composition


## (H+M) Lists and pairs

Constructing:

    >>> (1,2)

    >>> [1,2,3]

Destructing (also called *pattern matching*):

    >>> let (x,y) = (10,11)
    >>> x
    10 
    
    >>> let [a,b,c] = [1,2,3]
    >>> a
    1
    
Beware that when destructing a list you must give it exactly the right length, for reasons that will become apparent later on.

Functions taking lists

    >>> sum [2,3]
    6
    >>> product [5,10,1]
    15
    >>> highest [c,d]
    Just d
    >>> pseq [c,e,g]
    >>> ppar [c,e,g]

    >>> join [[1,2]‚[3,4]]
    [1,2,3,4]
    >>> pseq [c,e,g]
    >>> ppar [c,e,g]

### Exercises:

- Define several melodies and chords using `pseq`, `ppar` and pitch names
- Combine the melodies and chords you have written into a 1-minute composition

## Delay and stretch

TODO needs work to be comprehensible

### Exercises:

- Write a canon consisting of a single melody with delayed entries (see above)

## Defining functions

    >>> let twice x = x |> x

This function has the name 'twice', and is a unary function.

    >>> twice c

    >>> twice (c |> d)

    >>> let id x = x
    >>> id 2
    2
    >>> id []
    []

You can also define functions *inside* an expression. In this case you don't have to give the function a name (in fact you can't). You also have to replace the `=` sign with `->` and write a `\` before the first parameter name. Here is a way of defining and using `id` in a single line.

    >>> (\x -> x) 22
    22

And the same thing with `twice`:

    >>> (\x -> x |> x) c

(This may not look that impressive but is in fact the most important and powerful thing you can do in Haskell.)




## Types (H+M)

When we want to see the type of something, we write `:t`.

    >>> :t c
    Pitch

    >>> :t m3
    Interval

    >>> :t True
    Bool
    
### Common types
    
Name|Example
-|-
Bool | `True`, `False`
Integer | `-1`,`0`,`1`,`2`
Double | `-1`,`0`,`0.5123`
Pitch | `c`,`cs`,`gb`
Interval | `_P1`,`m3`,`d9`
Time|`1`,`2`,`3`
Duration|`1`,`2`,`3`
Span|`1<->2`
Dynamics|`ppp`,`pp`,`_p`,`mp`,`_f`,`ff`
Articulation|`staccato`,`legato`,`accent`
Instrument|`flute`,`piano`
Part|`flutes`,`divide 2 violins !! 0`



## Accessing things using lenses, traversals etc.

A lens is like a *label* that allows you to *view* and *set* a part of a larger structure. To view a value we use the `^.` operator (or simply `view`).

    >>> (1,2) ^. _1
    1
    
    >>> (1,2) ^. _2
    2

    >>> [c,d,e] ^. _head
    c

    >>> [c,d,e] ^. _last
    e
    
    >>> view _last [c,d,e]
    e

To set a value, use *set*:

    >>> set _1 10 (1,2)
    (10,2)

    >>> set _head 10 [1,2,3]
    [10,2,3]

To update value, use *over*:

    >>> over _1 (* 10) (1,2)
    (10,2)

### Traversals

### Prisms

### Isomorphisms

### Using pitches, dynamics, parts etc.

## Type constructors

We have already introduced pairs and lists

### Standard type constructors

Maybe

Either

Set

Map


## Voices, Notes, Chords and Rests

## Working with melody and harmony

## Spans, time and delayed values

## Scores and Tracks

## Instruments, playing techniques and percussion


