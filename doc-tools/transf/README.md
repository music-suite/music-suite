
# transf

*Transf* is simple text transformer and interpreter.

It scans its input for guard tokens and passes everything between to transformation functions. Transformation functions are composed from a small set of combinators and may perform arbitrary Haskell computation. Transf contains a full Haskell interpreter and can even interpret its input as Haskell. 

The main purpose of Transf is to allow the embedding of Domain-Specific Languages in text or Markdown files. 
For example one could use it with Diagrams as follows:

    This is my file. Here is an image:
    
    ~~~diagram "A circle!"
    circle <> stretchX 2 square
    ~~~

Transf can then generate the image, and replace the source in the text file with the name of the actual image. It can also include the source.

    This is my file. Here is an image:
    
    ![A circle](a22b15efb10b.png)

You can supply your own file names. In the above example, the file name is a hash of the source code.

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install

## Usage

### With Pandoc

    (transf OPTIONS | pandoc OPTIONS) <test.md >test.html