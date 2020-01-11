


## Basic usage

There are several ways to use the suite. Which one you want depends on your needs and preferences.

#### Writing music files

A music file is a file containing a Haskell module that defines a top-level music expression named `score`. Because music files are Haskell modules, they can be loaded into a Haskell interpreter.

Writing a music file is similar to entering a score in Lilypond, Guido, ABC notation or another text-based score format, except that the language you will use is embedded in Haskell and you have full access to Haskell libraries and features.

The programs `music2ly`, `music2abc`, `music2musicxml` etc. can be used to convert music files into common output formats.

#### Writing Haskell programs

You may want to use the Music Suite for something other than simply generating music. For example you may want to write a program than analyzes or transforms music, or a program that generates music interactively.

In this case you should write an ordinary Haskell program and compile it or execute it using `runhaskell`.

#### Writing Haskell libraries

You may want to use the music suite as a starting point for your own music-related Haskell libraries. In this case you can simply add the libraries to your `.cabal` file and import the modules as usual.

#### Writing text or Markdown files with embedded music

The suite also includes a special program called `music-transf`, which interprets music expressions in text or Markdown files. The resulting music can be rendered as graphics (rendered using `lilypond`), Midi, MusicXML or another text-based music format.

Among other things, `music-transf` is useful for generating nice inline musical examples such as this one:

```music
legato $ level mp $ stretch (1/16) $ (pseq [c,d,e,f]) |> g|*12
```
