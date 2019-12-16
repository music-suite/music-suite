
# music-docs

For main documentation/reference sources, see `src`.

Documentation is written in Markdown and converted to HTML/PDF etc.  using
Pandoc. The tools `hslinks` and `fences` are used to process the
documentation sources before feeding to pandoc. See `Makefile` for the
top-level invocation.

The documentation contains several inline examples which also
serves as tests. These are marked with `music+haskell` in the
markdown sources.

