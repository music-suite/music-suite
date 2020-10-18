
Experimental port of the docs to use Sphinx/Read-the-Docs.

## Instructions

In the normal Nix env, run

     pandoc --to=rst docs/src/User-Guide.md >docs-sphinx/docs/source/index.rst

Then in the Nix env from this dir, run

     cd docs && make html
