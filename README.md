# nl2ast

## What is it?

This tool allows you to parse a NetLogo model into an AST, serialized to JSON.

## How do you use it?

Get SBT from [here](https://www.scala-sbt.org/download/), if you don't already have it.  Clone this repo, `cd` into it, and then run `sbt` to load the project in SBT.

Once it's loaded, there's really only one thing that you can do with it: Type `run <uri> <out>`, substituting in the URI (e.g. `https://netlogoweb.org/assets/modelslib/Sample Models/Biology/Daisyworld.nlogox`, or `file:///home/me/nl2ast/testfiles/Slime.nlogo`) for `<uri>`, and optionally providing a filepath for the `<out>` value.  If `<out>` is provided, the JSON will be written to that file; if not, it will be printed to `stdout`.
