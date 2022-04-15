# vhdl-testbench-gen

This project is home to a naive implementation of a testbench
generator for basic VHDL modules. This tool is not intended to 
create fully-functional testbenches. Rather, this was written 
with the intention of cutting out as much boilerplate as possible 
involved in writing a testbench targeting a VHDL entity for the 
first time.

My foremost motivator for writing this tool was simply to get a
bit more comfortable using Haskell, to which I still feel pretty
new, please excuse non-idiomatic language use and avert your gaze 
from some of the more heinous parts of this project.

This project was developed and tested on a system running macOS
Monterey Version 12.1

## Running Project

```console
$ stack build
$ stack exec -- vhdl-testbench-gen-exe --input <path-to-vhdl-entity>
```

## TODO
* Real test cases need to be implemented. Though I've run
  rudimentary "tests" using various styles of VHDL from top 
  results on github, there are surely errors lurking in the 
  parsing logic that I have yet to uncover.
* I'd like to revisit this once I have a bit more Haskell under
  my belt. I used this project as a simple starting point for 
  trying to learn Parsec, particularly applicative-style parsing.
