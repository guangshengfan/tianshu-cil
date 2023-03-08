C Intermediate Language (CIL)
============================

Linux [![Linux build Status](https://travis-ci.org/cil-project/cil.svg?branch=develop)](https://travis-ci.org/cil-project/cil)
Windows [![Windows build status](https://ci.appveyor.com/api/projects/status/jtgf72r03jnge7jw/branch/develop?svg=true)](https://ci.appveyor.com/project/kerneis/cil/branch/develop)


CIL is a front-end for the C programming language that facilitates
program analysis and transformation. CIL will parse and typecheck a
program, and compile it into a simplified subset of C.

CIL supports ANSI C as well as most of the extensions of the GNU C and
Microsoft C compilers. A Perl script acts as a drop in replacement for
either gcc or Microsoft's cl, and allows merging of the source files in
your project. Other features include support for control-flow and
points-to analyses.

We  follow the way the same as Goblint-cil(version 2.0)  to change CIL(version 1.7.3) to another version compiled with dune.

For how to build and install, just see Goblint-cil (https://github.com/goblint/cil)

First create a local opam switch and install all dependencies:

opam switch -y create . --deps-only ocaml-base-compiler.4.14.0 --locked

Then, you can use dune to build tianshu-cil. Run the following commands to build:

dune build

You can also install tianshu-cil into the opam switch:

dune build @install
dune install


need to fix and improve