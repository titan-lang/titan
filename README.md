# Titan
[![Build Status](https://travis-ci.org/titan-lang/titan.svg?branch=master)](https://travis-ci.org/titan-lang/titan)
[![Coverage Status](https://codecov.io/gh/titan-lang/titan/coverage.svg?branch=master)](https://codecov.io/gh/titan-lang/titan/branch/master)

Titan is a new programming language, designed to be a statically-typed,
ahead-of-time compiled sister language to [Lua](http://www.lua.org). It is an
application programming language with a focus on performance.

# Install

First you need to download, extract and build the [sources to Lua 5.3.4](http://www.lua.org/ftp/lua-5.3.4.tar.gz)
inside the folder where you cloned this repository. The Lua tarball will extract
to a `lua-5.3.4` folder. Enter it and build Lua with `make linux MYCFLAGS=-fPIC`.

You can install the Titan compiler itself using  [LuaRocks](http://luarocks.org)
this will also install all dependencies automatically.

        $ [install luarocks]
        $ luarocks make titan-scm-1.rockspec
To install without `sudo` permissions.

        $ [cd into titan folder]
        $ luarocks build --local
You can also run the Titan compiler directly from the folder where you
cloned this repository if you install all the dependencies for the compiler.

# Requirements for running the compiler

1. [LPegLabel](https://github.com/sqmedeiros/lpeglabel) >= 1.5.0
2. [inspect](https://github.com/kikito/inspect.lua) >= 3.1.0
3. [argparse](https://github.com/mpeterv/argparse) >= 0.5.0
4. [luafilesystem](https://github.com/keplerproject/luafilesystem) >= 1.7.0

# Usage

        $ titanc [--print-ast] [--lua <path>] [--tree <path>] <module> [<module>]

The compiler takes a list of module names that you want to compile. Modules
are looked up in the source tree (defaults to the current working directory,
but you can override this with the `--tree` option), as well as in the Titan
binary path, a semicolon-separated list of paths 
(defaults to `.;/usr/local/lib/titan/0.5`, you can override with a `TITAN_PATH_0_5`
or `TITAN_PATH` environment variable).

If everything is all right with your modules, you will get the result of
your compilation as a native binary:

* if one of your Titan modules has a `main` function, with signature
  `function({string}):integer`, then `titanc` will bundle all modules
  given in the command-line, along with all their dependencies where
  source code was available, as a stand-alone executable program.
* Otherwise, it will compile each module into a shared library
  (in the same path as the module source) that you can `import` from
  Titan as well as `require` from Lua, and call any exported
  functions/access exported variables. For each generated module, any
  of its transitive imports is statically linked if source code was
  found; dependencies that were only available as a shared library
  will be dynamically loaded.

# Running the test suite

The test suite es written using Busted, which can be installed using LuaRocks:

        $ luarocks install busted

Then, you need to bulid the local copy of Lua, and run `busted` from the root directory
of this repository:

        $ cd lua
        $ make linux
        $ cd ..
        $ busted

You may need to adapt the invocation of `make` above to your platform.

# Compiler options

        --print-ast                     Print the AST.
        --lua <path>                    Path to the Lua sources (default 'lua-5.3.4/src')
        --tree <path>                   Path to the source tree for your Titan modules (default '.')
        -h, --help                      Show this help message and exit.
        
# Tentative roadmap

This is a *very* preliminary roadmap towards Titan 1.0, where everything is
subject to change, with things more likely to change the further
they are in the roadmap:

## Supported

* control structures
* integers
* floats
* booleans
* strings
* arrays
* top-level functions
* early-bound modules
* multiple assignment/multiple returns
* FFI with C (C pointers, call C functions)
* records (structs) with methods
* maps

## In progress

* first-class functions (still only in the top-level)

## Next

* FFI with C, continued (C arrays, C structs)
* standard library that is a subset of Lua's standard library, built using the C FFI
* tagged variants (unions of structs with some syntax for switch/case on the tag)
* polymorphic functions
* for-in
* self-hosted compiler
* nested and anonymous first-class functions with proper lexical scoping (closures)
* ":" syntax sugar for records of functions
* classes with single inheritance, either Go/Java/C#/Swift-like interfaces/protocols or Haskell/Rust-like typeclasses/traits
* ":" method calls (not syntax sugar)
* operator overloading
* ...Titan 1.0!
