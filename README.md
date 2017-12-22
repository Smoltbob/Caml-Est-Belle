# Caml-Est-Belle
Private compiler project repository.</br>
```diff
- Please do not commit on master branch /compiler if it doesn't work
- "master branch passes all tests" should be an invariant.
```

**Feel free to read the [wiki](https://github.com/Smoltbob/Caml-Est-Belle/wiki) for more documentation.**

## State of the project
So far we are handling a subset of ML such that :
* Arithmetic expressions
* Calls to external functions (print_int)
are supported.
We are using the *spill everything* register allocation algorithm.

We also have a wide range of tests regarding this subset.
Finally we have documentation available, both in `docs` and on the wiki.
Organization of the repository:</br>

| Folder | Description |
| --- | --- |
| __ARM/__ | arm source example and compilation with libmincaml |
| __asml/__ | asml examples |
| __compiler/__ | compiler source files |
| __compiler\_very\_basic/__ | compiler with very basic register allocation |
| __doc/__ | all the documentation, start with index.html |
| __mincaml/__ | MinCaml examples |
| __ocaml/__ | MinCaml parser in OCaml |
| __scripts/__ | test scripts and symbolic links to binaries |
| __tests/__ | ASML and MinCaml tests |
| __tools/__ | asml intepreter (linux binary) |

## Installation
Running command `make` in the __compiler/__ folder will create three binaries:
- `mincamlc` is the compiler binary, compiling a MinCaml file into a ARM assembly file.
- `armgen` is the backend binary, converting an ASML file into a ARM assembly file.
- `asmlparse` is the parser, generating and printing an ast from an ASML file.

You can add the __compiler__/ dir to your PATH.

## Usage
A basic usage of the compiler is
```
$ mincamlc mincaml_program.ml -o program.s
```

## Running the tests
To run all the tests :
```
$ make test
```
To run specific tests :
```
$ make test_asml
$ make test_gencode
$ make test_typecheck
```
