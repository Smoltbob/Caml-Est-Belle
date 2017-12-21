# Caml-Est-Belle
Private compiler project repository.</br>
```diff
- Please do not commit on master branch /compiler if it doesn't work :-(
- It's acutally very important.
- "master branch passes all tests" should be an invariant.
+ Thanks!
```

**Feel free to read the [wiki](https://github.com/Smoltbob/Caml-Est-Belle/wiki) for more documentation.**

Organization of the repository:</br>

| Folder | Description |
| --- | --- |
| __ARM/__ | arm source example and compilation with libmincaml |
| __asml/__ | asml examples |
| __compiler/__ | compiler source files |
| __compiler\_very\_basic/__ | very basic compiler with only very basic register allocation |
| __doc/__ | all the documentation, start with index.html |
| __mincaml/__ | MinCaml examples |
| __ocaml/__ | MinCaml parser in OCaml |
| __scripts/__ | test scripts and symbolic links to binaries |
| __tests/__ | ASML and MinCaml tests |
| __tools/__ | asml intepreter (linux binary) |

## Installation
Running command `make` in the __compiler/__ folder will create two binaries:
- `mincamlc` is the compiler binary, compiling a MinCaml file into a ARM assembly file
- `armgen` is only the backend binary, converting an ASML file into a ARM assembly file

You can add the __compiler__/ dir to your PATH.

## Usage
A basic usage of the compiler is
```
$ mincamlc mincaml_program.ml -o program.s
```
