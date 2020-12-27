# Boa
A statically typed, mixed-paradigm, Python-inspired language

<!-- ## Demo
The demo is stored in the `tests` directory. They are partitioned into failing,
passing, and passing without typechecking. The demo boa files include comments
documenting the behaviour of the Boa program. To generate a demo, run 
`make demo` from the project root. You should see a `demo.out.md` file generated. -->

## Requirements
- `OCaml 4.10.0`
- `Opam 2.0.7`
- `Llvm 10`
-  `Dune 2.7`

## Install
- `dune build @install` 
- `dune install` 

## Using the Interpreter
To typecheck and interpret a boa program, you can run the executable 
`boa [boa program filename] [optional : -nocheck]`