# Boa
A statically typed, mixed-paradigm, Python-inspired language

## Demo
The demo is stored in the `tests` directory. They are partitioned into failing,
passing, and passing without typechecking. The demo boa files include comments
documenting the behaviour of the Boa program. To generate a demo, run 
`make demo` from the project root. You should see a `demo.out.md` file generated.

## Compile
In order to compile the boa interpreter, run `make` from the project root.
The `boa` binary should be located at `./bin/boa`. 

## Using the Interpreter
To typecheck and interpret a boa program, you can run the executable 
`./bin/boa [boa program filename] [optional : -nocheck]`