# Progress Report
In this sprint, we really got a lot of the key features we were hoping for 
in boa implemented. Following is a list of the major goals that we achieved:

 - Hindley-Milner type inference (Algorithm W)
   - We can currently type-check (with inference) all language constructs,
     aside from classes
 - Static type-checking with parametric polymorphism of programs with optional
   type hints
 - Partial application of both lambdas and functions defined with the `def` 
   keyword
 - Mutability enforced in the type system
   - Immutable variables are declared with `let [id] = [expr]`, and 
     mutable variables are declared with `var [id] = [expr]`
   - Variables can be introduced/declared without assigning them a value,
     and their type will be inferred later:
     ```
     let x
     x = 500
     ```
   - Immutable variables can only be given a value a single time, but you 
     can re-introduce a variable with the same name as an already-defined
     variable, with another instance of one of the "var" or "let" keywords:
    ```
    let x = 500
    # x = 10 would not be allowed, x is immutable
    var x = 10
    x = 100
    # this is now allowed, because x has been re-introduced as mutable
    ```


