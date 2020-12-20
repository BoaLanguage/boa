# Boa: A demonstration
## Running passing tests...
### File: tests/passing/fact.boa
```
Inferred Types: [
z => Int
fact => (Int -> (Int -> Int))
]
Evaluating the expression...


State After Execution:
  
z: 24
fact: Some closure
```
### File: tests/passing/recursion.boa
```
Inferred Types: [
number => Int
add_maker => (Int -> (Int -> Int))
repeat => (('a -> 'a) -> ('a -> (Int -> 'a)))
]
Evaluating the expression...


State After Execution:
  
number: 8
add_maker: Some closure
repeat: Some closure
```
### File: tests/passing/test.boa
```
Inferred Types: [
x => MUTABLE: Int
]
Evaluating the expression...


State After Execution:
  
x: mutable (3)
```
### File: tests/passing/tuple.boa
```
Inferred Types: [
x => Int
]
Evaluating the expression...


State After Execution:
  
x: 5
```
### File: tests/passing/type_inference.boa
```
Inferred Types: [
is_f_of_int_positive => (('a -> Int) -> ('a -> Bool))
should_be_42 => Int
fact => (Int -> (Int -> Int))
should_be_true => Bool
compose_fns => (('c -> 'b) -> (('a -> 'c) -> ('a -> 'b)))
is_positive => (Int -> Bool)
add_5 => (Int -> Int)
apply_fn => (('a -> 'b) -> ('a -> 'b))
]
Evaluating the expression...


State After Execution:
  
is_f_of_int_positive: Some closure
should_be_42: 42
fact: Some closure
should_be_true: true
compose_fns: Some closure
is_positive: Some closure
add_5: Some closure
apply_fn: Some closure
```
## Running tests without type-checking...
### File: tests/passing_nocheck/vector.boa
```
Type checking skipped.

Evaluating the expression...


State After Execution:
  
y: {
	__class__ <- mutable ({
	mag <- Some closure
	mult <- Some closure
	add <- Some closure
	__init__ <- Some closure
	__mattrs__ <- y, x, 
	__attrs__ <- 

}
)
	y <- mutable (1)
	x <- mutable (11)

}

x: {
	__class__ <- mutable ({
	mag <- Some closure
	mult <- Some closure
	add <- Some closure
	__init__ <- Some closure
	__mattrs__ <- y, x, 
	__attrs__ <- 

}
)
	y <- mutable (5)
	x <- mutable (12)

}

Vector: mutable ({
	mag <- Some closure
	mult <- Some closure
	add <- Some closure
	__init__ <- Some closure
	__mattrs__ <- y, x, 
	__attrs__ <- 

}
)
```
## Running failing...
### File: tests/failing/fail_type_inference.boa
```
Constraints: , Int == Bool, Int == Int 
Fatal error: exception Check.IllTyped("Typing of program led to above impossible constraints")
```
