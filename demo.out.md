# Boa: A demonstration
## Running passing tests...
### File: tests/passing/fact.boa
```
Inferred Types: [
fact => (Int -> (Int -> Int))
]
Evaluating the expression...


State After Execution:
  
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
### File: tests/passing/fact.boa
```
Inferred Types: [
fact => (Int -> (Int -> Int))
]
Evaluating the expression...


State After Execution:
  
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
## Running failing...
### File: tests/failing/fail_type_inference.boa
```
Constraints: , Int == Bool, Int == Int 
Fatal error: exception Check.IllTyped("Typing of program led to above impossible constraints")
```
