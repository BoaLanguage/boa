# Blog Post

## Vision
Our vision is that we can produce a variant of Python where programs can have more assurances about abstractions, achieved through the type system and immutability. Ideally, programmers will be able to take advantage of the expressiveness and simplicity of Python while also being able to write elegant and clear code where correctness can be more easily reasoned about. 

## Status
boa is currently able to support typechecking of most control structures aside from objects and classes (some others), and can evaluate almost all control structures aside from object slice access expressions and for loops over iterables. 

To build the boa interpreter, run "make", pertaining to the makefile in the src directory. In order to run a boa program that makes use of classes and objects, you will have to run the command `boa [filename] -nocheck`. Otherwise,

Boa programs can be written in a similar manner to Python programs, with a couple of key differences. 

 - First, currently curly braces must be used to delineate blocks of code, though we hope to transition to using indentation in the future. 
 - Variable declarations can take one of two forms (below, non-bracketed words are langauge keywords). Declarations with the "let" keyword introduce immutable variables, and those with "var" are mutable. 
   - `let [id] : [type] = [expr]`
   - `var [id] : [type] = [expr]`
 - Function definitions must begin with a "def" keyword, followed by the name of the function, and a parenthesized, comma-separated list of arguments annotated with types (of the form `[id] : [type]`). After the argument list there must be an arrow `->` pointing to the return type of the function, followed by a newline and a block of code. 
 - Class declarations require a list of all data members at the beginning of the declaration. These data member declarations are variable declarations without an assignment, preceded by the keyword "member" (this will probably be removed in the future). Instance methods must have at least one parameter, which will be passed the object instance upon a method call. Instance initializers are just as they are in Python - there can be a single initializer in a class, and it must be named `__init__`. Currently, `__init__` methods must end with an expression returning "self" (or whatever the first method argument is). Following is the general structure of a class declaration:
   - ```class [name]:
     {
       member let [id1] : [type]
       member var [id2] : [type]

       def __init__(self, [...args])
       {
         self.[id1] = [expr]
         self.[id2] = [expr]

         return self
       }
     }```

Here is an example implementation of a vector class in Boa:

```class Vector2d
{
  member let x : Int
  member let y : Int

  def __init__(self : Vector2d, x : Int, y : Int) -> Vector2d
  {
    self.x = x
    self.y = y

    return self
  }
  def magnitude_squared(self : Vector2d) -> Int
  {
    return (self.x ** 2) + (self.y ** 2)
  }
  def add(self : Vector2d, other : Vector2d) -> Vector2d
  {
    return self.__class__(self.x + other.x, self.y + other.y)
  }
}

let vec1 : Vector2d = Vector2d(10, 20)
let vec2 : Vector2d = Vector2d(1, 2)
let vec3 : Vector2d = vec1.add(vec2)
print (vec3.x)
print (vec3.y)```

## Next Steps

In our next steps, we plan to finish off typechecking and evaluation of for loops, iterables, and slices. We also plan to build some part of a compatibility library which will allow boa users to interface with Python. 