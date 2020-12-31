(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Llvm
open Ast
exception Error of string

let context = global_context ()
let the_module = create_module context "my cool jit"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let rec codegen_expression (exp: exp) = 
match exp with
  | Int n -> const_float double_type (float_of_int n)    
  | _-> failwith "Unimplemented: Codegem"

let rec codegen_statement (stmt: stmt) = 
match stmt with
  |Exp exp -> codegen_expression exp 
  |Block (h::t) -> codegen_statement h
  | _ -> Pprint.print_stmt stmt; failwith "bad"