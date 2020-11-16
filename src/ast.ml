type var = string

type typ =
  | TFun of typ * typ
  | TBase of string
  | TTuple of typ list
  | TList of typ

type binop =
  | Plus
  | Less
  | Greater
  | And
  | Or
  | Equal
  | Times
  | Minus
  | Divide
  | Mod
  | Exponent
  | IntDivide
  | Leq
  | Geq
  | Is
  | In
  | Neq

type unop =
  | Not
  | Neg

type exp =
  | Var of var
  | Call of exp * exp list
  | AttrAccess of exp * var
  | SliceAccess of exp * exp 
  | Binary of binop * exp * exp
  | Unary of unop * exp
  | Bool of bool
  | Int of int
  | String of string
  | Lam of var * typ * exp
  | Let of var * exp * exp
  | Tuple of exp list
  | List of exp list 
  | Dict of (exp * exp) list 

type stmt = 
  | Exp of exp 
  | Assign of var * exp
  | Decl of typ * var
  | AttrAssgn of exp * var * exp
  | SliceAssgn of exp * exp * exp
  | Return of exp
  | Print of exp
  | Block of stmt list
  | If of exp * stmt * stmt
  | While of exp * stmt
  | For of var * exp * stmt
  | Class of var * exp * stmt
  | Def of typ * var * (typ * var) list * stmt
  | Break
  | Continue
  | Pass
