type var = string
type tvar = int


type typ =
  | TFun of typ * typ
  | TBase of string
  | TTuple of typ list
  | TList of typ
  | TDict of typ * typ
  | TVar of tvar
  | TObj of {attrs: (var * typ) list; mattrs: (var * typ) list}

type scheme = tvar list * typ
type mappings = (var * scheme) list

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
  | Lam of var * (typ option) * exp
  | Let of var * exp * exp
  | Tuple of exp list
  | List of exp list 
  | Dict of (exp * exp) list 
  | Skip

type stmt = 
  | Exp of exp 
  | Assign of var * exp
  | Decl of (typ option) * var
  | MemDecl of typ * var
  | MutableDecl of (typ option) * var
  | MutableMemDecl of typ * var
  | AttrAssgn of exp * var * exp
  | SliceAssgn of exp * exp * exp
  | Return of exp
  | Print of exp
  | Block of stmt list
  | If of exp * stmt * stmt
  | While of exp * stmt
  | For of var * exp * stmt
  | Class of var * exp * stmt
  | Def of (typ option) * var * ((typ option) * var) list * stmt
  | Break
  | Continue
  | Pass
 

type value = 
  | VInt of int
  | VString of string
  | VBool of bool 
  | VClosure of var list * stmt * env ref
  | VDict of (value * value) list
  | VTuple of value list
  | VObj of (var * value) list
  | VMethodCall of (value * value)
  | VPreObj of (var * value ref) list
  | VList of value list
  | VNone
  | VRef of value option ref

and env = (var * value) list