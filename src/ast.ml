
type t =
  | Null
  | Int of t 
  | Const of string
  | Var of string
  | Op of operator
  | Func of func

and operator =
  | Plus of t * t
  | Minus of t * t
  | Time of t * t
  | Divid of t * t

and func =
  | Exp of t
  | Ln of t
  | Sin of t
  | Asin of t
  | Cos of t
  | Acos of t
  | Tan of t
  | Atan of t
[@@deriving show]
