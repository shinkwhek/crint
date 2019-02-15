module Ast

type T =
    | Null
    | Int of T 
    | Const of int
    | Ng of T
    | Var of string
    | Op of Operator
    | Func of Func

and Operator =
    | Plus of T * T
    | Minus of T * T
    | Time of T * T
    | Divid of T * T

and Func =
    | Pow of T * T
    | Sqrt of T
    | Exp of T
    | Ln of T
    | Sin of T
    | Asin of T
    | Cos of T
    | Acos of T
    | Tan of T
    | Atan of T