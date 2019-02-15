module Randomize
open System
open Ast

type T =
    | CONST
    | NG
    | VAR
    | OP
    | FUNC
    | PLUS
    | MINUS
    | TIME
    | DIVID
    | POW
    | SQRT
    | EXP
    | LN
    | SIN
    | ASIN
    | COS
    | ACOS
    | TAN
    | ATAN

let glst    : T list = [ OP; OP; FUNC ] 
let grlst   : T list = [ CONST; VAR; VAR; OP; OP; FUNC; FUNC ]
let oplst   : T list = [ PLUS; MINUS; TIME; TIME; DIVID; DIVID ] 
let funclst : T list = [ POW; SQRT; EXP; LN; SIN; ASIN; COS; ACOS; TAN; ATAN ] 

let rnd = System.Random ()

let randomGet (l: T list) : T = List.item (rnd.Next(List.length l)) l
  
let rec exprRandomize (depth: int) (initial: int) : Ast.T =
  if depth > 0 then
    match (if depth = initial then (randomGet glst)
                              else (randomGet grlst)) with
    | CONST -> let c = 1 + (rnd.Next 9) in
               Ast.Const(c)
    | NG    -> let e1 = exprRandomize (depth - 1) initial in
               Ast.Ng(e1)  
    | VAR   -> Ast.Var("x")
    | OP -> let e1 = exprRandomize (depth - 1) initial in
            let e2 = exprRandomize (depth - 1) initial in
            match (randomGet oplst) with
            | PLUS  -> Ast.Op(Ast.Plus(e1, e2))
            | MINUS -> Ast.Op(Ast.Minus(e1, e2))
            | TIME  -> Ast.Op(Ast.Time(e1, e2))
            | DIVID -> Ast.Op(Ast.Divid(e1, e2))
            | _     -> Ast.Null

    | FUNC -> let e1 = exprRandomize (depth - 1) initial in
              match (randomGet funclst) with
              | POW   -> Ast.Func(Ast.Pow(e1, exprConstOrVar()))
              | SQRT  -> Ast.Func(Ast.Sqrt(e1))
              | EXP   -> Ast.Func(Ast.Exp(e1))
              | LN    -> Ast.Func(Ast.Ln(e1))
              | SIN   -> Ast.Func(Ast.Sin(e1))
              | ASIN  -> Ast.Func(Ast.Asin(e1))
              | COS   -> Ast.Func(Ast.Cos(e1))
              | ACOS  -> Ast.Func(Ast.Acos(e1))
              | TAN   -> Ast.Func(Ast.Tan(e1))
              | ATAN  -> Ast.Func(Ast.Atan(e1))
              | _     -> Ast.Null
    | _ -> Ast.Null

  else
    Ast.Var("x")

and exprConstOrVar () =
  match (randomGet [CONST; VAR; VAR]) with
  | CONST   -> let c = 1 + rnd.Next(9) in Ast.Const(c)
  | VAR     -> Ast.Var("x")
  | _       -> Ast.Null
  
let expr (depth: int) : Ast.T =
  let e = exprRandomize depth depth in
  e