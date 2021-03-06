module Simplify
open Ast

let rec eval (e: Ast.T) : Ast.T =
    match e with
    (* No Change *)
    | Ast.Var(s) -> Ast.Var(s)
    | Ast.Const(0) -> Ast.Const(0)
                  
    (* simplify negative *)
    | Ast.Ng(Ast.Const(0)) -> Ast.Const(0)
    | Ast.Ng(Ast.Ng(e)) -> e
    | Ast.Ng(e) -> Ast.Ng(eval(e))

    (* simplify (_ + _) *)          
    | Ast.Op(Ast.Plus(e1, e2)) ->
        match (e1, e2) with
        | Ast.Const(0), e
        | e, Ast.Const(0) -> e
        | Ast.Const(n1), Ast.Const(n2) -> Ast.Const(n1 + n2)
        | Ast.Ng(e1), e2 -> Ast.Op(Ast.Minus(e2, e1))
        | e1, Ast.Ng(e2) -> Ast.Op(Ast.Minus(e1, e2))
        | Ast.Var(s1), Ast.Var(s2) ->
            if s1 = s2
            then Ast.Op(Ast.Time(Ast.Const(2), Ast.Var(s1)))
            else Ast.Op(Ast.Plus(Ast.Var(s1), Ast.Var(s2)))
        | _ ->
            if e1 = e2
            then Ast.Op(Ast.Time(Ast.Const(2), eval(e1)))
            else Ast.Op(Ast.Plus(eval(e1), eval(e2)))

    (* simplify (_ - _) *)
    | Ast.Op(Ast.Minus(e1, e2)) ->
        match (e1, e2) with
        | Ast.Const(0), e -> Ast.Ng(e)
        | e, Ast.Const(0) -> e
        | Ast.Const(n1), Ast.Const(n2) -> Ast.Const(n1 - n2)
        | Ast.Ng(e1), e2 -> Ast.Ng(Ast.Op(Ast.Plus(e1, e2)))
        | e1, Ast.Ng(e2) -> Ast.Op(Ast.Plus(e1, e2))
        | _ ->
            if e1 = e2
            then Ast.Const(0)
            else Ast.Op(Ast.Minus(eval(e1), eval(e2)))

    (* simplify (_ * _) *)
    | Ast.Op(Ast.Time(e1, e2)) ->
        match (e1, e2) with
        | Ast.Const(0), e
        | e, Ast.Const(0) -> Ast.Const(0)
        | Ast.Const(1), e
        | e, Ast.Const(1) -> e
        | Ast.Const(n1), Ast.Const(n2) -> Ast.Const(n1 * n2)
        | e, Ast.Const(_) -> Ast.Op(Ast.Time(e2, e1))
        | Ast.Ng(e1), e2
        | e1, Ast.Ng(e2) -> Ast.Ng(eval(Ast.Op(Ast.Time(e1, e2))))
        | Ast.Const(c1), Ast.Op(Ast.Time(Ast.Const(c2),e1)) -> Ast.Op(Ast.Time(Ast.Const(c1 * c2), e1))
        | Ast.Op(Ast.Time(Ast.Const(c), e2)), e3 -> Ast.Op(Ast.Time(Ast.Const(c), Ast.Op(Ast.Time(e2, e3))))
        | e1, Ast.Op(Ast.Time(Ast.Const(c), e3)) -> Ast.Op(Ast.Time(Ast.Const(c), Ast.Op(Ast.Time(e1, e3))))
        | Ast.Op(Ast.Divid(e1,e2)), Ast.Op(Ast.Divid(e3,e4)) -> Ast.Op(Ast.Divid(Ast.Op(Ast.Time(e1,e3)), Ast.Op(Ast.Time(e2,e4))))
        | _ ->
            if e1 = e2
            then Ast.Func(Ast.Pow(eval(e1), Ast.Const(2)))
            else Ast.Op(Ast.Time(eval(e1), eval(e2)))

    (* simplify (_ / _) *)
    | Ast.Op(Ast.Divid(e1, e2)) ->
        match (e1, e2) with
        | Ast.Const(0), _ -> Ast.Const(0)
        | e, Ast.Const(1) -> eval(e)
        | Ast.Ng(e1'), e2'
        | e1', Ast.Ng(e2') -> Ast.Ng(eval(Ast.Op(Ast.Divid(e1', e2'))))
        | Ast.Op(Ast.Divid(e1',e2')), e3' -> Ast.Op(Ast.Divid(e1', Ast.Op(Ast.Time(e2', e3'))))
        | e1', Ast.Op(Ast.Divid(e2',e3')) -> Ast.Op(Ast.Divid(Ast.Op(Ast.Time(e1', e3')), e2'))
        | _ ->
            if e1 = e2
            then Ast.Const(1)
            else Ast.Op(Ast.Divid(eval(e1), eval(e2)))

    (* simplify (_^_) *)
    | Ast.Func(Ast.Pow(e1,e2)) ->
        match (e1, e2) with
        | Ast.Const(0), _ -> Ast.Const(0)
        | Ast.Const(1), _ -> Ast.Const(1)
        | _, Ast.Const(0) -> Ast.Const(1)
        | _, Ast.Const(1) -> eval(e1)
        | _ -> Ast.Func(Ast.Pow(eval(e1), eval(e2)))

     (* simplify (sqrt _) *)
    | Ast.Func(Ast.Sqrt(e)) ->
        match e with
        | Ast.Const(0) -> Ast.Const(0)
        | Ast.Const(1) -> Ast.Const(1)
        | _ -> Ast.Func(Ast.Sqrt(eval(e)))

    (* simplify (e^_) *)
    | Ast.Func(Ast.Exp(e)) ->
        match e with
        | Ast.Const(0) -> Ast.Const(1)
        | Ast.Const(1) -> Ast.Var("\\e")
        | _ -> Ast.Func(Ast.Exp(eval(e)))

    (* simplify other function *)
    | Ast.Func(Ast.Ln(e)) -> Ast.Func(Ast.Ln(eval(e)))
    | Ast.Func(Ast.Sin(e)) -> Ast.Func(Ast.Sin(eval(e)))
    | Ast.Func(Ast.Asin(e)) -> Ast.Func(Ast.Asin(eval(e)))
    | Ast.Func(Ast.Cos(e))  -> Ast.Func(Ast.Cos(eval(e)))
    | Ast.Func(Ast.Acos(e)) -> Ast.Func(Ast.Acos(eval(e)))
    | Ast.Func(Ast.Tan(e))  -> Ast.Func(Ast.Tan(eval(e)))
    | Ast.Func(Ast.Atan(e)) -> Ast.Func(Ast.Atan(eval(e)))

    | _ -> e


let rec iter (n: int) (e: Ast.T) : Ast.T =
  if n = 0 
  then e
  else let e' = eval e in
        if e = e'
        then e
        else iter (n - 1) e'