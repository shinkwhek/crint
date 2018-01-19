
let rec eval (e: Ast.t) : Ast.t =
  match e with
  | Ast.Var(s) -> Ast.Var(s)
  | Ast.Const(0) -> Ast.Const(0)
  | Ast.Ng(Ast.Const(0)) -> Ast.Const(0)
  | Ast.Ng(Ast.Ng(e)) -> eval(e)
  | Ast.Ng(e) -> Ast.Ng(eval(e))
                       
  | Ast.Op(Ast.Plus(e1, e2)) ->
     begin match e1, e2 with
     | Ast.Const(0), e | e, Ast.Const(0) -> e
     | Ast.Const(n1), Ast.Const(n2) -> Ast.Const(n1 + n2)
     | Ast.Var(s1), Ast.Var(s2) ->
        if s1 = s2 then
          Ast.Op(Ast.Time(Ast.Const(2), Ast.Var(s1)))
        else
          Ast.Op(Ast.Plus(Ast.Var(s1), Ast.Var(s2)))
     | _ ->
        if e1 = e2 then
          Ast.Op(Ast.Time(Ast.Const(2), eval(e1)))
        else
          Ast.Op(Ast.Plus(eval(e1), eval(e2)))
     end

  | Ast.Op(Ast.Minus(e1, e2)) ->
     begin match e1, e2 with
     | Ast.Const(0), e | e, Ast.Const(0) -> e
     | Ast.Const(n1), Ast.Const(n2) -> Ast.Const(n1 - n2)
     | _ ->
        if e1 = e2 then
          Ast.Const(0)
        else
          Ast.Op(Ast.Minus(eval(e1), eval(e2)))
     end
    
  | Ast.Op(Ast.Time(e1, e2)) ->
     begin match e1, e2 with
     | Ast.Const(0), e | e, Ast.Const(0) -> Ast.Const(0)
     | Ast.Const(1), e | e, Ast.Const(1) -> e
     | Ast.Const(n1), Ast.Const(n2) -> Ast.Const(n1 * n2)
     | _ ->
        if e1 = e2 then
          Ast.Func(Ast.Pow(eval(e1), Ast.Const(2)))
        else
          Ast.Op(Ast.Time(eval(e1), eval(e2)))
     end
    
  | Ast.Op(Ast.Divid(e1, e2)) ->
     begin match e1, e2 with
     | Ast.Const(0), _ -> Ast.Const(0)
     | e, Ast.Const(1) -> eval(e)
     | e1, Ast.Ng(e2) -> Ast.Ng(eval(Ast.Op(Ast.Divid(e1,e2))))
     | _ ->
        if e1 = e2 then
          Ast.Const(1)
        else
          Ast.Op(Ast.Divid(eval(e1), eval(e2)))
     end
  | Ast.Func(Ast.Pow(e1,e2)) ->
     begin match e1, e2 with
     | _, Ast.Const(0) -> Ast.Const(1)
     | e, Ast.Const(1) -> eval(e)
     | _ -> Ast.Func(Ast.Pow(eval(e1), eval(e2)))
     end
  | Ast.Func(Ast.Sqrt(e)) ->
     begin match e with
     | Ast.Const(0) -> Ast.Const(0)
     | Ast.Const(1) -> Ast.Const(1)
     | _ -> Ast.Func(Ast.Sqrt(eval(e)))
     end
  | Ast.Func(Ast.Exp(e)) ->
     begin match e with
     | Ast.Const(0) -> Ast.Const(1)
     | Ast.Const(1) -> Ast.Var("\\e")
     | _ -> Ast.Func(Ast.Exp(eval(e)))
     end
    
  | Ast.Func(Ast.Ln(e)) -> Ast.Func(Ast.Ln(eval(e)))
  | Ast.Func(Ast.Sin(e)) -> Ast.Func(Ast.Sin(eval(e)))
  | Ast.Func(Ast.Asin(e)) -> Ast.Func(Ast.Asin(eval(e)))
  | Ast.Func(Ast.Cos(e))  -> Ast.Func(Ast.Cos(eval(e)))
  | Ast.Func(Ast.Acos(e)) -> Ast.Func(Ast.Acos(eval(e)))
  | Ast.Func(Ast.Tan(e))  -> Ast.Func(Ast.Tan(eval(e)))
  | Ast.Func(Ast.Atan(e)) -> Ast.Func(Ast.Atan(eval(e)))

  | _ -> e


let rec iter (n: int) (e: Ast.t) : Ast.t =
  if n = 0 then
    e
  else
    let e' = eval e in
    if e = e' then
      e
    else
      iter (n - 1) e'
          
