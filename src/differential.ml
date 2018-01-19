
let rec cul (e: Ast.t) : Ast.t =
  match e with
  | Ast.Const(_) -> Ast.Const(0)
  | Ast.Ng(e)    -> Ast.Ng(cul(e))
  | Ast.Var(s)   -> Ast.Const(1)
  | Ast.Op(e)    -> cul_op(e)
  | Ast.Func(e)  -> cul_func(e)
  | _            -> Ast.Null

and cul_op (e: Ast.operator) : Ast.t =
  match e with
  | Ast.Plus(e1,e2)  -> let e1', e2' = cul(e1), cul(e2) in
                        Ast.Op(Ast.Plus(e1', e2'))

  | Ast.Minus(e1,e2) -> let e1', e2' = cul(e1), cul(e2) in
                        Ast.Op(Ast.Minus(e1', e2'))

  | Ast.Time(e1,e2)  -> let e1', e2' = cul(e1), cul(e2) in
                        let r1 = Ast.Op(Ast.Time(e1', e2)) in
                        let r2 = Ast.Op(Ast.Time(e1, e2')) in
                        Ast.Op(Ast.Plus(r1, r2))

  | Ast.Divid(e1,e2) -> let e1', e2' = cul(e1), cul(e2) in
                        let r1 = Ast.Op(Ast.Time(e1', e2)) in
                        let r2 = Ast.Op(Ast.Time(e1, e2')) in
                        let r3 = Ast.Func(Ast.Pow(e2, Ast.Const(2))) in
                        Ast.Op(Ast.Divid(Ast.Op(Ast.Minus(r1, r2)), r3))
                        
and cul_func (e: Ast.func) : Ast.t =
  match e with
  | Ast.Pow(e1, Ast.Const(n)) -> let e1' = cul(e1) in
                                 let e2' = Ast.Func(Ast.Pow(e1, Ast.Const(n-1))) in
                                 Ast.Op(Ast.Time(Ast.Const(n), (Ast.Op(Ast.Time(e1', e2')))))

  | Ast.Pow(e1, Ast.Var(s)) -> let e1' = Ast.Op(Ast.Time(Ast.Var(s), Ast.Func(Ast.Ln(e1)))) in
                               let e2' = Ast.Func(Ast.Exp(e1')) in
                               Ast.Op(Ast.Time(cul(e1'), e2'))

  | Ast.Sqrt(e) -> let e' = cul(e) in
                   let c = Ast.Op(Ast.Divid(Ast.Const(1), Ast.Const(2))) in
                   Ast.Op(Ast.Time(c, Ast.Op(Ast.Divid(e', Ast.Func(Ast.Sqrt(e))))))
                               
  | Ast.Exp(e) -> Ast.Op(Ast.Time(cul(e), Ast.Func(Ast.Exp(e))))

  | Ast.Ln(e)  -> let e' = cul(e) in
                  Ast.Op(Ast.Divid(e', e))

  | Ast.Sin(e) -> let e' = cul(e) in
                  Ast.Op(Ast.Time(e', Ast.Func(Ast.Cos(e))))

  | Ast.Asin(e) -> let e' = cul(e) in
                   let e2 = Ast.Func(Ast.Pow(e, Ast.Const(2))) in
                   let r1 = Ast.Func(Ast.Sqrt(Ast.Op(Ast.Minus(Ast.Const(1), e2)))) in
                   Ast.Op(Ast.Divid(e', r1))

  | Ast.Cos(e) -> let e' = cul(e) in
                  Ast.Op(Ast.Time(e', Ast.Ng(Ast.Func(Ast.Sin(e)))))

  | Ast.Acos(e) -> let e' = cul(e) in
                   let e2 = Ast.Func(Ast.Pow(e, Ast.Const(2))) in
                   let r1 = Ast.Func(Ast.Sqrt(Ast.Op(Ast.Minus(Ast.Const(1), e2)))) in
                   Ast.Op(Ast.Divid(Ast.Ng(e'), r1))

  | Ast.Tan(e)  -> let e' = cul(e) in
                   let e2 = Ast.Func(Ast.Pow(Ast.Func(Ast.Tan(e)), Ast.Const(2))) in
                   Ast.Op(Ast.Divid(e', e2))

  | Ast. Atan(e) -> let e' = cul(e) in
                    let e2 = Ast.Func(Ast.Pow(e, Ast.Const(2))) in
                    let r1 = Ast.Op(Ast.Plus(Ast.Const(1), e2)) in
                    Ast.Op(Ast.Divid(e', r1))
  | _ -> Ast.Null
