type t = CONST | VAR | OP | FUNC
         | PLUS | MINUS | TIME | DIVID
         | EXP | LN | SIN | ASIN | COS | ACOS | TAN | ATAN

let g_lst    : t list = [ CONST; VAR; OP; FUNC ] 
let op_lst   : t list = [ PLUS; MINUS; TIME; DIVID ] 
let func_lst : t list = [ EXP; LN; SIN; ASIN; COS; ACOS; TAN; ATAN ] 

let random_get (l: t list) : t = List.nth l (Random.int (List.length l))
  
let rec expr_randomize (depth: int) : Ast.t =
  if depth <> 0 then
    begin match (random_get g_lst) with
    | CONST -> let c = 1 + (Random.int 9) in
               Ast.Const(string_of_int(c))

    | VAR   -> Ast.Var("x")

    | OP -> begin
        let e1 = expr_randomize (depth - 1) in
        let e2 = expr_randomize (depth - 1) in
        begin match (random_get op_lst) with
        | PLUS  -> Ast.Op(Ast.Plus(e1, e2))
        | MINUS -> Ast.Op(Ast.Minus(e1, e2))
        | TIME  -> Ast.Op(Ast.Time(e1, e2))
        | DIVID -> Ast.Op(Ast.Divid(e1, e2))
        | _     -> Ast.Null
        end
      end

    | FUNC -> begin
        let e1 = expr_randomize (depth - 1) in
        begin match (random_get func_lst) with
        | EXP   -> Ast.Func(Ast.Exp(e1))
        | LN    -> Ast.Func(Ast.Ln(e1))
        | SIN   -> Ast.Func(Ast.Sin(e1))
        | ASIN  -> Ast.Func(Ast.Asin(e1))
        | COS   -> Ast.Func(Ast.Cos(e1))
        | ACOS  -> Ast.Func(Ast.Acos(e1))
        | TAN   -> Ast.Func(Ast.Tan(e1))
        | ATAN  -> Ast.Func(Ast.Atan(e1))
        | _     -> Ast.Null
        end
      end

    | _ -> Ast.Null
    end
  else
    begin match (random_get [CONST; VAR]) with
    | CONST -> let c = 1 + (Random.int 9) in
               Ast.Const(string_of_int(c))
    | VAR   -> Ast.Var("x")
    | _     -> Ast.Null
    end
             
let expr (depth: int) : Ast.t =
  let e = expr_randomize(depth) in
  Ast.Int(e)
  
  
