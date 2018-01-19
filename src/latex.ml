
let rec eval = function
  | Ast.Null      -> ""
  | Ast.Int(e)    -> "\\int{" ^ eval(e) ^ "}dx"
  | Ast.Ng(e)     -> "{-{" ^ eval(e) ^ "}}"
  | Ast.Const(c)  -> string_of_int(c)
  | Ast.Var(s)    -> s
  | Ast.Op(e)     -> eval_op(e)
  | Ast.Func(e)   -> eval_func(e)

and eval_op = function
  | Ast.Plus(e1, e2)  -> "{" ^ eval(e1) ^ "}+{" ^ eval(e2) ^ "}" 
  | Ast.Minus(e1, e2) -> "{" ^ eval(e1) ^ "}-{" ^ eval(e2) ^ "}"
  | Ast.Time(e1, e2)  -> "{" ^ eval(e1) ^ "}{" ^ eval(e2) ^ "}"
  | Ast.Divid(e1, e2) -> "\\frac{" ^ eval(e1) ^ "}{" ^ eval(e2) ^ "}"

and eval_func = function
  | Ast.Pow(e1,e2) -> "{" ^ eval(e1) ^ "}^{" ^ eval(e2) ^ "}"
  | Ast.Sqrt(e)    -> "\\sqrt{" ^ eval(e) ^ "}"
  | Ast.Exp(e)     -> "\\e^{"   ^ eval(e) ^ "}"
  | Ast.Ln(e)      -> "\\log{"  ^ eval(e) ^ "}"
  | Ast.Sin(e)     -> "\\sin{"  ^ eval(e) ^ "}"
  | Ast.Asin(e)    -> "\\asin{" ^ eval(e) ^ "}"
  | Ast.Cos(e)     -> "\\cos{"  ^ eval(e) ^ "}"
  | Ast.Acos(e)    -> "\\acos{" ^ eval(e) ^ "}"
  | Ast.Tan(e)     -> "\\tan{"  ^ eval(e) ^ "}"
  | Ast.Atan(e)    -> "\\atan{" ^ eval(e) ^ "}"
