  
let _ =
  Random.init (int_of_float (10000.0 *. (Unix.gettimeofday())));

  print_endline "==== ==== ==== CREATE INTEGRAL ==== ==== ====";

  let e = Randomize.expr 3 in

  print_endline "---- ---- ---- Type ---- ---- ----";
  print_endline(Ast.show(e));
  print_endline "---- ---- ---- LaTeX ---- ---- ----";
  print_endline(Eval.eval(e));
  print_endline "---- ---- ---- Diff ---- ---- ----";
  print_endline(Eval.eval(Differential.cul(e)));
  
  print_endline "";
