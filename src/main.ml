  
let _ =
  Random.init (int_of_float (10000.0 *. (Unix.gettimeofday())));

  print_endline "==== ==== ==== CREATE INTEGRAL ==== ==== ====";

  let test_1 = Randomize.expr 3 in

  print_endline "---- ---- ---- Type ---- ---- ----";
  print_endline(Ast.show(test_1));
  print_endline "---- ---- ---- LaTeX ---- ---- ----";
  print_endline(Eval.eval(test_1));

  print_endline "";
