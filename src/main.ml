
let limit = ref 1000

let _ =
  Random.init (int_of_float (10000.0 *. (Unix.gettimeofday())));

  print_endline "==== ==== ==== CREATE INTEGRAL ==== ==== ====";

  let e = Randomize.expr 3 |> Simplify.iter !limit in

  print_endline "---- ---- ---- Type ---- ---- ----";
  print_endline(Ast.show(e));

  print_endline "---- ---- ---- LaTeX ---- ---- ----";
  print_endline(Latex.eval(e));

  print_endline "---- ---- ---- Diff ---- ---- ----";
  let r = Differential.cul e |> Simplify.iter !limit |>  Latex.eval in
  print_endline(r);
  
  print_endline "";
