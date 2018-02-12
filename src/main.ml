
let limit = ref 1000

let _ =
  Random.self_init ();

  print_endline "==== ==== ==== CREATE INTEGRAL ==== ==== ====";

  let e = Randomize.expr 3 |> Simplify.iter !limit in

  print_endline "---- ---- ---- Type ---- ---- ----";
  print_endline(Ast.show(e));

  print_endline "---- ---- ---- LaTeX ---- ---- ----";
  print_endline(Latex.eval(e));

  print_endline "---- ---- ---- Diff ---- ---- ----";
  let r = Differential.cul e |> Simplify.iter !limit in
  print_endline(Latex.eval(r));
  
  print_endline "";
