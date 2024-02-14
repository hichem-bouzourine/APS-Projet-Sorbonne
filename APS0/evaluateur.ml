open Ast

(* Z : constructeur de type valeur *)

let rec eval_expr x env = 
  match x with 
  | ASTNum n -> Printf.printf"%d" n
  | _ -> Printf.printf "00"
