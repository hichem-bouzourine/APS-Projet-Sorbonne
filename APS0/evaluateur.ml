open Ast

(*E(Env) = ident(key string) -> V(value)*)
module Env = Map.Make(String)
  (*update environement Env.add*)

  type primOp =    (* <-- inspiré d'un étudiant dans la salle TME que je remercie pour son aide*)
  | NOT
  | EQ 
  | LT 
  | ADD 
  | SUB 
  | MUL 
  | DIV 

(*-V = Z ⊕ F ⊕ FR-*)
type value = 
  InZ of int (*-Z = valeurs immediates*)
  | InF of singleExpr * string list * value Env.t (*-F(InF) = Expr(string) × ident∗(string list) × E(value Env.k)-*)
  | InFR of singleExpr * string * string list * value Env.t (*-FR(InFR) = Expr(string) × ident(string) × ident∗(string list) × E(value Env.k)-*)
  | InPrim of primOp
  

(*-O(output_stream) = Z∗(int list)-*)
type output_stream = int list

(*Fonctions primitives*)
let eval_prim primOp args = 
  match primOp, args with
  (* Opérateurs unaires *)
  | "not", [InZ n] -> InZ (if n = 0 then 1 else 0)
  (* Opérateurs binaires *)
  | "eq", [InZ n1; InZ n2] -> InZ (if n1 = n2 then 1 else 0)
  | "lt", [InZ n1; InZ n2] -> InZ (if n1 < n2 then 1 else 0)
  | "add", [InZ n1; InZ n2] -> InZ (n1 + n2)
  | "sub", [InZ n1; InZ n2] -> InZ (n1 - n2)
  | "mul", [InZ n1; InZ n2] -> InZ (n1 * n2)
  (* Gestion des erreurs pour les opérateurs non supportés *)
  | _ -> failwith "Opérateur ou arguments non pris en charge"

let rec eval_expr x env = 
  match x with 
  | ASTNum n -> InZ n (* Construction de la valeur immédiate *)
  | ASTId id -> (try Env.find id env with Not_found -> failwith "Variable non définie dans l'environnement")
  (* Gestion des opérateurs logiques et de contrôle *)
  | ASTAnd (e1, e2) ->
      let v1 = eval_expr e1 env in
      if v1 = InZ 1 then eval_expr e2 env else InZ 0
  | ASTOr (e1, e2) ->
      let v1 = eval_expr e1 env in
      if v1 = InZ 1 then InZ 1 else eval_expr e2 env
  | ASTIf (e1, e2, e3) ->
      let v1 = eval_expr e1 env in
      if v1 = InZ 1 then eval_expr e2 env else eval_expr e3 env
  (* Cas de l'abstraction de fonction *)
  | ASTLambdaExpression (args, body) -> InF (body, args, env)
  (* Cas de l'application de fonction *)
  | ASTApp (func, args) -> 
      let func_value = eval_expr func env in
        (match func_value with
        | InPrim _ -> eval_prim func args (* Évaluation des opérateurs unaires et binaires avec eval_prim *)
        | InF (body, params, env') ->
            let args_values = List.map (fun arg -> eval_expr arg env) args in
            let new_env = List.fold_left2 (fun acc param arg_value -> Env.add param arg_value acc) env' params args_values in
            eval_expr body new_env
        | _ -> failwith "Impossible d'appeler une fonction qui n'est pas une fermeture")
  

let rec eval_stat ins flx env = 
  match ins with
  (* Évaluation de l'instruction ECHO *)
  | ASTEcho expr ->
      let result = eval_expr expr env in
      (* Ajout du résultat à la sortie *)
      result :: flx

let rec eval_def def env = 
  match def with
  (* Évaluation de la définition CONST *)
  | ASTConst (x, _, expr) ->
      let result = eval_expr expr env in
      (* Mise à jour de l'environnement avec la valeur calculée *)
      Env.add x result env
  (* Évaluation de la définition FUN *)
  | ASTFun (x, _, args, expr) ->
      let closure = InF (expr, args, env) in
      (* Mise à jour de l'environnement avec la fermeture calculée *)
      Env.add x closure env
  (* Évaluation de la définition FUN REC *)
  | ASTFunRec (x, _, args, expr) ->
      let recursion = InFR (expr, x, args, env) in
      (* Mise à jour de l'environnement avec la fermeture récursive calculée *)
      Env.add x recursion env

let rec eval_cmds cmds env flx =
  match cmds with
  | [] -> flx (* Aucune commande restante, renvoyer la sortie *)
  | cmd :: rest_cmds ->
      (match cmd with
      | ASTStat stat ->
          let new_flx = eval_stat stat flx env in
          eval_cmds rest_cmds env new_flx
      | ASTDef (def, more_cmds) ->
          let new_env = eval_def def env in
          eval_cmds more_cmds new_env flx)

let rec eval_prog p =
  match p with
  | [] -> [] (* Aucune commande à évaluer, renvoyer une liste vide *)
  | cmds :: rest_prog ->
      let final_output = eval_cmds cmds Env.empty [] in
      final_output :: eval_prog rest_prog
;;

let fname = Sys.argv.(1) in (* <-- inspiré d'un étudiant dans la salle TME *)
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    let _ = eval_prog p in
    Printf.printf "Evaluation terminée avec succès.\n"
  with
  | Lexer.Eof -> Printf.printf "Erreur : fin de fichier inattendue.\n"
  | Failure msg -> Printf.printf "Erreur : %s\n" msg
  | Parsing.Parse_error -> Printf.printf "Erreur de syntaxe.\n"
  | Sys_error msg -> Printf.printf "Erreur système : %s\n" msg
  | exn -> Printf.printf "Erreur inattendue : %s\n" (Printexc.to_string exn)
  finally close_in ic