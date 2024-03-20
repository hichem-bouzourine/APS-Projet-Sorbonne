open Ast

(*E(Env) = ident(key String) -> V(value)*)
module Env = Map.Make(String)
  (*update environement Env.add*)

(*-V = Z ⊕ F ⊕ FR-*)
type value = 
  | inZ of int (*-Z = valeurs immediates-*)
  | inF of expr * String list * value Env.k (*-F(inF) = Expr × ident∗(String list) × E(value Env.k)-*)
  | inFR of expr * String * String list * value Env.k (*-FR(inFR) = Expr × ident(String) × ident∗(String list) × E(value Env.k)-*)
  
(*-O(output_stream) = Z∗(int list)-*)
type output_stream = int list

(*Fonctions primitives*)
let eval_prim prim args = 
  match prim, args with
  (* Opérateurs unaires *)
  | "not", [inZ n] -> inZ (if n = 0 then 1 else 0)
  (* Opérateurs binaires *)
  | "eq", [inZ n1; inZ n2] -> inZ (if n1 = n2 then 1 else 0)
  | "lt", [inZ n1; inZ n2] -> inZ (if n1 < n2 then 1 else 0)
  | "add", [inZ n1; inZ n2] -> inZ (n1 + n2)
  | "sub", [inZ n1; inZ n2] -> inZ (n1 - n2)
  | "mul", [inZ n1; inZ n2] -> inZ (n1 * n2)
  (* Gestion des erreurs pour les opérateurs non supportés *)
  | _ -> failwith "Opérateur ou arguments non pris en charge"

(* Z : constructeur de type valeur *)

let rec eval_expr x env = 
  match x with 
  | ASTNum n -> inZ n (* Construction de la valeur immédiate *)
  | ASTId id -> (try Env.find id env with Not_found -> failwith "Variable non définie dans l'environnement")
  (* Évaluation des opérateurs unaires et binaires avec eval_prim *)
  | ASTPrim1 (op, e) ->
      let v = eval_expr e env in
      eval_prim op [v] (* Appel de la fonction eval_prim avec un seul argument *)
  | ASTPrim2 (op, e1, e2) ->
      let v1 = eval_expr e1 env in
      let v2 = eval_expr e2 env in
      eval_prim op [v1; v2] (* Appel de la fonction eval_prim avec deux arguments *)
  (* Gestion des opérateurs logiques et de contrôle *)
  | ASTAnd (e1, e2) ->
      let v1 = eval_expr e1 env in
      if v1 = inZ 1 then eval_expr e2 env else inZ 0
  | ASTOr (e1, e2) ->
      let v1 = eval_expr e1 env in
      if v1 = inZ 1 then inZ 1 else eval_expr e2 env
  | ASTIf (e1, e2, e3) ->
      let v1 = eval_expr e1 env in
      if v1 = inZ 1 then eval_expr e2 env else eval_expr e3 env
  (* Cas de l'abstraction de fonction *)
  | ASTAbs (args, body) -> inF (body, args, env)
  (* Cas de l'application de fonction *)
  | ASTApp (func, args) ->
      let func_value = eval_expr func env in
      (match func_value with
      | inF (body, params, env') ->
          let args_values = List.map (fun arg -> eval_expr arg env) args in
          let new_env = List.fold_left2 (fun acc param arg_value -> Env.add param arg_value acc) env' params args_values in
          eval_expr body new_env
      | _ -> failwith "Impossible d'appeler une fonction qui n'est pas une fermeture")
  (* Cas de l'application de fonction récursive *)
  | ASTAppRec (func, args) ->
      let func_value = eval_expr func env in
      (match func_value with
      | inFR (body, recursive_name, params, env') ->
          let args_values = List.map (fun arg -> eval_expr arg env) args in
          let new_env = List.fold_left2 (fun acc param arg_value -> Env.add param arg_value acc) env' params args_values in
          let new_env_with_rec = Env.add recursive_name func_value new_env in
          eval_expr body new_env_with_rec
      | _ -> failwith "Impossible d'appeler une fonction récursive qui n'est pas une fermeture récursive")

