open Batteries
open Front.Ast

exception TypeError of ty * ty
exception NotInScope of string
exception TypeMismatch of ty * ty

(* Context we perform type checking inside *)
type context = (string, ty) Hashtbl.t

let are_types_compatible (t1 : ty) (t2 : ty) =
  match (t1, t2) with
  | _, TAny -> true (* t1 = Any is true, since Any can represent any type*)
  | t1, t2 -> t1 = t2

let check_lit_type _ (lit : literal) (t : ty) =
  match (lit, t) with
  | AInt _, TInt -> ()
  | AFloat _, TFloat -> ()
  | AStr _, TString -> ()
  | _ -> ()

let lookup_var ctx name =
  match Hashtbl.find_option ctx name with
  | Some ty -> ty
  | None -> raise (NotInScope name)

let rec check_expr ctx (exp : expr) (t : ty) =
  match (exp, t) with
  | _, TAny -> () (* anything can be checked against Any *)
  | Lit lit, t -> check_lit_type ctx lit t
  | exp, t ->
      let synth_ty = synth_expr ctx exp in
      if are_types_compatible t synth_ty then ()
      else raise (TypeMismatch (t, synth_ty))

and check_stmt ctx (stmt : stmt) (t : ty) =
  match stmt with
  | VarAssign (name, exp) -> Hashtbl.add ctx name (synth_expr ctx exp)
  | VarDecl (TUntyped, name, exp) ->
      (* Type inference *)
      let exp_ty = synth_expr ctx exp in
      Hashtbl.add ctx name exp_ty
  | VarDecl (ty, name, exp) ->
      let exp_ty = synth_expr ctx exp in
      if ty == exp_ty then Hashtbl.add ctx name ty
      else raise (TypeMismatch (t, exp_ty))
  | If (cond, _, _) ->
      let cond_ty = synth_expr ctx cond in
      if cond_ty == TBool then () else raise (TypeMismatch (t, cond_ty))
  | For (name, exp, _) ->
      let name_ty = lookup_var ctx name in
      let exp_ty = synth_expr ctx exp in
      if name_ty == TString then () else raise (TypeMismatch (t, exp_ty))
  | While (cond, _) ->
      let cond_ty = synth_expr ctx cond in
      if cond_ty == TBool then () else raise (TypeMismatch (t, cond_ty))

and synth_expr ctx (exp : expr) =
  match exp with
  | Lit lit -> (
      match lit with
      | AInt _ -> TInt
      | AFloat _ -> TFloat
      | AStr _ -> TString)
  | Ident name -> lookup_var ctx name
  | BinOp (e1, op, e2) -> (
      let t1 = synth_expr ctx e1 in
      let t2 = synth_expr ctx e2 in
      match (t1, op, t2) with
      | TInt, (Op_Plus | Op_Minus | Op_Star | Op_Slash), TInt -> TInt
      | TBool, (Op_Eq | Op_NotEq | Op_Lt | Op_Gt | Op_LtEq | Op_GtEq), TBool ->
          TBool
      | _ -> TUntyped)
  (* TODO: add lists *)
  | List xs -> TUntyped
  | _ -> TUntyped
