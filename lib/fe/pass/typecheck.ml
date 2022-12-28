(*
 * Perform bidirectional type checking on the generated AST to ensure
 * we have a well-typed program 
 *)

open Batteries
open Fe.Ast

exception TypeError of string * ty
exception NotInScope of string
exception TypeMismatch of string * ty * ty

(* Context we perform type checking inside *)
type context = {
  var_table : (string, ty) Hashtbl.t;
  func_table : (string, ty list) Hashtbl.t;
}

(* Create and return a empty context *)
let new_context =
  { var_table = Hashtbl.create 20; func_table = Hashtbl.create 20 }

let type_err why (t1 : ty) = raise (TypeError (why, t1))
let mismatch why (t1 : ty) (t2 : ty) = raise (TypeMismatch (why, t1, t2))

let are_types_compatible (t1 : ty) (t2 : ty) =
  match (t1, t2) with
  | _, TAny -> true (* t1 = Any is true, since Any can represent any type*)
  | t1, t2 -> t1 = t2

let string_of_type = function
  | TBool -> "bool"
  | TInt -> "int"
  | TFloat -> "float"
  | TString -> "string"
  | TAny -> "any"
  | TNeedsInfer -> "<needs infer>"

let check_lit_type _ (lit : literal) (t : ty) =
  match (lit, t) with
  | LitInt _, TInt -> ()
  | LitFloat _, TFloat -> ()
  | LitStr _, TString -> ()
  | _ -> ()

let lookup_var ctx name =
  match Hashtbl.find_option ctx.var_table name with
  | Some ty -> ty
  | None -> raise (NotInScope name)

let rec check_expr ctx (exp : expr) (t : ty) =
  match (exp, t) with
  | _, TAny -> () (* anything can be checked against Any *)
  | Lit lit, t -> check_lit_type ctx lit t
  | exp, t ->
      let synth_ty = infer ctx exp in
      if are_types_compatible t synth_ty then ()
      else mismatch "Types are not compatible" t synth_ty

and check_stmt ctx (stmt : stmt) =
  match stmt with
  | VarAssign (name, exp) ->
      let t1 = infer ctx exp in
      let t2 = lookup_var ctx name in
      if are_types_compatible t1 t2 then Hashtbl.replace ctx.var_table name t1
      else
        mismatch (Printf.sprintf "Mismatched re-assignment between types") t1 t2
  | ShortVarDecl (name, exp) ->
      (* Type inference *)
      let exp_ty = infer ctx exp in
      Hashtbl.add ctx.var_table name exp_ty
  | VarDecl (ty, name, exp) -> Hashtbl.add ctx.var_table name ty
  | If (cond, _, _) ->
      let cond_ty = infer ctx cond in
      if cond_ty == TBool || cond_ty == TAny then ()
      else type_err "Expected a boolean condition" cond_ty
  | For (name, exp, _) ->
      Hashtbl.add ctx.var_table name TString;
      let name_ty = lookup_var ctx name in
      let exp_ty = infer ctx exp in
      if name_ty == TString then () else type_err "Expected a name" exp_ty
  | While (cond, _) ->
      let cond_ty = infer ctx cond in
      if cond_ty == TBool then ()
      else type_err "Expected a boolean condition" cond_ty
  (* TODO: type check function calls *)
  | FuncCall call -> ()

and check_block ctx (Block stmts : block) =
  List.iter (fun a -> check_stmt ctx a) stmts

and check_top_level ctx (tl : top_level) =
  match tl with
  | Import _ -> ()
  | FuncDefn (_, params, block) -> check_block ctx block
  | Stmt stmt -> check_stmt ctx stmt

and check_program ctx (Program tl) = List.iter (check_top_level ctx) tl

and infer ctx (exp : expr) =
  match exp with
  | Lit lit -> (
      match lit with
      | LitBool _ -> TBool
      | LitInt _ -> TInt
      | LitFloat _ -> TFloat
      | LitStr _ -> TString)
  | Ident name -> lookup_var ctx name
  | BinOp (e1, op, e2) -> (
      let t1 = infer ctx e1 in
      let t2 = infer ctx e2 in
      match (t1, op, t2) with
      | TInt, (OPlus | OMinus | OStar | OSlash), TInt -> TInt
      | TString, OPlus, TString -> TString
      | TInt, (OEq | ONe | OLt | OGt | OLtEq | OGtEq), TInt -> TBool
      | TAny, (OEq | ONe | OLt | OGt | OLtEq | OGtEq), TBool -> TBool
      | TBool, (OEq | ONe | OLt | OGt | OLtEq | OGtEq), TBool -> TBool
      | _ ->
          type_err
            (Printf.sprintf
               "Unable to infer type for this binop, maybe it is not \
                implemented yet?\n\
                While trying to infer types for: %s and %s" (string_of_type t1)
               (string_of_type t2))
            t1)
  (* TODO: add lists *)
  | List xs -> TAny
  (* TODO: type check function calls *)
  | FuncCall (name, params) -> TAny
  | t -> type_err "Unimplemented type" (infer ctx t)
