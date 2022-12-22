open Batteries
open Front.Ast

exception TypeError of ty * ty
exception NotInScope of string
exception TypeMismatch of ty * ty

(* Context we perform type checking inside *)
type context = (string, ty) Hashtbl.t

(* Check if two types are equal *)
let are_types_equal (t1 : ty) (t2 : ty) = t1 = t2

let check_lit_type ctx (lit : literal) (t : ty) =
  match (lit, t) with
  | AInt _, TInt -> ()
  | AFloat _, TFloat -> ()
  | AStr _, TString -> ()
  | _ -> ()

let rec check_expr ctx (exp : expr) (t : ty) =
  match (exp, t) with
  | Lit lit, t -> check_lit_type ctx lit t
  | exp, t ->
      let synth_ty = synth_expr ctx exp in
      if are_types_equal t synth_ty then ()
      else raise (TypeMismatch (t, synth_ty))

(* Synthesize the type of an expression *)
and synth_expr ctx (exp : expr) =
  match exp with
  | Lit lit -> (
      match lit with AInt _ -> TInt | AFloat _ -> TFloat | AStr _ -> TString)
  | Ident _ -> TIdent
  | BinOp (e1, op, e2) -> (
      let t1 = synth_expr ctx e1 in
      let t2 = synth_expr ctx e2 in
      match (t1, op, t2) with
      | TInt, Op_Plus, TInt -> TInt
      | TInt, Op_Minus, TInt -> TInt
      | TInt, Op_Star, TInt -> TInt
      | TInt, Op_Slash, TInt -> TInt
      | _ -> TDefault)
  (* Unhandled expression *)
  | _ -> TDefault
