open Batteries
open Fe.Ast

exception NotInScope of string
exception TypeError of string * ty
exception TypeMismatch of string * ty * ty

type context = { vars : (string, ty) Hashtbl.t }

let new_context = { vars = Hashtbl.create 20 }
let type_err why t = raise (TypeError (why, t))
let type_mismatch why t1 t2 = raise (TypeMismatch (why, t1, t2))

let are_types_compatible t1 t2 =
  match (t1, t2) with
  | _, TAny -> true
  | t1, t2 -> t1 = t2

let rec string_of_type = function
  | TBool -> "bool"
  | TInt -> "int"
  | TFloat -> "float"
  | TString -> "string"
  | TAny -> "<any>"
  | TFunction (param_tys, ret_ty) ->
      Printf.sprintf "<fn %s -> %s>"
        (String.concat "," (List.map string_of_type param_tys))
        (string_of_type ret_ty)
  (* at this point, there is not enough information to infer the type
     so it will need to be inferred later on *)
  | TNeedsInfer -> "<needs infer>"

let lookup_var tbl name =
  match Hashtbl.find_option tbl name with
  | Some ty -> ty
  | None -> raise (NotInScope name)

let check_lit _ (lit : literal) t =
  match (lit, t) with
  | LitBool _, TBool -> ()
  | LitInt _, TInt -> ()
  | LitFloat _, TFloat -> ()
  | LitStr _, TString -> ()
  | _ -> raise (TypeError ("could not check against", t))

let rec check_expr ctx (exp : expr) t =
  match (exp, t) with
  | _, TAny -> ()
  | Lit lit, t -> check_lit ctx lit t
  | exp, t ->
      let synth_ty = infer ctx exp in
      if are_types_compatible t synth_ty then ()
      else type_mismatch "types are not compatible" t synth_ty

and check_stmt ctx (stmt : stmt) =
  match stmt with
  | VarAssign (name, val') ->
      let t1 = infer ctx val' in
      let t2 = lookup_var ctx.vars name in
      if are_types_compatible t1 t2 then Hashtbl.replace ctx.vars name t1
      else type_mismatch "type mismatch while re-assigning" t1 t2
  | ShortVarDecl (name, val') ->
      let synth_t = infer ctx val' in
      Hashtbl.add ctx.vars name synth_t
  | VarDecl (t, name, _) -> Hashtbl.add ctx.vars name t
  | If (cond, _, _) ->
      let cond_t = infer ctx cond in
      if cond_t = TBool || cond_t = TAny then ()
      else type_err "expected boolean condition" cond_t
  | For (name, exp, _) ->
      Hashtbl.add ctx.vars name TString;
      let name_t = lookup_var ctx.vars name in
      let exp_t = infer ctx exp in
      if name_t = TString then () else type_err "expected a name" exp_t
  | While (cond, _) ->
      let cond_t = infer ctx cond in
      if cond_t = TBool then ()
      else type_err "expected boolean condition" cond_t
  | Return _ -> ()
  | Expr exp ->
      let synth_t = infer ctx exp in
      check_expr ctx exp synth_t

and check_top_level ctx (tl : top_level) =
  match tl with
  | Import path -> ()
  | FuncDefn (ret_t, name, params, (Block stmts : block)) ->
      List.iter (fun a -> check_stmt ctx a) stmts
  | Stmt stmt -> check_stmt ctx stmt

and check_program ctx (Program tl) = List.iter (check_top_level ctx) tl

(* Type inference *)

and infer_lit = function
  | LitBool _ -> TBool
  | LitInt _ -> TInt
  | LitFloat _ -> TFloat
  | LitStr _ -> TString

and infer_call ctx name params =
  match lookup_var ctx.vars name with
  | TFunction (param_tys, ret_ty) ->
      (* check the length of the params *)
      if List.length param_tys = List.length params then
        let _ =
          List.iter
            (fun (a, t) -> check_expr ctx a t)
            (List.combine params param_tys)
        in
        ret_ty
      else type_err "too few or too many parameters" ret_ty
  | t -> type_err "expected a function call" t

and infer ctx (exp : expr) =
  match exp with
  | Lit lit -> infer_lit lit
  | Ident name -> lookup_var ctx.vars name
  | BinOp (e1, op, e2) -> (
      let t1 = infer ctx e1 in
      let t2 = infer ctx e2 in
      match (t1, op, t2) with
      | TInt, (OPlus | OMinus | OStar | OSlash), TInt -> TInt
      | TString, OPlus, TString -> TString
      | _ ->
          type_err
            (Printf.sprintf "unable to infer type for %s and %s" (string_of_type t1)
               (string_of_type t2))
            t1)
  | List _ -> TAny (* FIXME: fix type checking for lists *)
  | FuncCall (name, params) -> infer_call ctx name params
