open Batteries
open Scope
open Frontend.Ast

exception TypeError of string * ty
exception TypeMismatch of string * ty * ty

let type_err why t = raise (TypeError (why, t))
let type_mismatch why t1 t2 = raise (TypeMismatch (why, t1, t2))

let are_types_compatible t1 t2 =
  match (t1, t2) with
  | _, TAny -> true
  | t1, t2 -> t1 = t2

let check_lit ctx (lit : literal) t =
  match (lit, t) with
  | LitBool _, TBool -> ctx
  | LitInt _, TInt -> ctx
  | LitFloat _, TFloat -> ctx
  | LitStr _, TString -> ctx
  | _ -> raise (TypeError ("could not check against", t))

let rec check_expr ctx (exp : expr) t =
  match (exp, t) with
  | Ident name, _ ->
      if Scope.has_symbol ctx name then ctx else failwith "not in scope"
  | _, TAny -> ctx
  | Lit lit, t -> check_lit ctx lit t
  | exp, t ->
      let synth_ty = infer ctx exp in
      if are_types_compatible t synth_ty then ctx
      else type_mismatch "types are not compatible" t synth_ty

and check_stmt ctx (stmt : stmt) =
  match stmt with
  | VarAssign (name, val') ->
      let t1 = infer ctx val' in
      let t2 = Scope.lookup_symbol ctx name in
      if are_types_compatible t1 t2 then ctx
      else type_mismatch "type mismatch while re-assigning" t1 t2
  | ShortVarDecl (name, val') ->
      let synth_t = infer ctx val' in
      Scope.add_symbol ctx name synth_t
  | VarDecl (t, name, _) -> Scope.add_symbol ctx name t
  | If (cond, _, _) ->
      let cond_t = infer ctx cond in
      if cond_t = TBool || cond_t = TAny then ctx
      else type_err "expected boolean condition" cond_t
  | For (name, exp, _) ->
      let ctx' = Scope.add_symbol ctx name TString in
      let name_t = Scope.lookup_symbol ctx' name in
      let exp_t = infer ctx' exp in
      if name_t = TString then ctx' else type_err "expected a name" exp_t
  | While (cond, _) ->
      let cond_t = infer ctx cond in
      if cond_t = TBool then ctx
      else type_err "expected boolean condition" cond_t
  | Return _ -> ctx
  | Expr exp ->
      let synth_t = infer ctx exp in
      check_expr ctx exp synth_t

and check_top_level ctx (tl : top_level) =
  match tl with
  | Import path -> ctx
  | FuncDefn (ret_t, name, params, (Block stmts : block)) ->
      let param_tys = List.map (fun (_, a) -> a) params in
      let ctx' =
        List.fold_left (fun m (k, v) -> Scope.add_symbol m k v) ctx params
      in
      (* check the statements inside the function scope combined w/ the parameters *)
      let _ = List.fold_left check_stmt ctx' stmts in

      (* return only the outer scope, so we dont leak parameters *)
      let scoped = Scope.add_symbol ctx name (TFunction (param_tys, ret_t)) in
      scoped
  | Stmt stmt -> check_stmt ctx stmt

and check_program ctx (Program tl) =
  List.fold_left (fun m e -> check_top_level m e) ctx tl

(* Type inference *)
and infer_lit = function
  | LitBool _ -> TBool
  | LitInt _ -> TInt
  | LitFloat _ -> TFloat
  | LitStr _ -> TString

and infer_call ctx name params =
  match Scope.lookup_symbol ctx name with
  | TFunction (param_tys, ret_ty) ->
      if List.length param_tys = List.length params then
        let _ =
          List.map
            (fun (a, t) -> check_expr ctx a t)
            (List.combine params param_tys)
        in
        ret_ty
      else type_err "too few or too many parameters" ret_ty
  | t -> type_err "expected a function call" t

and infer ctx (exp : expr) =
  match exp with
  | Lit lit -> infer_lit lit
  | Ident name -> Scope.lookup_symbol ctx name
  | BinOp (e1, op, e2) -> (
      let t1 = infer ctx e1 in
      let t2 = infer ctx e2 in
      match (t1, op, t2) with
      | TInt, (OPlus | OMinus | OStar | OSlash), TInt -> TInt
      | TString, OPlus, TString -> TString
      | _ ->
          type_err
            (Printf.sprintf "unable to infer type for %s and %s"
               (Pretty.string_of_type t1) (Pretty.string_of_type t2))
            t1)
  | List _ -> TAny (* FIXME: fix type checking for lists *)
  | FuncCall (name, params) -> infer_call ctx name params