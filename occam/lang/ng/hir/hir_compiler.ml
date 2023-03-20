open Common_ast
open Hir

exception CompileError

let bind_index sym exp index =
  HBindVal (sym, None, Some (HGetIndex (HRef exp, index)))

let bind_obj sym obj = HBindVal (sym, None, Some obj)

let rec compile_exp = function
  | IntegerValue v -> HInt v
  | StringValue v -> HStr v
  | BooleanValue v -> HBool v
  | UnitLiteral -> HUnitValue
  | ArrayLiteral exps -> HArray (UnknownType, List.map compile_exp exps)
  | TupleLiteral exps ->
      HTuple (List.map (fun exp -> (UnknownType, compile_exp exp)) exps)
  | IdentityExpr id -> HRef id
  | Assignment (id, exp) -> HAssign (id, compile_exp exp)
  | BinaryOperation (op, lexp, rexp) ->
      HBinOp (op, compile_exp lexp, compile_exp rexp)
  | AccessorExpr (expr, id) -> HAccess (compile_exp expr, id)
  | ApplicationExpr (expr, args) ->
      HApply (compile_exp expr, List.map compile_exp args)
  | IfElseExpr (cond, conseq, alter) ->
      HCondExpr (compile_exp cond, compile_exp conseq, compile_exp alter)
  | LoopCollectExpr (looper, collector) ->
      HLoopCollect (compile_exp looper, compile_exp collector)
  | LetBindingExpr (bindings, expr) ->
      let new_scope_bindings = compile_value_def_bindings [] bindings in
      HNewScope (new_scope_bindings, compile_exp expr)
  (* todo: add all expr types *)
  | _ -> raise CompileError

and compile_binding_pattern hobj =
  let mangled = Name.mangling_name () in
  function
  | DirectBinding id -> [ bind_obj id hobj ]
  | DestructTupleBinding (_, patterns) | ListBinding patterns ->
      let binded_mangled = [ bind_obj mangled hobj ] in
      compile_destructions binded_mangled mangled patterns 0
  | WildcardBinding | UnitBinding -> [ HSimple hobj ]
  | _ -> raise CompileError

and compile_destructions cmds exp patterns index =
  match patterns with
  | DirectBinding id :: rest ->
      compile_destructions
        (cmds @ [ bind_index id exp (IndexAt index) ])
        exp rest (index + 1)
  | DestructTupleBinding (_, patterns) :: rest | ListBinding patterns :: rest ->
      let mangled = Name.mangling_index_at index in
      let mangled_mod =
        cmds @ [ bind_obj mangled (HGetIndex (HRef exp, IndexAt index)) ]
      in
      let inner_binded_hmod =
        compile_destructions mangled_mod mangled patterns 0
      in
      compile_destructions inner_binded_hmod exp rest (index + 1)
  | [ SpreadBinding x ] -> (
      match x with
      | Some id -> cmds @ [ bind_index id exp (index_starts_from index) ]
      | None -> cmds)
  | [] -> cmds
  | _ -> raise CompileError

and compile_value_binding = function
  | pat, exp -> compile_binding_pattern (compile_exp exp) pat

and compile_value_def_bindings cmds = function
  | [] -> cmds
  | binding :: rest ->
      compile_value_def_bindings (cmds @ compile_value_binding binding) rest

and compile_value_definiton hmod = function
  | ValueBinding bindings ->
      HMod.append_cmds (compile_value_def_bindings [] bindings) hmod
  | _ -> raise CompileError

and compile_fundef_body body cmds _ =
  match body with
  | FunStmtsBody stmts -> cmds @ List.map compile_statement stmts
  | FunExprBody expr -> cmds @ [ HSimple (compile_exp expr) ]

and compile_fundef patterns body hmod =
  let cmds = compile_destructions [] "arguments" patterns 0 in
  compile_fundef_body body cmds hmod

and compile_definition def hmod =
  match def with
  | ValueDef valdef -> compile_value_definiton hmod valdef
  | FunDef (FunctionDefinition (id, patterns, body)) ->
      let hfundef =
        HBindFn
          {
            name = id;
            arity = List.length patterns;
            body = compile_fundef patterns body hmod;
          }
      in
      HMod.append_cmd hfundef hmod
  | _ -> raise CompileError

and compile_statement stmt =
  match stmt with
  | SimpleStatement exp -> HSimple (compile_exp exp)
  | ReturnStatement exp -> HReturn (compile_exp exp)

and compile_program els hmod =
  match els with
  | [] -> hmod
  | h :: t -> (
      match h with
      | Definition def -> compile_program t (compile_definition def hmod)
      | Statement stmt ->
          compile_program t (HMod.append_cmd (compile_statement stmt) hmod))

let compile (Program els) = compile_program els HMod.empty
