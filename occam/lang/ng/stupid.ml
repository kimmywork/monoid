open Common_ast

(* simplified testing ugly prototype intepreter (STUPI) type-D *)

exception EvalError

type obj =
  | Str of string
  | Int of int
  | Bool of bool
  | Arr of obj list
  | Tup of obj list
  | Obj of string option * (string * obj) list
[@@deriving show]

type 'a context =
  | Val of 'a
  | Slot of string * 'a context
  | Expr of ('a context -> 'a context)
  | Fn of string list * 'a context
  | Mod of 'a context list
  | Defn of 'a context list
[@@deriving show]

let add_obj = function
  | Val (Int x), Val (Int y) -> Int (x + y)
  | _, _ -> raise EvalError

let subtract_obj = function
  | Val (Int x), Val (Int y) -> Int (x - y)
  | _, _ -> raise EvalError

let multiply_obj = function
  | Val (Int x), Val (Int y) -> Int (x * y)
  | _, _ -> raise EvalError

let rec to_obj_list = function
  | [] -> []
  | Val x :: t -> x :: to_obj_list t
  | _ -> raise EvalError

let eql_obj = function Val x, Val y -> Bool (x = y) | _, _ -> raise EvalError
let id_to_slot id obj = Slot (id, obj)

let rec param_symbols l =
  match l with
  | [] -> []
  | h :: t -> (
      match h with
      | DirectBinding x -> x :: param_symbols t
      | _ -> raise EvalError)

let append m ctx : 'a context =
  match (m, ctx) with
  | Mod l, Mod r -> Mod (r @ l)
  | Mod l, _ -> Mod (ctx :: l)
  | _, Mod r -> Mod (r @ [ m ])
  | _ -> Mod [ ctx; m ]

let rec lookup ctx sym =
  match ctx with
  | Slot (slot, value) when slot = sym -> value
  | Mod (h :: t) -> (
      match h with
      | Slot (slot, value) when slot = sym -> value
      | _ -> lookup (Mod t) sym)
  | _ -> raise EvalError

let rec eval_program_element el ctx =
  match el with
  | Definition defn -> eval_definition defn ctx
  | Statement stmt -> eval_statement stmt ctx

and eval_definition defn ctx =
  match defn with
  | ValueDef (ValueBinding bindings) -> eval_bindings bindings ctx
  | FunDef (FunctionDefinition (id, params, FunExprBody body)) ->
      eval_fundef id params body ctx
  | _ -> ctx

and eval_fundef id params body defctx =
  append defctx
    (id_to_slot id
       (Fn
          ( param_symbols params,
            Expr (fun ctx -> eval_expr body (append defctx ctx)) )))

and eval_bindings bindings ctx =
  match bindings with
  | [] -> ctx
  | h :: t -> eval_bindings t (eval_binding h ctx)

and eval_binding binding ctx =
  match binding with
  | DirectBinding i, expr -> append ctx (id_to_slot i (eval_expr expr ctx))
  | _ -> ctx

and eval_statement stmt ctx =
  match stmt with SimpleStatement expr -> eval_expr expr ctx | _ -> ctx

and eval_expr expr ctx =
  match expr with
  | IntegerValue i -> Val (Int i)
  | BooleanValue b -> Val (Bool b)
  | ArrayLiteral a ->
      Val (Arr (to_obj_list (List.map (fun expr -> eval_expr expr ctx) a)))
  | BinaryOperation (op, exl, exr) ->
      eval_op op (eval_expr exl ctx) (eval_expr exr ctx) ctx
  | IdentityExpr sym -> lookup ctx sym
  | ApplicationExpr (expr, exprs) -> (
      let fn = eval_expr expr ctx in
      match fn with
      | Fn (params, closure) -> (
          match closure with
          (* TODO: parital apply *)
          | Expr fn ->
              let exec_ctx =
                bind_params params
                  (List.map (fun exp -> val_of (eval_expr exp ctx) ctx) exprs)
                  ctx
              in
              fn exec_ctx
          | _ -> raise EvalError)
      | _ -> raise EvalError)
  | IfElseExpr (cond, cons, alter) ->
      if eval_expr cond ctx = Val (Bool true) then eval_expr cons ctx
      else eval_expr alter ctx
  | FunctionLiteral (params, FunExprBody body) ->
      let defctx = ctx in
      Fn
        ( param_symbols params,
          Expr (fun ctx -> eval_expr body (append defctx ctx)) )
  | _ -> ctx

and bind_params params values ctx =
  match (params, values) with
  | [], [] -> ctx
  | [], _ -> raise EvalError
  | _, [] -> raise EvalError
  | h1 :: t1, h2 :: t2 -> bind_params t1 t2 (append ctx (Slot (h1, h2)))

and val_of expr ctx =
  match expr with
  | Val x -> Val x
  | Slot (_, v) -> val_of expr v
  | Expr fn -> val_of expr (fn ctx)
  | Fn (_, _) -> expr
  | _ -> raise EvalError

and eval_op op expl expr ctx =
  match op with
  | "+" -> Val (add_obj (val_of expl ctx, val_of expr ctx))
  | "-" -> Val (subtract_obj (val_of expl ctx, val_of expr ctx))
  | "*" -> Val (multiply_obj (val_of expl ctx, val_of expr ctx))
  | "==" -> Val (eql_obj (val_of expl ctx, val_of expr ctx))
  | _ -> raise EvalError

and eval_program_elements pl ctx =
  match pl with
  | [] -> ctx
  | h :: t -> eval_program_elements t (eval_program_element h ctx)

and eval_program (Program pl) = eval_program_elements pl (Mod [])

and eval text =
  eval_program (Parser.program Lexer.token (Lexing.from_string text))

let%test _ =
  let m =
    eval
      {|
    fun fact x i =
      if x == 0 then i
      else fact (x - 1) (i * x);
    val result = fact 5 1;
    |}
  in
  lookup m "result" = Val (Int 120)

let%test _ =
  let m = eval {|
  val x = 1 + 1;
  |} in
  lookup m "x" = Val (Int 2)

let%test _ =
  let m = eval {|
  val x = 1 == 1;
  |} in
  lookup m "x" = Val (Bool true)

let%test _ =
  let m =
    eval
      {|
    fun plus1 x = x + 1;
    fun times2 x = x * 2;
    fun compose f g = fun x -> f (g x);
    val v = (compose plus1 times2) 3;
    |}
  in
  lookup m "v" = Val (Int 7)
