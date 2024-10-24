open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
  | ID (id) -> lookup env id
  | Not (e1) ->
    (match eval_expr env e1 with
    | Bool (b) -> Bool (not b)
    | _ -> raise (TypeError "Expected type bool"))
  | Binop (Add, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (int2) -> Int (int1 + int2)
    | _ -> raise (TypeError "Expected type int"))
  | Binop (Sub, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (int2) -> Int (int1 - int2)
    | _ -> raise (TypeError "Expected type int"))
  | Binop (Mult, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (int2) -> Int (int1 * int2)
    | _ -> raise (TypeError "Expected type int"))
  | Binop (Div, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (0) -> raise DivByZeroError
    | Int (int1), Int (int2) -> Int (int1 / int2)
    | _ -> raise (TypeError "Expected type int"))
  | Binop (Greater, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (int2) -> Bool (int1 > int2)
    | _ -> raise (TypeError "Expected type int"))
  | Binop (Less, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (int2) -> Bool (int1 < int2)
    | _ -> raise (TypeError "Expected type int"))
  | Binop (GreaterEqual, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (int2) -> Bool (int1 >= int2)
    | _ -> raise (TypeError "Expected type int"))
  | Binop (LessEqual, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (int2) -> Bool (int1 <= int2)
    | _ -> raise (TypeError "Expected type int"))
  | Binop (Concat, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | String (str1), String (str2) -> String (str1 ^ str2)
    | _ -> raise (TypeError "Expected type string"))
  | Binop (Equal, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (int2) -> Bool (int1 = int2)
    | Bool (b1), Bool (b2) -> Bool (b1 = b2)
    | String (str1), String (str2) -> Bool (str1 = str2)
    | _ -> raise (TypeError "Cannot compare types"))
  | Binop (NotEqual, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Int (int1), Int (int2) -> Bool (int1 <> int2)
    | Bool (b1), Bool (b2) -> Bool (b1 <> b2)
    | String (str1), String (str2) -> Bool (str1 <> str2)
    | _ -> raise (TypeError "Cannot compare types"))
  | Binop (Or, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Bool (b1), Bool (b2) -> Bool (b1 || b2)
    | _ -> raise (TypeError "Expected type bool"))
  | Binop (And, e1, e2) -> 
    (match eval_expr env e1, eval_expr env e2 with
    | Bool (b1), Bool (b2) -> Bool (b1 && b2)
    | _ -> raise (TypeError "Expected type bool"))
  | If (guard, e1, e2) -> 
    (match eval_expr env guard with
    | Bool (guard_b) -> if guard_b then eval_expr env e1 else eval_expr env e2
    | _ -> raise (TypeError "Expected type bool"))
  | Let (id, false, e1, e2) -> 
    let init = eval_expr env e1 in
    let env' = extend env id init in
    eval_expr env' e2
  | Let (id, true, e1, e2) -> 
    let env' = extend_tmp env id in
    let init = eval_expr env' e1 in
    let () = update env' id init in
    eval_expr env' e2
  | Fun (id, e1) -> Closure (env, id, e1)
  | App (f, e2) -> 
    (match eval_expr env f, eval_expr env e2 with
    | Closure (env', id, e1), v -> 
      let env'' = extend env' id v in
      eval_expr env'' e1
    | _ -> raise (TypeError "Not a function")
    )
  | Select (Lab (id), record_e) ->
    let f target_id (Lab x_id, _) = (target_id = x_id) in
    (match eval_expr env record_e with
    | Record (lst) -> 
      (match List.find_opt (f id) lst with
      | Some (Lab (x_id), e1) -> e1
      | _ -> raise (SelectError "Label not found")
      )
    | _ -> raise (TypeError "Not a record")
    )
  | Int (_) | Bool (_) | String (_) | Closure (_, _, _) | Record (_) ->  e

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def(id, e) -> 
    let env' = extend_tmp env id in
    let ex = eval_expr env' e in
    let () = update env' id ex in
    (env', Some ex)
  | Expr (e) -> (env, Some (eval_expr env e))
  | NoOp -> ([], None)