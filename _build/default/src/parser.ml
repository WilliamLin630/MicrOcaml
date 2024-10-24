open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)

let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  match lookahead toks with
  | Some Tok_Let -> parse_let_expr toks
  | Some Tok_If -> parse_if_expr toks
  | Some Tok_Fun -> parse_function_expr toks
  | _ -> parse_or_expr toks

and parse_let_expr toks = 
  match lookahead_many toks 1, lookahead_many toks 2 with
  | Some Tok_Rec, Some Tok_ID (id) -> let t = match_many toks [Tok_Let; Tok_Rec; Tok_ID (id); Tok_Equal] in
                let (t', e1) = parse_expr t in
                let t'' = match_token t' Tok_In in
                let (t''', e2) = parse_expr t'' in
                (t''', Let (id, true, e1, e2))
  | Some Tok_ID (id), Some Tok_Equal -> let t = match_many toks [Tok_Let; Tok_ID (id); Tok_Equal] in
                  let (t', e1) = parse_expr t in
                  let t'' = match_token t' Tok_In in
                  let (t''', e2) = parse_expr t'' in
                  (t''', Let (id, false, e1, e2))
  | _ -> raise (InvalidInputException "Failed to parse")
  
and parse_if_expr toks = 
  let t = match_token toks Tok_If in
  let (t', guard) = parse_expr t in
  let t'' = match_token t' Tok_Then in
  let (t''', true_branch) = parse_expr t'' in
  let t'''' = match_token t''' Tok_Else in
  let (t''''', false_branch) = parse_expr t'''' in
  (t''''', If (guard, true_branch, false_branch))

and parse_function_expr toks = 
  match lookahead_many toks 1 with
  | Some Tok_ID (id) -> let t = match_many toks [Tok_Fun; Tok_ID (id); Tok_Arrow] in
                  let (t', e) = parse_expr t in
                  (t', Fun (id, e))
  | _ -> raise (InvalidInputException "Failed to parse")

and parse_or_expr toks = 
  let (t, and_e) = parse_and_expr toks in
  match lookahead t with
  | Some Tok_Or -> let t' = match_token t Tok_Or in
                let (t'', or_e) = parse_or_expr t' in
                (t'', Binop (Or, and_e, or_e))
  | _ -> t, and_e

and parse_and_expr toks = 
  let (t, equality_e) = parse_equality_expr toks in
  match lookahead t with
  | Some Tok_And -> let t' = match_token t Tok_And in
                let (t'', and_e) = parse_and_expr t' in
                (t'', Binop (And, equality_e, and_e))
  | _ -> t, equality_e

and parse_equality_expr toks = 
  let (t, rel_e) = parse_relational_expr toks in
  match lookahead t with
  | Some Tok_Equal -> let t' = match_token t Tok_Equal in
                let (t'', equality_e) = parse_equality_expr t' in
                (t'', Binop (Equal, rel_e, equality_e))
  | Some Tok_NotEqual -> let t' = match_token t Tok_NotEqual in
                let (t'', equality_e) = parse_equality_expr t' in
                (t'', Binop (NotEqual, rel_e, equality_e))
  | _ -> t, rel_e

and parse_relational_expr toks = 
  let (t, additive_e) = parse_additive_expr toks in
  match lookahead t with
  | Some Tok_Less -> let t' = match_token t Tok_Less in
                let (t'', rel_e) = parse_relational_expr t' in
                (t'', Binop (Less, additive_e, rel_e))
  | Some Tok_Greater -> let t' = match_token t Tok_Greater in
                let (t'', rel_e) = parse_relational_expr t' in
                (t'', Binop (Greater, additive_e, rel_e))
  | Some Tok_LessEqual -> let t' = match_token t Tok_LessEqual in
                let (t'', rel_e) = parse_relational_expr t' in
                (t'', Binop (LessEqual, additive_e, rel_e))
  | Some Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in
                let (t'', rel_e) = parse_relational_expr t' in
                (t'', Binop (GreaterEqual, additive_e, rel_e))              
  | _ -> t, additive_e

and parse_additive_expr toks = 
  let (t, multiplicative_e) = parse_multiplicative_expr toks in
  match lookahead t with
  | Some Tok_Add -> let t' = match_token t Tok_Add in
                let (t'', additive_e) = parse_additive_expr t' in
                (t'', Binop (Add, multiplicative_e, additive_e))
  | Some Tok_Sub -> let t' = match_token t Tok_Sub in
                let (t'', additive_e) = parse_additive_expr t' in
                (t'', Binop (Sub, multiplicative_e, additive_e))
  | _ -> 
    t, multiplicative_e

and parse_multiplicative_expr toks = 
  let (t, concat_e) = parse_concat_expr toks in
  match lookahead t with
  | Some Tok_Mult -> let t' = match_token t Tok_Mult in
                let (t'', multiplicative_e) = parse_multiplicative_expr t' in
                (t'', Binop (Mult, concat_e, multiplicative_e))
  | Some Tok_Div -> let t' = match_token t Tok_Div in
                let (t'', multiplicative_e) = parse_multiplicative_expr t' in
                (t'', Binop (Div, concat_e, multiplicative_e))
  | _ -> 
    t, concat_e

and parse_concat_expr toks = 
  let (t, unary_e) = parse_unary_expr toks in
  match lookahead t with
  | Some Tok_Concat -> let t' = match_token t Tok_Concat in
                let (t'', concat_e) = parse_concat_expr t' in
                (t'', Binop (Concat, unary_e, concat_e))
  | _ -> 
    t, unary_e

and parse_unary_expr toks = 
  match lookahead toks with
  | Some Tok_Not -> let t = match_token toks Tok_Not in
                let (t', unary_e) = parse_unary_expr t in
                (t', Not (unary_e))
  | _ -> parse_app_expr toks

and parse_app_expr toks = 
  let (t, select_e) = parse_select_expr toks in
  match lookahead t with
  | Some Tok_Int (_) | Some Tok_Bool (_) | Some Tok_String (_) | Some Tok_ID (_) | Some Tok_LParen -> 
                let (t', primary_e) = parse_primary_expr t in
                (t', App (select_e, primary_e))
  | _ -> t, select_e

and parse_select_expr toks = 
  let (t, primary_e) = parse_primary_expr toks in
  match lookahead t, lookahead_many t 1 with
  | Some Tok_Dot, Some Tok_ID (id) -> let t' = match_many t [Tok_Dot; Tok_ID (id)] in
                (t', Select (Lab (id), primary_e))
  | _ -> t, primary_e

and parse_primary_expr toks = 
  match lookahead toks with
  | Some Tok_Int (integer) -> 
    let t = match_token toks (Tok_Int (integer)) in
                (t, Int (integer))
  | Some Tok_Bool (boolean) -> let t = match_token toks (Tok_Bool (boolean)) in
                (t, Bool (boolean))
  | Some Tok_String (str) -> let t = match_token toks (Tok_String (str)) in
                (t, String (str))
  | Some Tok_ID (id) -> let t = match_token toks (Tok_ID (id)) in
                (t, ID (id))
  | Some Tok_LParen -> let t = match_token toks Tok_LParen in
                let (t', e) = parse_expr t in
                let t'' = match_token t' Tok_RParen in
                (t'', e)
  | _ -> parse_record_expr toks

and parse_record_expr toks = 
  let t = match_token toks Tok_LCurly in
  let (t', record_body_e) = parse_record_body_expr t in
  let t'' = match_token t' Tok_RCurly in
  (t'', Record record_body_e)

and parse_record_body_expr toks = 
  match lookahead toks with
  | Some Tok_RCurly -> (toks, [])
  | Some Tok_ID(id) -> let t = match_many toks [Tok_ID (id); Tok_Equal] in
                let (t', e) = parse_expr t in
                (
                match lookahead t' with 
                | Some Tok_Semi -> let t'' = match_token t' Tok_Semi in
                                let (t''', record_body_e) = parse_record_body_expr t'' in
                                (t''', [(Lab (id), e)] @ record_body_e)
                | _ -> (t', [(Lab (id), e)])
                )
  | _ -> raise (InvalidInputException "Failed to parse")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  | Some Tok_Def -> parse_def_mutop toks
  | Some Tok_DoubleSemi -> ([], NoOp)
  | _ -> parse_expr_mutop toks
  
and parse_def_mutop toks = 
  match lookahead toks, lookahead_many toks 1, lookahead_many toks 2 with
  | Some Tok_Def, Some Tok_ID (id), Some Tok_Equal -> let t' = match_many toks [Tok_Def; Tok_ID (id); Tok_Equal] in
                let (t'', e) = parse_expr t' in
                let t''' = match_token t'' Tok_DoubleSemi in
                (t''', Def (id, e))
  | _ -> raise (InvalidInputException "Failed to parse")

and parse_expr_mutop toks =
  let (t, e) = parse_expr toks in
  let t'' = match_token t Tok_DoubleSemi in
  (t'', Expr (e))