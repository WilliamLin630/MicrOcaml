open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let rec tokenize input =
  let input_len = String.length input in
  let ws_re = Re.compile (Re.Perl.re "^(\\s+)") in
  let pos_int_re = Re.compile (Re.Perl.re "^([\\d]+)") in
  let neg_int_re = Re.compile (Re.Perl.re "^\\(-([\\d]+)\\)") in
  let rparen_re = Re.compile (Re.Perl.re "^\\)") in
  let lparen_re = Re.compile (Re.Perl.re "^\\(") in
  let rcurly_re = Re.compile (Re.Perl.re "^\\}") in
  let lcurly_re = Re.compile (Re.Perl.re "^\\{") in
  let dot_re = Re.compile (Re.Perl.re "^\\.") in
  let equal_re = Re.compile (Re.Perl.re "^=") in
  let notequal_re = Re.compile (Re.Perl.re "^<>") in
  let greater_re = Re.compile (Re.Perl.re "^>") in
  let less_re = Re.compile (Re.Perl.re "^<") in
  let greaterequal_re = Re.compile (Re.Perl.re "^>=") in
  let lessequal_re = Re.compile (Re.Perl.re "^<=") in
  let or_re = Re.compile (Re.Perl.re "^\\|\\|") in
  let and_re = Re.compile (Re.Perl.re "^&&") in
  let not_re = Re.compile (Re.Perl.re "^not ") in
  let if_re = Re.compile (Re.Perl.re "^if ") in
  let then_re = Re.compile (Re.Perl.re "^then ") in
  let else_re = Re.compile (Re.Perl.re "^else ") in
  let add_re = Re.compile (Re.Perl.re "^\\+") in
  let arrow_re = Re.compile (Re.Perl.re "^->") in
  let sub_re = Re.compile (Re.Perl.re "^-") in
  let mult_re = Re.compile (Re.Perl.re "^\\*") in
  let div_re = Re.compile (Re.Perl.re "^/") in
  let concat_re = Re.compile (Re.Perl.re "^\\^") in
  let let_re = Re.compile (Re.Perl.re "^let ") in
  let rec_re = Re.compile (Re.Perl.re "^rec ") in
  let in_re = Re.compile (Re.Perl.re "^in ") in
  let def_re = Re.compile (Re.Perl.re "^def ") in
  let fun_re = Re.compile (Re.Perl.re "^fun ") in
  let doublesemi_re = Re.compile (Re.Perl.re "^;;") in
  let semi_re = Re.compile (Re.Perl.re "^;") in
  let bool_re = Re.compile (Re.Perl.re "^(true|false)") in
  let string_re = Re.compile (Re.Perl.re "^\"([^\"]*)\"") in
  let id_re = Re.compile (Re.Perl.re "^([a-zA-Z][a-zA-Z\\d]*)") in

  if input = "" then []
  else if Re.execp ws_re input then 
    let group = Re.exec ws_re input in
    let ws = Re.Group.get group 1 in
    let len = String.length ws in
    tokenize (String.sub input len (input_len - len))
  else if Re.execp pos_int_re input then 
    let group = Re.exec pos_int_re input in
    let str_int = Re.Group.get group 1 in
    let len = String.length str_int in
    let int = int_of_string str_int in
    Tok_Int (int) :: (tokenize (String.sub input len (input_len - len)))
  else if Re.execp neg_int_re input then 
    let group = Re.exec neg_int_re input in
    let str_int = Re.Group.get group 1 in
    let int = int_of_string str_int in
    let len = String.length str_int + 3 in
    Tok_Int (-1 * int) :: (tokenize (String.sub input len (input_len - len)))
  else if Re.execp rparen_re input then Tok_RParen :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp lparen_re input then Tok_LParen :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp rcurly_re input then Tok_RCurly :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp lcurly_re input then Tok_LCurly :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp dot_re input then Tok_Dot :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp equal_re input then Tok_Equal :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp notequal_re input then Tok_NotEqual :: tokenize (String.sub input 2 (input_len - 2))
  else if Re.execp greaterequal_re input then Tok_GreaterEqual :: tokenize (String.sub input 2 (input_len - 2))
  else if Re.execp lessequal_re input then Tok_LessEqual :: tokenize (String.sub input 2 (input_len - 2))
  else if Re.execp greater_re input then Tok_Greater :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp less_re input then Tok_Less :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp or_re input then Tok_Or :: tokenize (String.sub input 2 (input_len - 2))
  else if Re.execp and_re input then Tok_And :: tokenize (String.sub input 2 (input_len - 2))
  else if Re.execp not_re input then Tok_Not :: tokenize (String.sub input 3 (input_len - 3))
  else if Re.execp if_re input then Tok_If :: tokenize (String.sub input 2 (input_len - 2))
  else if Re.execp then_re input then Tok_Then :: tokenize (String.sub input 4 (input_len - 4))
  else if Re.execp else_re input then Tok_Else :: tokenize (String.sub input 4 (input_len - 4))
  else if Re.execp add_re input then Tok_Add :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp arrow_re input then Tok_Arrow :: tokenize (String.sub input 2 (input_len - 2))
  else if Re.execp sub_re input then Tok_Sub :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp mult_re input then Tok_Mult :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp div_re input then Tok_Div :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp concat_re input then Tok_Concat :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp let_re input then Tok_Let :: tokenize (String.sub input 3 (input_len - 3))
  else if Re.execp rec_re input then Tok_Rec :: tokenize (String.sub input 3 (input_len - 3))
  else if Re.execp in_re input then Tok_In :: tokenize (String.sub input 2 (input_len - 2))
  else if Re.execp def_re input then Tok_Def :: tokenize (String.sub input 3 (input_len - 3))
  else if Re.execp fun_re input then Tok_Fun :: tokenize (String.sub input 3 (input_len - 3))
  else if Re.execp doublesemi_re input then Tok_DoubleSemi :: tokenize (String.sub input 2 (input_len - 2))
  else if Re.execp semi_re input then Tok_Semi :: tokenize (String.sub input 1 (input_len - 1))
  else if Re.execp bool_re input then
    let group = Re.exec bool_re input in
    let bool_str = Re.Group.get group 1 in
    let bool = bool_str = "true" in
    let len = String.length bool_str in
    Tok_Bool (bool) :: (tokenize (String.sub input len (input_len - len)))
  else if Re.execp string_re input then 
    let group = Re.exec string_re input in
    let str = Re.Group.get group 1 in
    let len = String.length str + 2 in
    Tok_String (str) :: tokenize (String.sub input len (input_len - len))
  else if Re.execp id_re input then 
    let group = Re.exec id_re input in
    let id = Re.Group.get group 1 in
    let len = String.length id in
    Tok_ID (id) :: tokenize (String.sub input len (input_len - len))
  else failwith "InvalidInputException"