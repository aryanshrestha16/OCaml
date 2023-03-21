open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

let tokenize input = 
  let re_int = Str.regexp "[0-9]+" in 
  let re_negint = Str.regexp "(-[0-9]+)" in
  let re_bool = Str.regexp "true\\|false" in 
  let re_string = Str.regexp "\"[^\"]*\"" in 
  let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in 
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in 
  let re_equal = Str.regexp "=" in 
  let re_notequal = Str.regexp "<>" in 
  let re_greater = Str.regexp ">" in 
  let re_less = Str.regexp "<" in 
  let re_greaterequal = Str.regexp ">=" in 
  let re_lessequal = Str.regexp "<=" in 
  let re_or = Str.regexp "||" in 
  let re_and = Str.regexp "&&" in 
  let re_not = Str.regexp "not" in
  let re_if = Str.regexp "if" in 
  let re_then = Str.regexp "then" in 
  let re_else = Str.regexp "else" in 
  let re_add = Str.regexp "+" in 
  let re_sub = Str.regexp "-" in 
  let re_mult = Str.regexp "*" in 
  let re_div = Str.regexp "/" in 
  let re_concat = Str.regexp "\\^" in 
  let re_let = Str.regexp "let" in 
  let re_def = Str.regexp "def" in 
  let re_in = Str.regexp "in" in 
  let re_rec = Str.regexp "rec" in 
  let re_fun = Str.regexp "fun" in 
  let re_arrow = Str.regexp "->" in 
  let re_doublesemi = Str.regexp ";;" in 
  let rec mklst text =
    if text = "" then [] else 
    if (Str.string_match re_int text 0) then 
    let matched = Str.matched_string text  in 
    Tok_Int (int_of_string matched)::(mklst (String.sub text (String.length matched) ((String.length text) - (String.length matched))))
    else if (Str.string_match re_negint text 0) then 
      let matched = Str.matched_string text in 
      Tok_Int(int_of_string (String.sub matched 1 (String.length matched - 2) ))::(mklst (String.sub text (String.length matched) ((String.length text) - (String.length matched))))
    else if (Str.string_match re_bool text 0) then 
    let matched = Str.matched_string text  in 
    Tok_Bool (bool_of_string matched) ::(mklst (String.sub text (String.length matched) ((String.length text) - (String.length matched))))
    else if (Str.string_match re_string text 0) then 
    let matched = Str.matched_string text  in 
    Tok_String (String.sub matched 1 (String.length matched - 2)) ::(mklst (String.sub text (String.length matched) ((String.length text) - (String.length matched))))
    else if (Str.string_match re_id text 0) then 
    let matched = Str.matched_string text  in 
    if matched = "let" then
      Tok_Let::(mklst (String.sub text 3 ((String.length text) - 3)))
    else if matched = "def" then 
      Tok_Def::(mklst (String.sub text 3 ((String.length text) - 3)))
    else if matched = "in" then 
      Tok_In::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if matched = "then" then 
      Tok_Then::(mklst (String.sub text 4 ((String.length text) - 4)))
    else if matched = "else" then 
      Tok_Else::(mklst (String.sub text 4 ((String.length text) - 4)))
    else if matched = "if" then 
      Tok_If::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if matched = "not" then 
      Tok_Not::(mklst (String.sub text 3 ((String.length text) - 3)))
    else if matched = "fun" then 
      Tok_Fun::(mklst (String.sub text 3 ((String.length text) - 3)))
    else if matched = "rec" then 
      Tok_Rec::(mklst (String.sub text 3 ((String.length text) - 3)))
    else
    Tok_ID (matched) ::(mklst (String.sub text (String.length matched) ((String.length text) - (String.length matched))))
    else if (Str.string_match re_lparen text 0 ) then
    Tok_LParen::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_rparen text 0 ) then
    Tok_RParen::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_arrow text 0 ) then
    Tok_Arrow::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if (Str.string_match re_equal text 0 ) then
    Tok_Equal::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_notequal text 0 ) then
    Tok_NotEqual::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if (Str.string_match re_greater text 0 ) then
    Tok_Greater::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_less text 0 ) then
    Tok_Less::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_greaterequal text 0 ) then
    Tok_GreaterEqual::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if (Str.string_match re_lessequal text 0 ) then
    Tok_LessEqual::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if (Str.string_match re_or text 0 ) then
    Tok_Or::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if (Str.string_match re_and text 0 ) then
    Tok_And::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if (Str.string_match re_not text 0 ) then
    Tok_Not::(mklst (String.sub text 3 ((String.length text) - 3)))
    else if (Str.string_match re_if text 0 ) then
    Tok_If::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if (Str.string_match re_then text 0 ) then
    Tok_Then::(mklst (String.sub text 4 ((String.length text) - 4)))
    else if (Str.string_match re_else text 0 ) then
    Tok_Else::(mklst (String.sub text 4 ((String.length text) - 4)))
    else if (Str.string_match re_add text 0 ) then
    Tok_Add::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_sub text 0 ) then
    Tok_Sub::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_mult text 0 ) then
    Tok_Mult::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_div text 0 ) then
    Tok_Div::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_concat text 0 ) then
    Tok_Concat::(mklst (String.sub text 1 ((String.length text) - 1)))
    else if (Str.string_match re_let text 0 ) then
    Tok_Let::(mklst (String.sub text 3 ((String.length text) - 3)))
    else if (Str.string_match re_in text 0 ) then
    Tok_In::(mklst (String.sub text 2 ((String.length text) - 2)))
    else if (Str.string_match re_def text 0 ) then
    Tok_Def::(mklst (String.sub text 3 ((String.length text) - 3)))
    else if (Str.string_match re_fun text 0 ) then
    Tok_Fun::(mklst (String.sub text 3 ((String.length text) - 3)))
    else if (Str.string_match re_rec text 0 ) then
    Tok_Rec::(mklst (String.sub text 3 ((String.length text) - 3)))
    else if (Str.string_match re_doublesemi text 0 ) then
    Tok_DoubleSemi::(mklst (String.sub text 2 ((String.length text) - 2)))
    else (mklst (String.sub text 1 ((String.length text)-1))) in 
  mklst input