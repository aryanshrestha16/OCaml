open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = match toks with
|Tok_Let::t -> parse_LetExpr(toks)
|Tok_If::t -> parse_IfExpr(toks)
|Tok_Fun::t -> parse_FunctionExpr(toks)
|_ -> parse_OrExpr(toks)
  and parse_LetExpr tokens = 
  let tok = match_token tokens Tok_Let in 
  match tok with 
  |Tok_Rec::t -> let tok1 = match_token tok Tok_Rec in  
    (
      match lookahead tok1 with
      |Some Tok_ID(x) -> let tok2 = match_token tok1 (Tok_ID(x)) in 
        let tok3 = match_token tok2 Tok_Equal in 
        let (tok4,expr) = parse_expr(tok3) in 
        let tok5 = match_token tok4 Tok_In in 
        let (tok6,expr2) = parse_expr(tok5) in 
        (tok6,Let(x,true,expr,expr2))
      |_ -> raise (InvalidInputException("Invalid INput"))
    )

  | _ -> 
    (
      match lookahead tok with
      |Some Tok_ID(x) -> let tok2 = match_token tok (Tok_ID(x)) in 
        let tok3 = match_token tok2 Tok_Equal in 
        let (tok4,expr) = parse_expr(tok3) in 
        let tok5 = match_token tok4 Tok_In in 
        let (tok6,expr2) = parse_expr(tok5) in 
        (tok6,Let(x,false,expr,expr2))
      |_ -> raise (InvalidInputException("Invalid INput"))
    )
  
  and parse_FunctionExpr tokens = match lookahead tokens with
  |Some Tok_Fun -> let token1 = (match_token tokens Tok_Fun) in 
  (
    match lookahead token1 with
    |Some Tok_ID(x) -> let tok2 = match_token token1 (Tok_ID(x)) in 
    let tok3 = match_token tok2 Tok_Arrow in 
    let (tok4,expr) = parse_expr(tok3) in 
    (tok4,Fun(x,expr))
    | _ -> raise (InvalidInputException("Invalid Input"))
  )
  |_ -> raise (InvalidInputException("Invalid Input"))
  
  and parse_IfExpr tokens = match tokens with
  |Tok_If::t -> let token1 = (match_token tokens Tok_If) in 
    let (toks1,expr1) = parse_expr(token1) in 
    let token2 = (match_token toks1 Tok_Then) in 
    let (toks2,expr2) = parse_expr(token2) in 
    let token3  = (match_token toks2 Tok_Else) in
    let (toks3,expr3) = parse_expr(token3) in 
    (toks3,If(expr1,expr2,expr3))
  |_ -> raise (InvalidInputException("Invalid Input"))
  and parse_OrExpr tokens = 
  let (toks,expr) = parse_AndExpr(tokens) in 
  match toks with
  |Tok_Or::t -> let toks1 = (match_token toks Tok_Or) in 
  let (toks2,expr2) = parse_OrExpr(toks1) in (toks2,Binop(Or,expr,expr2))
  |_ -> (toks,expr)
  and parse_AndExpr tokens =
  let (toks,expr) = parse_EqualityExpr(tokens) in 
  match toks with
  |Tok_And::t -> let toks1 = (match_token toks Tok_And) in 
  let (toks2,expr2) = parse_AndExpr(toks1) in (toks2,Binop(And,expr,expr2))
  |_ -> (toks,expr)
  and parse_EqualityExpr tokens =
  let (toks,expr) = parse_RelationalExpr(tokens) in
  match toks with 
  |Tok_Equal::t -> let toks1 = (match_token toks Tok_Equal) in 
  let (toks2,expr2) = parse_EqualityExpr(toks1) in (toks2,Binop(Equal,expr,expr2))
  |Tok_NotEqual::t -> let toks1 = (match_token toks Tok_NotEqual) in 
  let (toks2,expr2) = parse_EqualityExpr(toks1) in (toks2,Binop(NotEqual,expr,expr2))
  |_ -> (toks,expr)
  and parse_RelationalExpr  tokens =
  let (toks,expr) = parse_AdditiveExpr(tokens) in 
  match toks with
  |Tok_Less::t -> let toks1 = (match_token toks Tok_Less) in 
  let (toks2,expr2) = parse_RelationalExpr(toks1) in (toks2,Binop(Less,expr,expr2))
  |Tok_Greater::t -> let toks1 = (match_token toks Tok_Greater) in 
  let (toks2,expr2) = parse_RelationalExpr(toks1) in (toks2,Binop(Greater,expr,expr2))
  |Tok_GreaterEqual::t -> let toks1 = (match_token toks Tok_GreaterEqual) in 
  let (toks2,expr2) = parse_RelationalExpr(toks1) in (toks2,Binop(GreaterEqual,expr,expr2))
  |Tok_LessEqual::t -> let toks1 = (match_token toks Tok_LessEqual) in 
  let (toks2,expr2) = parse_RelationalExpr(toks1) in (toks2,Binop(LessEqual,expr,expr2))
  | _ -> (toks,expr)
  
  and parse_AdditiveExpr tokens = 
  let (toks,expr) = parse_MultiplicativeExpr(tokens) in
  match lookahead toks with
  |Some Tok_Add -> let toks1 = (match_token toks Tok_Add) in 
    let (toks2,expr2) = parse_AdditiveExpr(toks1) in (toks2,Binop(Add,expr,expr2))
  |Some Tok_Sub ->let toks1 = (match_token toks Tok_Sub) in 
    let (toks2,expr2) = parse_AdditiveExpr(toks1) in (toks2,Binop(Sub,expr,expr2))
  |_ -> (toks,expr)
  
     
  and parse_MultiplicativeExpr tokens = 
    let (toks,expr) = parse_ConcatExpr(tokens) in
    match lookahead toks  with 
    |Some Tok_Mult -> let toks1 = (match_token toks Tok_Mult) in 
      let (toks2,expr2) = parse_MultiplicativeExpr(toks1) in (toks2,Binop(Mult,expr,expr2))
    |Some Tok_Div -> let toks1 = (match_token toks Tok_Div) in 
      let (toks2,expr2) = parse_MultiplicativeExpr(toks1) in (toks2,Binop(Div,expr,expr2))
    | _ -> (toks,expr)

  and parse_ConcatExpr tokens = 
    let (toks,expr) = parse_UnaryExpr(tokens) in 
    match toks with 
    |Tok_Concat::t -> let toks1 = (match_token toks Tok_Concat) in 
          let (toks2,expr2) = parse_ConcatExpr(toks1) in (toks2,Binop(Concat,expr,expr2))
    | _ -> (toks,expr)
  and parse_UnaryExpr tokens = match tokens with
  |Tok_Not::t -> let expr = (match_token tokens Tok_Not) in 
  let (toks1,expr1) = parse_UnaryExpr expr in 
      (toks1,Not(expr1))
  | _ -> parse_FunctionCallExpr tokens
  and parse_FunctionCallExpr tokens = 
  let (toks,expr) = parse_PrimaryExpr tokens in 
   match lookahead toks with 
   |Some Tok_Int(x) -> let (toks1,expr1) = parse_PrimaryExpr toks in (toks1,FunctionCall(expr,expr1))
   |Some Tok_Bool(x) -> let (toks1,expr1) = parse_PrimaryExpr toks in (toks1,FunctionCall(expr,expr1))
   |Some Tok_String(x) -> let (toks1,expr1) = parse_PrimaryExpr toks in (toks1,FunctionCall(expr,expr1))
   |Some Tok_ID (x)->let (toks1,expr1) = parse_PrimaryExpr toks in (toks1,FunctionCall(expr,expr1))
   |_ -> (toks,expr)
  and parse_PrimaryExpr tokens = match lookahead tokens with
  |Some Tok_Int(x) -> let toks = match_token tokens (Tok_Int(x)) in (toks, Value(Int (x)))
  |Some Tok_Bool(x) -> let toks = match_token tokens (Tok_Bool(x)) in (toks, Value(Bool(x)))
  |Some Tok_String(x)-> let toks = match_token tokens (Tok_String(x)) in (toks, Value(String(x)))
  |Some Tok_ID(x) -> let toks = match_token tokens (Tok_ID(x)) in (toks, ID(x))
  |Some Tok_LParen -> let expr = (match_token tokens Tok_LParen) in 
      let (toks1, expr1) = parse_expr (expr) in let toks2 = (match_token toks1 Tok_RParen) in (toks2 , expr1)
  | _ -> raise (InvalidInputException("Invalid Input"))

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = match  toks with 
|Tok_Def::t -> parse_def(toks)
|[Tok_DoubleSemi] -> let token = match_token toks Tok_DoubleSemi in (token,NoOp)
|_ -> parse_exprmutop(toks)
and parse_def toks = match lookahead toks with
|Some Tok_Def -> let toks1 = match_token toks Tok_Def in 
    (
      match lookahead toks1 with
      |Some Tok_ID(x) -> let toks2 = match_token toks1 (Tok_ID(x)) in 
      let toks3 = match_token toks2 Tok_Equal in 
      let (toks4,expr) = parse_expr toks3 in 
      let toks5 = match_token toks4 Tok_DoubleSemi in 
      (toks5,Def(x,expr))
      |_ -> raise (InvalidInputException("Invalid Input")) 
    )
|_ -> raise (InvalidInputException("Invalid Input")) 


and parse_exprmutop toks = 
let (toks1,expr) = parse_expr(toks) in 
match lookahead toks1 with
|Some Tok_DoubleSemi-> let toks2 = match_token toks1 Tok_DoubleSemi in 
    (toks2,Expr(expr))
|_ -> raise (InvalidInputException("Invalid Input")) 