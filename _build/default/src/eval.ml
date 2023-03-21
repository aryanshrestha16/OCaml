open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
  | Value (Int x) -> Int x
  | Value (Bool x) -> Bool x
  | Value (String x) -> String x
  | Value (Closure (a,x, v)) -> Closure (a,x, v) 
  | ID x -> lookup env x
  | Not expr-> (
                match eval_expr env expr with
              | Bool x  -> Bool (not x)
              | _ -> raise (TypeError("Exptected type bool"))
            )
            
  | Binop(Add,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> Int (x + y)
                                      |_ -> raise (TypeError("Expected type int"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type int")))
  | Binop(Sub,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> Int (x - y)
                                      |_ -> raise (TypeError("Expected type int"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type int")))
  | Binop(Mult,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> Int (x * y)
                                      |_ -> raise (TypeError("Expected type int"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type int")))
  | Binop(Div,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                   ( match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> if y <> 0 then Int (x/y) else raise(DivByZeroError)
                                      |_ -> raise (TypeError("Expected type int"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type int")))
  | Binop(Greater,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> Bool (x > y)
                                      |_ -> raise (TypeError("Expected type int"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type int")))
  | Binop(Less,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> Bool (x < y)
                                      |_ -> raise (TypeError("Expected type int"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type int")))
  | Binop(GreaterEqual,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> Bool (x >= y)
                                      |_ -> raise (TypeError("Expected type int"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type int")))
  | Binop(LessEqual,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> Bool (x <= y)
                                      |_ -> raise (TypeError("Expected type int"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type int")))
  | Binop(Concat,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |String x -> (
                                      match n2 with
                                      |String y -> String (x ^ y)
                                      |_ -> raise (TypeError("Expected type String"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type int")))
  | Binop(Equal,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> Bool (x = y)
                                      |_ -> raise (TypeError("Cannot compare types"))
                                    ) 
                                    |String x -> (
                                      match n2 with
                                      |String y -> Bool (x = y)
                                      |_ -> raise (TypeError("Cannot compare types"))
                                    )
                                    |Bool x -> (
                                      match n2 with
                                      |Bool y -> Bool (x = y)
                                      |_ -> raise (TypeError("Cannot compare types"))
                                    )
                                    |_ -> raise (TypeError("Cannot comapre Closure"))
                                    )
  | Binop(NotEqual,expr1,expr2 ) -> let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Int x -> (
                                      match n2 with
                                      |Int y -> Bool (x <> y)
                                      |_ -> raise (TypeError("Cannot compare types"))
                                    ) 
                                    |String x -> (
                                      match n2 with
                                      |String y -> Bool (x <> y)
                                      |_ -> raise (TypeError("Cannot compare types"))
                                    )
                                    |Bool x -> (
                                      match n2 with
                                      |Bool y -> Bool (x <> y)
                                      |_ -> raise (TypeError("Cannot compare types"))
                                    )
                                    |_ -> raise (TypeError("Cannot comapre Closure"))  
                                    )
  |Binop(Or,expr1,expr2) ->  let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Bool x -> (
                                      match n2 with
                                      |Bool y -> Bool(x || y)
                                      |_ -> raise (TypeError("Expected type bool"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type bool")))                                                                                                
  |Binop(And,expr1,expr2) ->  let n1 = (eval_expr env expr1) in
                                  let n2 = (eval_expr env expr2) in 
                                    (match n1 with
                                    |Bool x -> (
                                      match n2 with
                                      |Bool y -> Bool(x && y)
                                      |_ -> raise (TypeError("Expected type bool"))
                                    ) 
                                    |_ -> raise (TypeError("Expected type bool")))
  |If(expr1,expr2,expr3) -> let n1 = (eval_expr env expr1) in 
                                    (match n1 with
                                    |Bool true -> eval_expr env expr2
                                    |Bool false -> eval_expr env expr3
                                    |_ -> raise(TypeError("Expected type bool")) )
  |Let(x,false,expr1,expr2) -> let v = eval_expr env expr1 in
                                      let env1 = extend env x v in 
                                      eval_expr env1 expr2
  |Let(x,true,expr1,expr2) -> let tempenv = extend_tmp env x in
                              let v = eval_expr tempenv expr1 in 
                              let _ = update tempenv  x v in 
                              eval_expr tempenv expr2

  |Fun(x,expr1) -> Closure(env,x,expr1)
  |FunctionCall(expr1,expr2) -> let fun1 = eval_expr env expr1 in
                                (
                                  match fun1 with 
                                  |Closure(a,b,c) -> let n1 = eval_expr env expr2 in 
                                                      eval_expr (extend a b n1) c
                                  |_ -> raise(TypeError("Not a function"))
                                )
                            
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
|Def (x,expr) -> let n1 = eval_expr env expr in 
                  (extend env x n1 , Some n1)
|Expr expr -> (env,Some (eval_expr env expr))
|NoOp -> (env,None)