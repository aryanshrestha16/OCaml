(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with
|(a,b,c) -> (c,b,a)

let is_odd x = 
  if x mod 2 = 0 then false 
  else true

let area x y = match x with
|(a,b) -> (match y with
  |(c,d) -> abs((d - b) * (c - a)))

let volume x y = match x with
|(a,b,c) -> (match y with
  |(d,e,f) -> abs((f - c) * (e - b) * (d - a)) )

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
  if n = 0 then 0 
  else if n <= 2 then 1
  else (fibonacci(n - 2)) + (fibonacci( n - 1)) 

let rec pow x y = 
  if y = 0 then 1 
  else x * pow x (y-1)

let rec log x y = 
  if x > y then 0 
  else 1 + log x (y/x)

let rec gcf x y =
  if y = 0 then x 
  else gcf y (x mod y )
  
let rec is_prime_helper x n =
  if n <=  0  then false
  else if n = 1 then true
  else if x mod n = 0 then false
  else is_prime_helper x (n - 1)

let rec is_prime x = 
  is_prime_helper x (x - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = match lst with
|h::t -> if idx = 0 then h else get (idx - 1) t 
| _ -> failwith "Out of bounds"

let rec size lst1 = match lst1 with
|[] -> 0
|(h::t) -> 1 + size t

let  larger lst1 lst2 = 
if size lst1 = size lst2 then []
else if size lst1 > size lst2 then lst1
else lst2 

let rec reverse lst = match lst with 
|[] -> []
|h::t -> reverse t @ [h]

let rec combine lst1 lst2 = match lst1 with 
|[] -> lst2
|h::t -> h ::  combine t lst2

let rec merge lst1 lst2 = match lst1 with 
|[] -> lst2
|h::t -> ( match lst2 with
  |[] -> lst1
  |h1::t2 -> if h < h1 then [h] @ merge t lst2 
  else [h1] @ merge lst1 t2)

let rec rotate shift lst = match lst with
|[] -> []
|h::t -> if shift = 0 then lst else rotate (shift - 1) (t @ [h])

let rec is_palindrome lst = match lst with
|[] -> true
|_ -> if lst = reverse lst then true else false