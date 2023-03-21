open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t = match t with
|IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
|IntNode(value, None,l,m,r) -> if x < value then IntNode(x,Some value,l,m,r) 
else if x = value then t 
else IntNode(value,Some x,l,m,r)
|IntNode(value, Some value1, l,m,r) -> if x < value then IntNode(value, Some value1, int_insert x l,m,r) 
else if x = value then t 
else if x > value1  then IntNode(value, Some value1,l,m,int_insert x r) 
else IntNode(value,Some value1,l,int_insert x  m,r)

let rec int_mem x t = match t with
|IntLeaf -> false
|IntNode(value, None,l,m,r) -> if x = value then true else false
|IntNode(value, Some value1,l,m,r) -> if x = value then true
else if x = value1 then true   
else if x < value then int_mem x l 
else if x > value1 then int_mem x r 
else int_mem x m 

let rec int_size t = match t with 
|IntLeaf -> 0
|IntNode(value, None,l,m,r) -> 1 + int_size l 
|IntNode(value, Some value1,l,m,r) -> 2 + int_size l + int_size m + int_size r

let rec int_max t = match t with 
|IntLeaf -> raise (Invalid_argument("int max"))
|IntNode(value, None, l, m ,r) ->  if r = IntLeaf then value else int_max r 
|IntNode(value,Some value1,l,m,r) -> if r = IntLeaf then value1 else int_max r 

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = match t with 
|MapLeaf -> MapNode((k,v), None, MapLeaf,MapLeaf,MapLeaf)
|MapNode((key,value), None, l,m,r) -> if k = key then raise (Invalid_argument("map_put"))
else if k > key then MapNode((key,value), Some (k,v),l,m,r)
else MapNode((k,v), Some (key,value),l,m,r)
|MapNode((key,value), Some (key1,value1),l,m,r) -> if k = key then raise (Invalid_argument("map_put"))
else if k = key1 then raise (Invalid_argument("map_put"))
else if k < key then MapNode((key,value), Some (key1,value1), map_put k v l, m ,r)
else if k > key1 then MapNode((key,value), Some (key1,value1), l, m ,map_put k v  r)
else MapNode((key,value), Some (key1,value1), l,map_put k v m, r)

let rec map_contains k t = match t with
|MapLeaf -> false
|MapNode((key,value), None,l,m,r) -> if k = key then true else false
|MapNode((key,value), Some (key1,value1),l,m,r) -> if k = key then true
else if k = key1 then true 
else if k < key then map_contains k l 
else if k > key1 then map_contains k r 
else map_contains k m 

let rec map_get k t = match t with
|MapLeaf -> raise (Invalid_argument("map_get"))
|MapNode((key,value), None,l,m,r) -> if k = key then value else raise (Invalid_argument("map_get"))
|MapNode((key,value), Some (key1,value1),l,m,r) -> if k = key then value 
else if k = key1 then value1
else if k < key then map_get k l
else if k > key1 then map_get k r 
else map_get k m 

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
| TableLeaf
| TableNode of (string * int) list * lookup_table

let empty_table : lookup_table = TableLeaf

let push_scope (table : lookup_table) : lookup_table = 
  TableNode([], table)

let pop_scope (table : lookup_table) : lookup_table = match table with 
|TableLeaf -> failwith "No scopes remain!"
|TableNode(lst, t) -> t 

let add_var name value (table : lookup_table) : lookup_table = match table with 
|TableLeaf -> failwith "There are no scopes to add a variable to!"
|TableNode([],t) -> TableNode([(name,value)],t)
|TableNode(lst, t) -> if (fold (fun a (s,i) -> if s = name then a + 1 else a) 0 lst) > 0 then failwith "Duplicate variable binding in scope!"
else TableNode((name,value)::lst,t)

let rec lookup name (table : lookup_table) = match table with 
|TableLeaf -> failwith "Variable not found!"
|TableNode([],t) -> lookup name t 
|TableNode(lst,t) -> if (fold (fun a (s,i) -> if s = name then a + 1 else a) 0 lst) > 0 then (fold (fun a (s,i) -> if s = name then i else a) 0 lst)
else lookup name t 