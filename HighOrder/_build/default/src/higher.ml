open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = match lst with
|[] -> false
|h::t ->  if (fold (fun a x -> if x = e then a + 1 else a) 0 lst) > 0 then true else false

let is_present lst x = match lst with
|[] -> []
|h::t -> map (fun a -> if a = x then 1 else 0) lst

let count_occ lst target = match lst with
|[] -> 0
|h::t -> fold (fun a x -> if x = target then a + 1 else a) 0 lst 

let uniq lst = match lst with
|[] -> []
|h::t -> fold(fun a x -> if (contains_elem a x) then a else x :: a) [] lst 

let assoc_list lst = match lst with
|[] -> []
|h::t -> map(fun x -> (x, count_occ lst x)) (uniq lst)

let ap fns args = match (fns,args) with
|([],[]) -> []
|(h::t,h1::t1) -> fold (@) [] (map(fun x -> map x args) fns)
|_ -> []
