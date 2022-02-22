(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
    match tup with
    (x, y, z) -> (z, y, x)
;;

let is_odd x =
    if (x mod 2) = 0 then
        false
    else
        true
;;

let area x y = 
    let (a, b) = x in
    let (c, d) = y in
    abs(a - c) * abs(b - d)
;;

let volume x y = 
    let (a, b, c) = x in
    let (d, e, f) = y in
    abs(a - d) * abs(b - e) * abs(c - f)
;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
    match n with
    | (0 | 1) -> n
    | n -> fibonacci (n-1) + fibonacci(n-2)
;;

let rec pow x y = 
    match y with
    | 0 -> 1
    | 1 -> x
    | _ -> x * pow x (y-1)
;;

let rec log x y = 
    if y < x then
        0
    else
        (log x (y/x)) + 1
;;

let rec gcf x y = 
    if x = 0 then y
    else if y = 0 then x

    else if x = y then
        x
    else if x > y then
        gcf (x-y) y
    else
        gcf x (y-x)
;;

let rec is_prime_helper x y = 
    if ((y*y) > x) then
        true
    else if ((x mod y) == 0) then
        false
    else
        is_prime_helper x (y+2)
;;

let rec is_prime x = 
    if x = 2 then
        true
    else if ((x mod 2) = 0) then
        false
    else if x < 2 then
        false
    else
        is_prime_helper x 3
;;

(*****************)
(* Part 3: Lists *)
(*failwith "Out of bounds"*)
(*****************)

let rec get idx lst = 
    match lst with
    | [] -> raise (failwith "Out of bounds")
    | h::t -> if idx = 0 then h 
                else get (idx-1) t
;;

let rec length list = 
    match list with
    | [] -> 0
    | (h::t) -> 1 + length t
;;

let larger lst1 lst2 = 
    if length lst1 = length lst2 then []
    else if length lst1 > length lst2 then lst1
    else lst2
;;

let reverse lst = 
    let rec aux_reverse ax = function
        | [] -> ax
        | (h::t) -> aux_reverse (h::ax) t in 
        aux_reverse [] lst
;;

let rec combine lst1 lst2 = 
    match (lst1,lst2) with
    | [],x ->x
    | x, [] -> x 
    | (h1::t1),(h2::t2) -> h1 :: (combine t1 lst2)
;;

let rec insert x l=
    match l with 
        |[]->[x]
        |h::t-> if x < h then x::h::t 
                    else h::insert x t
;;
	
let rec sort l = 
    match l with 
    []->[]
    |[x]->[x]
    |h::t->insert h (sort t)
;;

let rec merge lst1 lst2 = 
    sort (combine lst1 lst2)
;;

let rec rotate shift lst = 
    match lst with 
    [] -> []
    | (h::[]) -> [h]
    | (h::t) -> if shift = 0 then lst 
                else rotate (shift-1) (t@[h])
 ;;

let rec is_palindrome lst = 
    reverse lst = lst
;;