
(* {{{ I/ Itération et récursivité *)
(* {{{ 0/ Quelques programmes de base *)
(* {{{ Bits faibles *)
let rec bits_faible i = match i with
| 0 -> []
| _ -> (i mod 2) :: bits_faible (i/2);;
bits_faible 6;;
(* }}} *)

(* {{{ Bits forts *)
let bits_forts_v i =
    let n = int_of_float (log (float_of_int i) /. log 2.) + 1 in
    let v = make_vect n 0 in
    let j = ref i in
    for k = 1 to n do
        v.(n-k) <- (!j mod 2);
        j := !j / 2;
    done;
    v;;
bits_forts_v 6;;

let rec bits_forts i = match i with
| 0 -> []
| _ -> (bits_forts (i/2)) @ [i mod 2];;
bits_forts 6;;

let bits_forts2 i =
    let res = ref [] and p = ref i in
    while !p > 0 do
        res := (!p mod 2) :: !res;
        p := !p / 2;
    done;
    !res;;
bits_forts2 6;;

(* }}} *)

(* {{{ Tri liste *)
let rec insert_lst i l = match l with
| []   -> [i]
| h::t -> if i <= h then i::l else h::insert_lst i t;;
insert 4 [1; 2; 3; 4; 5; 6];;

let tri_ins_lst l =
    let rec tri_ins_lst_rec l1 l2 = match l2 with
    | []   -> l1
    | h::t -> tri_ins_lst_rec (insert_lst h l1) t
    in tri_ins_lst_rec [] l;;
tri_ins_lst [3; 5; 2; 7; 8; 4; 6; 1; 9];;

let rec tri_ins_lst2 l = match l with
| [] -> []
| h::t -> insert h (tri_ins_lst2 t);;
tri_ins_lst2 [3; 5; 2; 7; 8; 4; 6; 1; 9];;

(* }}} *)

(* {{{ Tri vecteur *)
let insert_vect v n =
    let i = ref n in
    while !i > 0 && v.(!i) < v.(!i-1) do
        let t = v.(!i) in
        v.(!i)   <- v.(!i-1);
        v.(!i-1) <- t;
        i := !i - 1;
    done;;
let v = [|1; 4; 8; 17; 7; 8; 5 |];;
insert_vect v 4;;
v;;

let tri_ins_vect v =
    for i = 1 to vect_length v - 1 do
        insert_vect v i;
    done;;
let v = [|3; 5; 2; 7; 8; 4; 6; 1; 9|];;
tri_ins_vect v;;
v;;

(* }}} *)
(* }}} *)

(* {{{ 2/ *)
(* {{{ miroir *)
let miroir u =
    let n = string_length u in
    for i = 0 to n / 2 - 1 do
        let t = u.[i] in
        u.[i]   <- u.[n-1-i];
        u.[n-1-i] <- t;
    done;;
let u = "abcdefg";;
miroir u;;
u;;

let miroir2 u =
    let n = string_length u in
    let s = make_string n `1` in
    for i = 0 to n-1 do
        s.[i] <- u.[n-1-i];
    done;
    s;;
miroir2 "abcdefg";;

(* }}} *)

(* {{{ est_miroir *)
let est_miroir u =
    let n = string_length u in
    let ret = ref true in
    for i = 0 to n / 2 - 1 do
        ret := !ret && (u.[i] == u.[n-1-i]);
    done;
    !ret;;
est_miroir "abcba";;
est_miroir "abcdba";;

(* }}} *)
(* }}} *)

(* }}} *)

(* {{{ III/ Exceptions *)
let rec croissante l = match l with
| []    -> true
| h1::t -> match t with
    | []    -> true
    | h2::_ -> h1 <= h2 && croissante t;;
croissante [1; 2; 4; 5; 7; 8; 9; 14];;
croissante [1; 5; 6; 3; 7; 9];;

exception dec;;
let croissante_ex v =
    try
        for i = 0 to vect_length v - 2 do
            if v.(i) > v.(i+1) then raise dec;
        done; true;
    with dec -> false;;
croissante_ex [|1; 2; 4; 5; 7; 8; 9; 14|];;
croissante_ex [|1; 5; 6; 3; 7; 9|];;

(* }}} *)

(* {{{ IV/ Manipulations de listes *)
(* {{{ 7/ *)
let map_list_imp f l=
    let tail = ref l in
    let ret = ref [] in
    while !tail != [] do
        let h = hd !tail in
        ret := !ret@[f h];
        tail := tl !tail
    done;
    !ret;;
let sq x = x*x;;
map_list_imp sq [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

let map_list f l=
    let rec map_list_rec a = match a with
    | []     -> []
    | hd::tl -> (f hd)::map_list_rec tl
    in map_list_rec l;;
map_list_imp sq [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

(* }}} *)

(* {{{ 8/ *)
let cart_prod1 l1 l2 =
    let rec prd1 a l acc = match a with
    | []   -> acc
    | h::t -> let rec prd2 e l acc = match l with
        | [] -> acc
        | h::t -> prd2 e t ((e,h)::acc)
        in prd1 t l (prd2 h l acc)
    in prd1 l1 l2 [];;
cart_prod1 [1;2;3] ["a"; "b"; "c"];;

let rec it_list f a=function
    | []       -> a
    | x::queue -> it_list f (f a x) queue;;
let add x y = y+x;;
let S = it_list add 0;;
S [1; 2; 3];;

let rec it_list_end f a=function
    | []       -> a
    | x::queue -> f x (it_list_end f a queue);;
let S2 = it_list_end add 0;;
S2 [1; 2; 3];;

let cart_prod l1 l2=
    let prod e a = (e,a) in
    let mp l q e = (map_list (prod e) l)@q in
    it_list (mp l2) [] l1;;
cart_prod [1;2;3] ["a"; "b"; "c"];;

(* }}} *)

(* }}} *)

