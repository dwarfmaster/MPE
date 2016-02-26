(* vim:set foldmethod=marker: *)

type arbgen = R of int*arbgen list;;
type arbbin = Nil | Noeud of int*arbbin*arbbin;;

let arbre1 = R (3, [R (1, []); R (4, [R (2, [])]); R (6, [])]);;
let arbre2 = R (5, [R (3, [R (1, []); R (4, [])]); R (2, [])]);;

(* {{{ Exercice 1 *)
let rec preordre_acc (R (v,l)) acc = v :: preordre_foret l acc
and preordre_foret foret acc = match foret with
| []     -> acc
| a :: t -> preordre_acc a (preordre_foret t acc);;

let rec postordre_acc (R (v,l)) acc = v :: postordre_foret l acc
and postordre_foret foret acc = match foret with
| []     -> acc
| a :: t -> postordre_foret t (postordre_acc a acc);;

let preordre arbre = preordre_acc arbre [];;
let postordre arbre = postordre_acc arbre [];;

preordre arbre1;;
preordre arbre2;;
postordre arbre1;;
postordre arbre2;;

(* }}} *)

(* {{{ Exercice 2 *)
(* Hypothèse : parcours en largeur *)
let rec f foret = match foret with
| []           -> []
| R (v,l) :: t -> v :: f (t @ l);;

f [arbre1];;
f [arbre2];;

(* }}} *)

(* {{{ Exercice 3 *)
let rec has_chemin_foret foret s = match foret with
| []           -> (false, [])
| R (v,l) :: t -> if v == s then (true, [0]) else begin
    let (b,c) = has_chemin_foret l s in
    if b then (true, 0 :: c) else begin
        let (b1, c1) = has_chemin_foret t s in
        if b1 then let h1::t1 = c1 in (true, h1 + 1 :: t1)
        else (false, [])
    end
end;;

let rec has_chemin arbre s = let (b,t) = has_chemin_foret [arbre] s in
    if b then (true, tl t)
    else (false, []);;

let rec go_to_foret foret c = match c with
| [] -> let (R (v,l) :: n) = foret in v
| h::t -> let (R (v,l) :: n) = foret in
    if h = 0 then go_to_foret l t
    else go_to_foret n ((h-1) :: t);;

has_chemin arbre1 6;;
has_chemin arbre2 6;;
has_chemin arbre1 2;;
has_chemin arbre2 2;;

(* }}} *)

(* {{{ Exercice 4 *)
let rec foret_to_bin foret = match foret with
| []           -> Nil
| R (v,l) :: t -> Noeud (v, foret_to_bin l, foret_to_bin t);;

let rec bin_to_foret bin = match bin with
| Nil -> []
| Noeud (v,l,t) -> R (v, bin_to_foret l) :: bin_to_foret t;;

(bin_to_foret (foret_to_bin [arbre1])) = [arbre1];;
(bin_to_foret (foret_to_bin [arbre2])) = [arbre2];;
foret_to_bin [arbre1];;

(* }}} *)

(* {{{ Exercice 5 *)
let rec printn str n = match n with
| 0 -> ()
| _ -> print_string str;
       printn str (n-1);;

let rec print_bin_offset bin off = match bin with
| Nil               -> printn "  " off;
                       print_string "V\n"
| Noeud (v,Nil,Nil) -> printn "  " off;
                       print_string "F "; print_int v; print_string "\n"
| Noeud (v,b1,b2)   -> printn "  " off;
                       print_string "N "; print_int v; print_string "\n";
                       print_bin_offset b1 (off + 1);
                       print_bin_offset b2 (off + 1);;

print_bin_offset (foret_to_bin [arbre1]) 0;;

(* }}} *)

(* {{{ Exercice 6 *)
type arbpur = V | N of arbpur*arbpur;;
let arbre_pur = N (N (V,N (N (V,V), N (V,V))), V);;

let rec code_brutal pur = match pur with
| V         -> [0]
| N (a1,a2) -> 1 :: code_brutal a1 @ code_brutal a2;;

let rec code_acc pur acc = match pur with
| V         -> 0 :: acc
| N (a1,a2) -> 1 :: code_acc a1 (code_acc a2 acc);;
let code pur = code_acc pur [];;

code_acc arbre_pur [];;
code_brutal arbre_pur;;

let rec decode_rec cd = match cd with
| []                -> failwith "Invalid code"
| h :: t when h = 0 -> (V,t)
| h :: t when h = 1 -> let (a1,t1) = decode_rec t in
                       let (a2,t2) = decode_rec t1 in
                       (N (a1,a2), t2);;
let decode cd = let (a,_) = decode_rec cd in a;;

decode (code arbre_pur);;

(* TODO decode et fin *)

(* }}} *)

(* {{{ Exercice 7 *)
let rec evalflot_rec pur flot = match (pur,flot) with
| (V,[])         -> failwith "Bad flow"
| (V,h::t)       -> (Noeud (h,Nil,Nil), t, h)
| (N (a1,a2), t) -> let (n1, t1, s1) = evalflot_rec a1 t  in
                    let (n2, t2, s2) = evalflot_rec a2 t1 in
                    let s = (s1 + s2) mod 4 in
                    (Noeud (s, n1, n2), t2, s);;

evalflot_rec arbre_pur [1; 2; 3; 4; 5; 6];;

let rec estcompatible arbre = match arbre with
| Nil             -> true
| Noeud (v,a1,a2) -> v != 0 && estcompatible a1 && estcompatible a2;;

(* }}} *)

(* {{{ Exercice 8 *)
let rec cross_join_r e1 e2 ac =
    let rec rjoin a e acc = match e with
    | []   -> acc
    | h::t -> rjoin a t (Noeud (0,a,h) :: acc)
    in match e1 with
    | []   -> ac
    | h::t -> cross_join_r t e2 (rjoin h e2 ac);;
let cross_join e1 e2 = cross_join_r e1 e2 [];;

let enumerate n =
    let mem = make_vect (n+1) [] in
    mem.(0) <- [Nil];
    for i = 1 to n do
        for j = 0 to i - 1 do
            mem.(i) <- cross_join_r mem.(j) mem.(i-j-1) mem.(i);
        done;
    done;
    mem.(n);;

enumerate 3;;

let rec map f a = match a with
| []   -> []
| h::t -> f h :: map f t;;

let enumerate_gen n =
    let toar l = R (0,l)
    in map toar (map bin_to_foret (enumerate (n-1)));;

enumerate_gen 5;;

(* }}} *)

