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
| N (a1,a2) -> 1 :: code_brutal a1 @ code a2;;

let rec code_acc pur acc = match pur with
| V         -> 0 :: acc
| N (a1,a2) -> 1 :: code_acc a1 (code_acc a2 acc);;

code_acc arbre_pur [];;
code_brutal arbre_pur;;

(* TODO decode et fin *)

(* }}} *)

(* TODO question 7 et 8 *)

