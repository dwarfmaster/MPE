(* vim:set foldmethod=marker: *)
type arb = V | N of int*arb*arb;;

let arbtest = N (0, N (1, N (3, V, N (5,V,V)), N (6, N (7,V,V), V)),
                    N (8, V, N (9, V, V)));;

(* {{{ I - Algorithmes généraux *)
(* {{{ Question 1 *)
let rec numerote_rec a n = match a with
| V           -> V
| N (_,a1,a2) -> N (n, numerote_rec a1 (2*n), numerote_rec a2 (2*n+1));;
let numerote a = numerote_rec a 1;;
numerote arbtest;;

let less_than n q = q <= n;;
let more_than n q = q >= n;;

let rec est_abr_r1 a t = match a with
| V           -> true
| N (n,a1,a2) ->    est_abr_r1 a1 (fun x -> t x && x <= n)
                 && est_abr_r1 a2 (fun x -> t x && x >= n)
                 && t n;;
let est_abr1 a = est_abr_r a (fun x -> true);;
est_abr1 (numerote arbtest);;

let rec est_abr_r a = match a with
| V                                 -> (true, 0, 0)
| N (n,a1,a2) when a1 = V && a2 = V -> (true, n, n)
| N (n,a1,a2) when a1 <> V          -> let (b, mn, mx) = est_abr_r a1 in
                                       (b && n >= mx, mn, n)
| N (n,a1,a2) when a2 <> V          -> let (b, mn, mx) = est_abr_r a2 in
                                       (b && n <= mn, n, mx)
| N (n,a1,a2)                       -> let (b1,mn1,mx1) = est_abr_r a1 in
                                       let (b2,mn2,mx2) = est_abr_r a2 in
                                       (b1 && b2 && n >= mx1 && n <= mn2,
                                        mn1, mx2);;
let est_abr a = let (b,_,_) = est_abr_r a in b;;

type maybe = Just of int | Nothing;;
let rec est_abr_r2 a c = match a with
| V -> (true,c)
| N (n,a1,a2) -> let (b1,c1) = est_abr_r2 a1 c in
                 match c1 with
                 | Nothing -> if not b1 then (false,Nothing)
                                        else est_abr_r2 a2 (Just n)
                 | Just cv -> if not b1 && n < cv
                                        then (false,Nothing)
                                        else est_abr_r2 a2 (Just n);;
let est_abr2 a = let (b,_) = est_abr_r2 a Nothing in b;;

let rec est_tas a = match a with
| V           -> true
| N (_,V,V)   -> true
| N (n,a1,V)  -> let (N (n1,_,_)) = a1 in n >= n1 && est_tas a1
| N (n,V,a2)  -> let (N (n2,_,_)) = a2 in n >= n2 && est_tas a2
| N (n,a1,a2) -> let (N (n1,_,_)) = a1 and (N (n2,_,_)) = a2 in
                 n >= n1 && n >= n2 && est_tas a1 && est_tas a2;;

let rec recherche a x = match a with
| V           -> []
| N (n,a1,a2) -> if x = n      then [n]
                 else if x > n then let c = recherche a2 x in
                                    if c = [] then [] else n::c
                 else               let c = recherche a1 x in
                                    if c = [] then [] else n::c;;

let rec insert a x = match a with
| V           -> N (x,V,V)
| N (n,a1,a2) -> if x <= n     then N (n,insert a1 x,a2)
                 else               N (n,a1,insert a2 x);;
let rec construit_r l acc = match l with
| []   -> acc
| h::t -> construit_r t (insert acc h);;
let construit l = construit_r l V;;

let rec infix_r a acc = match a with
| V -> acc
| N (n,a1,a2) -> let ac = infix_r a2 acc in infix_r a1 (n :: ac);;
let infix a = infix_r a [];;

let tri l = infix (construit l);;
tri [3; 5; 7; 2; 1; 9; 6; 0; 3; 7; 4];;

(* }}} *)

(* {{{ Question 2 *)
let root a = let (N (n,_,_)) = a in n;;
let rec fusion a1 a2 = match a1 with
| V           -> a2
| N (n,s1,s2) -> match a2 with
                 | V -> a1
                 | N (q,q1,q2) -> if n <= q
                                   then N (n, s1, N (q, fusion s2 q1, q2))
                                   else fusion a2 a1;;

let rec extract a x = match a with
| V           -> V
| N (n,s1,s2) -> if n = x      then fusion s1 s2
                 else if x > n then N (n,s1,extract s2 x)
                 else               N (n,extract s1 x,s2);;

(* }}} *)

(* }}} *)

(* {{{ II - Arbres H-équilibrés *)
(* {{{ Question 4 *)
let rot_d a = match a with
| V                    -> V
| N (_,V,_)            -> a
| N (q,N (p,s1,s2),a2) -> N (p, s1, N (q, s2, a2));;
let rot_g a = match a with
| V                    -> V
| N (_,_,V)            -> a
| N (p,a1,N (q,s1,s2)) -> N (q, N (p, a1, s1), s2);;

(* }}} *)

(* {{{ Question 5 *)
let rot_gd a = match a with
| V                               -> V
| N (_,V,_)                       -> a
| N (_,N (_,_,V),_)               -> a
| N (r, N (p, a, N (q, b, c)), d) -> N (q, N (p, a, b), N (r, c, d));;

(* }}} *)

(* }}} *)

