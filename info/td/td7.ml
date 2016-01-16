(* vim:set foldmethod=marker: *)

(* {{{ Types *)
type auto_det = {initial   : int;
                 terminaux : int list;
                 delta     : (int vect) vect
                };;
type auto_gen = {initiaux  : int list;
                 finaux    : int list;
                 Delta     : (int*int) list vect
                };;
type expr_rat = Eps
              | Lettre of int
              | Etoile of expr_rat
              | Concat of expr_rat*expr_rat
              | Sum    of expr_rat*expr_rat
              ;;
type lang_loc = {I    : int list;
                 S    : int list;
                 F    : (int*int) list;
                 vide : bool
                };;

(* }}} *)

(* {{{ Fonctions générales *)
(* {{{ Q1 *)
let rec insert l a = match l with
| []     -> [a]
| (h::t) -> if a > h then h::insert t a
                     else a::l;;
insert [1;2;3;5;6;7] 4;;

let rec fusion l1 l2 = match (l1,l2) with
| ([],[])         -> []
| ([],_)          -> l2
| (_,[])          -> l1
| (h1::t1,h2::t2) -> if h1 = h2 then h1 :: fusion t1 t2 else
                     if h1 < h2 then h1 :: fusion t1 l2
                                else h2 :: fusion l1 t2;;
fusion [1;3;6;8;10;13;15;17;36] [2;5;7;9;10;36];;

let rec commun l1 l2 = match (l1,l2) with
| ([],_)          -> false
| (_,[])          -> false
| (h1::t1,h2::t2) -> if h1 = h2 then true else
                     if h1 < h2 then commun t1 l2
                                else commun l1 t2;;
commun [1;3;5;7;9] [2;4;6;8;10];;
commun [1;3;5;7;9;10] [2;4;6;8;10];;

(* }}} *)

(* {{{ Q2 *)
let rec mkcouples a l = match l with
| []   -> []
| h::t -> (a,h) :: mkcouples a t;;

let rec prod l1 l2 = match l1 with
| []   -> []
| h::t -> mkcouples h l2 @ prod t l2;;
prod [1;3;5;7;9] [2;4;6;8];;

(* }}} *)

(* {{{ Q3 *)
let rec test_auto_det_from au q m = if q = -1 then false else match m with
| []   -> commun au.terminaux [q]
| h::t -> test_auto_det_from au au.delta.(q).(h) t;;

let test_auto_det au m = test_auto_det_from au au.initial m;;

(* }}} *)

(* {{{ Q4 *)
let rec auto_gen_access_from_elem a l = match l with
| []       -> []
| (x,y)::t -> if a = x then y :: auto_gen_access_from_elem a t
                       else      auto_gen_access_from_elem a t;;

let auto_gen_access_from au a p =
    let access = ref [] in
    let ps = ref p in
    while !ps <> [] do
        let h::t = !ps in
        ps := t;
        access := fusion !access (auto_gen_access_from_elem a au.Delta.(h));
    done;
    !access;;

(* }}} *)

(* {{{ Q5 *)
let rec test_auto_gen_from au p m = if p = [] then false else match m with
| []   -> commun au.finaux p
| h::t -> test_auto_gen_from au (auto_gen_access_from au h p) t;;

let rec test_auto_gen au m = test_auto_gen_from au au.initiaux m;;

(* }}} *)

(* }}} *)

(* {{{ Algorithme de Berry-Sethi *)
let la = Lettre 0 and lb = Lettre 1;;
let expression = Concat (la, (Etoile (Sum (la, lb))));;

(* {{{ Tools *)
let concat_vect v1 v2 =
    let n1 = vect_length v1 and n2 = vect_length v2 in
    let v = make_vect (n1 + n2) v1.(0) in
    for i = 0 to n1 - 1 do
        v.(i) <- v1.(i)
    done;
    for i = 0 to n2 - 1 do
        v.(n1+i) <- v2.(i)
    done;
    v;;
concat_vect [|3; 4; 5|] [|6;7;8|];;
(* }}} *)

(* {{{ Q8 *)
let rec linearise_from expr n = match expr with
| Eps            -> (Eps, [||], 0)
| Lettre l       -> (Lettre n, [|l|], 1)
| Etoile e       -> let (ne, v, n2) = linearise_from e n in
                    (Etoile ne, v, n2)
| Concat (e1,e2) -> let (ne1, v1, m1) = linearise_from e1 n in
                    let (ne2, v2, m2) = linearise_from e2 (n+m1) in
                    (Concat (ne1,ne2), concat_vect v1 v2, m1 + m2)
| Sum (e1,e2)    -> let (ne1, v1, m1) = linearise_from e1 n in
                    let (ne2, v2, m2) = linearise_from e2 (n+m1) in
                    (Sum (ne1,ne2), concat_vect v1 v2, m1 + m2);;
let linearise expr = linearise_from expr 1;;
linearise expression;;

(* }}} *)

(* {{{ Q9 *)
let rec calcule_lang_local expl = match expl with
| Eps            -> {I = []; S = []; F = []; vide=true}
| Lettre l       -> {I = [l]; S = [l]; F = []; vide=false}
| Etoile e       -> let l = calcule_lang_local e in
                    {I = l.I; S = l.S; F = fusion l.F (prod l.S l.I); vide=true}
| Concat (e1,e2) -> let l1 = calcule_lang_local e1
                        and l2 = calcule_lang_local e2 in
                    let p = if l1.vide then fusion l1.I l2.I else l1.I in
                    let s = if l2.vide then fusion l1.S l2.S else l2.S in
                    let f = fusion l1.F (fusion l2.F (prod l1.S l2.I)) in
                    {I = p; S = s; F = f; vide = l1.vide && l2.vide}
| Sum (e1,e2)    -> let l1 = calcule_lang_local e1
                        and l2 = calcule_lang_local e2 in
                    let p = fusion l1.I l2.I in
                    let s = fusion l1.S l2.S in
                    let f = fusion l1.F l2.F in
                    {I = p; S = s; F = f; vide = l1.vide || l2.vide};;
let (l,_,_) = linearise expression in calcule_lang_local l;;

(* }}} *)

(* {{{ Q10 *)
let calcule_auto_local ll n =
    let init = [0] and fin = ll.S in
    let delt = make_vect (n+1) [] in
    let i = ref ll.I in
    while !i <> [] do
        let h::t = !i in
        i := t;
        delt.(0) <- (h,h) :: delt.(0);
    done;
    let fs = ref ll.F in
    while !fs <> [] do
        let (a,b)::t = !fs in
        fs := t;
        delt.(a) <- (b,b) :: delt.(a);
    done;
    {initiaux = init; finaux = fin; Delta = delt};;

(* }}} *)

(* {{{ Q11 *)
let automate expr =
    let (n_expr, vlettres, n) = linearise expr in
    let langl = calcule_lang_local n_expr in
    let auto  = calcule_auto_local langl n in
    for i = 0 to n do
        let ntrans = ref [] in
        let trans = ref auto.Delta.(i) in
        while !trans <> [] do
            let (a,b)::t = !trans in
            trans := t;
            ntrans := (vlettres.(a-1), b) :: !ntrans;
        done;
        auto.Delta.(i) <- !ntrans;
    done;
    auto;;
automate expression;;

(* }}} *)

(* }}} *)

(* {{{ Playing *)
let test exp m =
    let auto = automate exp in test_auto_gen auto m;;
let la = Lettre 0 and lb = Lettre 1 and lc = Lettre 2 and ld = Lettre 3;;
let mexp = (Concat ((Etoile (Sum (la,lb))),(Concat (lc,(Concat ((Etoile (Sum (la,lb))),(Concat (ld,(Concat ((Etoile (Sum (la,lb))),(Concat (lc,(Concat ((Sum (la,lb)),(Sum (la,lb))))))))))))))));;

let rec convert_r m i n = if i >= n then [] else match m.[i] with
| `a` -> 0 :: convert_r m (i+1) n
| `b` -> 1 :: convert_r m (i+1) n
| `.` -> 2 :: convert_r m (i+1) n
| `@` -> 3 :: convert_r m (i+1) n
|  _  -> 4 :: convert_r m (i+1) n;;
let convert m = convert_r m 0 (string_length m);;
convert "aababa.ababa@abab.ba";;

test mexp (convert "aabababa.ababa@ababaa.ba");;
test mexp (convert "babbabab.baa@ababa.ab");;
test mexp (convert "abbaba.abab@aba.b");;

(* }}} *)

(* {{{ Déterminisation *)
(* {{{ Q12 *)

(* }}} *)

(* }}} *)

