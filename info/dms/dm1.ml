(* vim:set foldmethod=marker: *)

(* {{{ Partie 1 *)
let rec est_un_systeme l = match l with
| []        -> false
| h::[]     -> h == 1
| h1::h2::t -> h1 > h2 && est_un_systeme (h2::t);;

est_un_systeme [5;2;1];;
est_un_systeme [5;7;1];;
est_un_systeme [7;5;2];;

(* }}} *)

(* {{{ Partie 2 *)
let retour x c =
    let cv = vect_of_list c in
    let m  = vect_length cv in
    let r  = make_vect (x+1) (-1) in
    let ps = make_vect (x+1) [] in
    r.(0) <- 0;
    for y = 1 to x do
        for i = 0 to m - 1 do
            if cv.(i) <= y then begin
                if r.(y) < 0 then begin
                    r.(y)  <- 1 + r.(y - cv.(i));
                    ps.(y) <- i :: ps.(y - cv.(i))
                end
                else if r.(y) > 1 + r.(y - cv.(i)) then begin
                    r.(y)  <- 1 + r.(y - cv.(i));
                    ps.(y) <- i :: ps.(y - cv.(i))
                end
            end
        done;
    done;
    (ps,r);;

retour 5 [5;2;1];;

let poids_minimaux x c =
    let cv = vect_of_list c in
    let m  = vect_length cv in
    let r  = make_vect (x+1) (-1) in
    r.(0) <- 0;
    for y = 1 to x do
        for i = 0 to m - 1 do
            if cv.(i) <= y then begin
                if r.(y) < 0 then begin
                    r.(y)  <- 1 + r.(y - cv.(i));
                end
                else if r.(y) > 1 + r.(y - cv.(i)) then begin
                    r.(y)  <- 1 + r.(y - cv.(i));
                end
            end
        done;
    done;
    tl (list_of_vect r);;

poids_minimaux 5 [5;2;1];;

(* }}} *)

(* {{{ Partie 3 *)
let rec glouton x c = match x with
| 0 -> []
| _ -> let h::t = c in
       let r  = x / h in r :: glouton (x - r * h) t;;

glouton 13 [5;2;1];;

(* }}} *)

(* {{{ Partie 4 *)
let rec lsum l = match l with
| []   -> 0
| h::t -> h + lsum t;;
lsum [5;2;1];;

let kozen_zaks c =
    let v = vect_of_list c in
    let m = vect_length v in
    let d = v.(m-3) + 2 and f = v.(0) + v.(1) + 1 in
    let r = ref true in
    for x = d to f do
        for i = 0 to m - 1 do
            if v.(i) < x then begin
                let g = glouton x c and g2 = glouton (x - v.(i)) c in
                if lsum g < lsum g2 - 1 then r := false;
            end;
        done;
    done;
    !r;;

kozen_zaks [5;2;1];;
kozen_zaks [6;5;1];;

(* }}} *)

