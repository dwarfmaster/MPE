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

(* {{{ Répartition de paires de ski *)
let paires_ski h s =
    let m = vect_length s in
    let n = vect_length h in
    let l = make_vect (m-n+1) (abs (h.(0) - s.(0))) in
    let d = make_matrix n (m-n+1) false in
    for j = 1 to m - n do
        if l.(j-1) < abs (h.(0) - s.(j)) then begin
            d.(0).(j) <- true;
            l.(j) <- l.(j-1);
        end else l.(j) <- abs (h.(0) - s.(j))
    done;
    for i = 1 to n - 1 do
        l.(0) <- l.(0) + abs (h.(i) - s.(i));
        for j = 1 to m - n do
            let t = l.(j) + abs (h.(i) - s.(i+j)) in
            if t < l.(j-1) then l.(j) <- t
            else begin
                l.(j) <- l.(j-1);
                d.(i).(j) <- true;
            end;
        done;
    done;
    let r = make_vect n 0 in
    let rec attribution i j = match (i,j) with
    | (0,0)         -> r.(0) <- 0
    | (0,_)         -> if d.(0).(j) then attribution i (j-1) else r.(0) <- j
    | _ when j == 0 -> begin
                r.(i) <- i;
                attribution (i-1) j;
            end;
    | _             -> if d.(i).(j) then attribution i (j-1)
                       else begin
                           r.(i) <- i + j;
                           attribution (i-1) j;
                       end;
    in attribution (n-1) (m-n); r;;

paires_ski [|0; 1; 2; 3; 5; 9|] [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10|];;

(* }}} *)

