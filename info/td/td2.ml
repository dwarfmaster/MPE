(* {{{ Exercice 1 *)
(* {{{ Approche naive *)
let amplitude1 e =
    let n = vect_length e in
    let m = ref 0 in
    for i = 0 to n-2 do
        for j = i+1 to n-1 do
            let diff = abs (e.(i) - e.(j)) in
            if !m < diff then
                m := diff
        done;
    done;
    !m;;
(* }}} *)

let amplitude e =
    let n = vect_length e in
    let mn = ref e.(0) in
    let mx = ref e.(0) in
    for i = 0 to n - 1 do
        if e.(i) > !mx then mx := e.(i);
        if e.(i) < !mn then mn := e.(i);
    done;
    !mx - !mn;;

let gain_naif e =
    let n = vect_length e in
    let m = ref 0 in
    for i = 0 to n-2 do
        for j = i+1 to n-1 do
            let d = e.(j) - e.(i) in
            if d > !m then m := d;
        done;
    done;
    !m;;

let rec gain_rec e d f = match f-d with
| 1 -> [|e.(d); e.(d); 0|]
| _ -> let t1 = gain_rec e d ((f+d)/2) in
       let t2 = gain_rec e ((f+d)/2) f in
       let mn = min t1.(0) t2.(0) in
       let mx = max t1.(1) t2.(1) in
       let df = max (max t1.(2) t2.(2)) (t2.(1) - t1.(0)) in
       [|mn; mx; df|];;
let gain e =
    let t = gain_rec e 0 (vect_length e) in t.(2);;

let gain_din e =
    let n = vect_length e in
    let gi = make_vect n 0 in
    let m = ref gi.(0) in
    let imn = ref 0 and imx = ref 0 in
    for i = 1 to n-1 do
        gi.(i) <- max 0 (e.(i) - e.(i-1) + gi.(i-1));
        if !m < gi.(i) then begin
            m := gi.(i);
            imx := i;
            if gi.(i) == 0 then imn := i;
        end;
    done;
    (!m, !imn, !imx);;

gain_naif [| 1; 2; 4; 6; 3; 1|];;
gain [| 1; 2; 4; 6; 3; 1|];;
gain_din [| 1; 2; 4; 6; 3; 1 |];;

let sum_max e =
    let n = vect_length e in
    let si = make_vect n e.(0) in
    let m = ref e.(0) in
    for i = 1 to n-1 do
        si.(i) <- e.(i) + max 0 si.(i-1);
        m := max !m si.(i);
    done;
    !m;;
sum_max [|1; 2; 3; -3; 1|];;

(* }}} *)

(* {{{ Exercice 2 *)
let max1 e =
    let n = vect_length e in
    let mmx = ref (max e.(0) e.(1)) in
    let mmn = ref (min e.(0) e.(1)) in
    for i = 2 to n - 1 do
        if e.(i) > !mmx then begin
            mmn := !mmx;
            mmx := e.(i);
        end
        else if e.(i) > !mmn then mmn := e.(i);
    done;
    (!mmn, !mmx);;

let rec max2_rec e d f = match f-d with
| 1 -> (min e.(d) e.(f), max e.(d) e.(f))
| _ -> let m = (f+d)/2 in
       let (a,b) = max2_rec e d m in
       let (c,d) = max2_rec e (m+1) f in
       if b < d then (max b c, d)
                else (max a d, b);;
let max2 e = max2_rec e 0 (vect_length e - 1);;

(* TODO faux *)
let rec max3_rec e d f = match f - d with
| 1 -> (max e.(d) e.(f), [min e.(d) e.(f)])
| _ -> let m = (f+d)/2 in
       let (g1, p1) = max3_rec e d m in
       let (g2, p2) = max3_rec e (m+1) f in
       if g1 < g2 then (g2, g1::p1)
                  else (g1, g2::p2);;
let max3 e = max3_rec e 0 (vect_length e - 1);;

max1 [|5; 8; 4; 7; 3; 6; 2; 1|];;
max2 [|5; 8; 4; 7; 3; 6; 2; 1|];;
max3 [|5; 8; 4; 7; 3; 6; 2; 1|];;

(* }}} *)

