(* vim:set foldmethod=marker: *)

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

let rec max3_rec e d f = match f - d with
| 1 -> (max e.(d) e.(f), [min e.(d) e.(f)])
| _ -> let m = (f+d)/2 in
       let (b1,g1) = max3_rec e d m in
       let (b2,g2) = max3_rec e (m+1) f in
       if b1 > b2 then (b1, b2::g1)
                  else (b2, b1::g2);;

let rec maxl l = match l with
| h::[] -> h
| h::t  -> let h2 = maxl t in if h > h2 then h else h2;;

let max3 e = 
    let (b,g) = max3_rec e 0 (vect_length e - 1) in
    (maxl g, b);;

max1 [|5; 8; 4; 7; 3; 6; 2; 1|];;
max2 [|5; 8; 4; 7; 3; 6; 2; 1|];;
max3 [|5; 8; 4; 7; 3; 6; 2; 1|];;

type arbre=Feuille of int | Noeud of int*bool*arbre*arbre;;
let aval a = match a with
| Feuille n       -> n
| Noeud (n,_,_,_) -> n;;
aval (Noeud (5, false, Feuille 0, Feuille 1));;

let rec tournoi e d f = match f - d with
| 0 -> Feuille e.(d)
| _ -> let m = (f+d) / 2 in
       let a1 = tournoi e d m in
       let a2 = tournoi e (m+1) f in
       let v1 = aval a1 and v2 = aval a2 in
       if v1 > v2 then Noeud (v1, true,  a1, a2)
                  else Noeud (v2, false, a1, a2);;
tournoi [|5; 8; 4; 7; 3; 6; 2; 1|] 0 7;;

let rec second a = match a with
| Noeud (_,b,Feuille n1,Feuille n2) -> if b then n2 else n1
| Noeud (_,b,a1,a2) -> let (n1,n2) = if b then (second a1, aval a2)
                                          else (second a2, aval a1) 
                       in max n1 n2;;
second (tournoi [|5; 8; 4; 7; 3; 6; 2; 1|] 0 7);;

let max4 e = let a = tournoi e 0 (vect_length e - 1) in (second a, aval a);;
max4 [|5; 8; 4; 7; 3; 6; 2; 1|];;

(* }}} *)

(* {{{ Exercice 3 *)
let rec pow n i = match i with
| 0 -> 1
| _ -> n * pow n (i-1);;

let chiffre j n =
    let p = pow 2 16 in
    let d = pow p j in
    (n / d) mod p;;

let tri tab res aux cle =
    let n = vect_length tab and k = vect_length aux in
    for i = 0 to k - 1 do aux.(i) <- 0; done;
    for i = 0 to n - 1 do aux.(cle(tab.(i))) <- aux.(cle(tab.(i))) + 1; done;
    for i = 1 to k - 1 do aux.(i) <- aux.(i) + aux.(i-1); done;
    for i = n - 1 downto 0 do
        let p = cle(tab.(i)) in
        res.(aux.(p)-1) <- tab.(i);
        aux.(p) <- aux.(p) - 1;
    done;;

let copy a b =
    let n = vect_length a in
    for i = 0 to n - 1 do
        b.(i) <- a.(i)
    done;;

let sort64 tab =
    let n = vect_length tab in
    let k = pow 2 16 in
    let aux = make_vect k 0 in
    let res = make_vect n 0 in
    for i = 0 to 3 do
        tri tab res aux (chiffre i);
        copy res tab;
    done;;

let a = [|25; 48; 96; 32; 14; 5; 7502; 35|];;
sort64 a;;
a;;

(* }}} *)

(* {{{ Exercice 4 *)
let swap e i j =
    let t = e.(i) in
    e.(i) <- e.(j);
    e.(j) <- t;;

let rec reor e a b = if b - a >= 3 then begin
    let m = (a+b)/2 in
    reor e a     m;
    reor e (m+1) b;
    let d = (b-a+1) / 4 in
    let n = (a+m) / 2 in
    for i = 1 to d do
        swap e (n + i) (n + i + d);
    done;
end;;

let a = [|0; 1; 2; 3; 4; 5; 6; 7|];;
reor a 0 7;;
a;;

(* }}} *)

(* {{{ Exercice 5 *)
let rec max_vect_r e a b = match b - a with
  0 -> a
| 1 -> if e.(a) >= e.(b) then a else b
| _ -> let m = (a+b) / 2 in
       if e.(m) >= e.(m-1) then if e.(m) >= e.(m+1)
                                   then m
                                   else max_vect_r e (m+1) b
                           else max_vect_r e a (m-1);;
let max_vect e = max_vect_r e 0 (vect_length e - 1);;

max_vect [|1; 2; 1; 4; 5; 4|];;

let check_r m i j n = if i == n - 1 then true else m.(i).(j) >= m.(i+1).(j);;
let check_l m i j n = if i == 0     then true else m.(i).(j) >= m.(i-1).(j);;
let check_u m i j n = if j == 0     then true else m.(i).(j) >= m.(i).(j-1);;
let check_d m i j n = if j == n - 1 then true else m.(i).(j) >= m.(i).(j+1);;
let test m i j =
    let n = vect_length m in
    check_r m i j n && check_l m i j n && check_u m i j n && check_d m i j n;;

let max_cl m j p q =
    let i = ref p in
    for k = p+1 to q do
        if m.(k).(j) > m.(!i).(j) then i := k
    done;
    !i;;
let max_ln m p i j =
    let q = ref i in
    for k = i+1 to j do
        if m.(p).(k) > m.(p).(!q) then q := k
    done;
    !q;;

let rec max_mat_r m i j p q k l =
    let n = vect_length m in
    let     ni = ref i and nj = ref j
        and np = ref p and nq = ref q
        and nk = ref k and nl = ref l in
    let m1 = (i + j) / 2 and m2 = (p + q) / 2 in
    let mc = max_cl m m1 p q in
    if test m m1 mc then (m1,mc) else begin
        if m.(mc).(m1) < m.(k).(l) then ni := m1 + 1
          else if check_l m m1 mc n then begin
              ni := m1 + 1;
              nk := m1 + 1;
              nl := mc;
          end else begin
              nj := m1 - 1;
              nk := m1 - 1;
              nl := mc;
          end;
        let ml = max_ln m m2 !ni !nj in
        if m.(m2).(ml) < m.(!nk).(!nl) then np := m2 + 1
          else if check_u m ml m2 n then begin
              np := m2 + 1;
              nk := m2 + 1;
              nl := ml;
          end else begin
              nq := m2 - 1;
              nk := m2 - 1;
              nl := ml;
          end;
        max_mat_r m !ni !nj !np !nq !nk !nl;
    end;;
let max_mat m = let n = vect_length m in max_mat_r m 0 (n-1) 0 (n-1) 0 0;;

let mat = [| [| 1; 2; 3; 4; 5;  6  |];
             [| 2; 3; 4; 5; 6;  7  |];
             [| 3; 4; 7; 6; 7;  8  |];
             [| 4; 5; 6; 7; 8;  9  |];
             [| 5; 6; 7; 8; 9;  10 |];
             [| 6; 7; 8; 9; 10; 9  |]; |];;

test mat 5 5;;
test mat 2 2;;
test mat 3 1;;
test mat 5 0;;

max_ln mat 2 0 3;;
max_ln mat 2 3 5;;
max_cl mat 5 0 5;;
max_cl mat 1 1 3;;

max_mat mat;;

(* }}} *)

