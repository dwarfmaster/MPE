(* vim:set foldmethod=marker: *)

(* {{{ Exercice 1 *)
let r j l p =
    let v = make_vect (j+1) 0 in
    let n = vect_length l in
    for i = 1 to j do
        for k = 0 to n-1 do
            if l.(k) <= i then begin
                let t = p.(k) + v.(i-l.(k)) in
                if t > v.(i) then v.(i) <- t
            end;
        done;
    done;
    v.(j);;
(* complexité : O(jn) *)

(* Amélioration : plutôt que rechercher la valeur où est
 * atteinte le maximum dans une deuxième boucle, on pourrait
 * la stocker dans un tableau auxiliaire lors du premier
 * parcours.
 *)
let decoupe j l p =
    let v = make_vect (j+1) 0 in
    let n = vect_length l in
    let d = make_vect n 0 in
    for i = 1 to j do
        for k = 0 to n-1 do
            if l.(k) <= i then begin
                let t = p.(k) + v.(i-l.(k)) in
                if t > v.(i) then v.(i) <- t
            end;
        done;
    done;
    let i = ref j in
    while !i > 0 do
        let km = ref (-1) in
        let ni = ref 0 in
        for k = 0 to n-1 do
            if l.(k) <= !i then begin
                let t = p.(k) + v.(!i-l.(k)) in
                if t == v.(!i) then begin
                    km := k;
                    ni := !i - l.(k)
                end
            end
        done;
        if !km >= 0 then d.(!km) <- d.(!km) + 1;
        i := !ni
    done;
    d;;
(* complexité : O(jn) *)

r 19 [|2; 3; 4|] [|1; 3; 7|];;
decoupe 19 [|2; 3; 4|] [|1; 3; 7|];;

(* }}} *)

(* {{{ Exercice 2 *)
type Point = { x : float; y : float };;

let square x = x*.x;;

let longueur p i j = match abs (i-j) with
| 1 -> 0.
| _ -> sqrt (square (p.(i).x -. p.(j).x) +. square (p.(i).y -. p.(j).y));;

let rec conc a b = match a with
| []   -> b
| h::t -> conc t (h::b);;

let rec rem d i j = match j - i with
| 0 -> []
| 1 -> []
| 2 -> if d.(i).(j) == i then [(i,j)]
                         else [(i-1,i+1)]
| _ -> let k = d.(i).(j) in
       (i-1,k) :: (k,j) :: conc (rem d i k) (rem d (k+1) j);;

let triangulation poly =
    let n = vect_length poly in
    let m = make_matrix n n 0.0 in
    let d = make_matrix n n 0 in
    m.(n-1).(n-1) <- 0.0;
    for i = n-2 downto 1 do
        m.(i).(i) <- 0.0;
        m.(i).(i+1) <- 0.0;
        for j = i+2 to n-1 do
            m.(i).(j) <-    m.(i).(i) 
                         +. m.(i+1).(j)
                         +. longueur poly (i-1) i
                         +. longueur poly i j;
            d.(i).(j) <- i;
            for k = i + 1 to j - 1 do
                let t =    m.(i).(k)
                        +. m.(k+1).(j)
                        +. longueur poly (i-1) k
                        +. longueur poly k j in
                if t < m.(i).(j) then begin
                    m.(i).(j) <- t;
                    d.(i).(j) <- k;
                end;
            done;
        done;
    done;
    (m.(1).(n-1), rem d 1 (n-1));;
triangulation [|{x=1.0; y=2.0}; {x=1.0; y=4.0}; {x=4.0; y=5.0}; {x=4.0; y=3.0}|];;

(* O(n^3) *)

(* }}} *)

(* {{{ Exercice 3 *)
let print_vect v =
    let n = vect_length v in begin
        print_string "[|";
        for i = 0 to n - 2 do
            print_float v.(i);
            print_string "; ";
        done;
        print_float v.(n-1);
        print_string "|]";
        print_string "\n";
    end;;

let pmax a t l =
    let snd (x,y) = y in
    let i = ref 0 in
    while snd t.(!i) < l do
        i := !i + 1
    done;
    !i - 1;;

let rec rem b j i = match i with
| _ when i < 0 -> []
| _            -> let n = j.(i) in if b.(i) then i :: rem b j n else rem b j n;;

let prix_max t p =
    let n = vect_length t in
    let a = make_vect n p.(0) in
    let j = make_vect n (-1) in
    let b = make_vect n true in
    for i = 1 to n-1 do
        let (ai,_) = t.(i) in
        let imax = pmax a t ai in
        let pm1 = if imax < 0 then 0.0 else a.(imax - 1) in
        let pm = p.(i) +. pm1 in
        if pm > a.(i-1) then begin
            a.(i) <- pm;
            b.(i) <- true;
            j.(i) <- imax;
        end else begin
            a.(i) <- a.(i-1);
            b.(i) <- false;
            j.(i) <- i - 1;
        end
    done;
    (a.(n-1), rem b j (n-1));;
(* O(n^2) *)

let t = [|(0.1, 10.0); (5.0, 11.0); (7.0, 13.0); (12.0, 14.0)|];;
let p = [|   50.0;         20.0;       100.0;        60.0    |];;
prix_max t p;;

(* }}} *)

(* {{{ Exercice 4 *)
let rec conc a b = match a with
| []   -> b
| h::t -> conc t (h::b);;

let rec rem h is i =
    let n = vect_length h.(i) in
    if is.(i) then i :: rrrem h is i (n - 1)
              else rrem h is i (n - 1)
and rrem h is i j = match j with
| -1 -> []
| _  -> conc (rem h is h.(i).(j)) (rrem h is i (j-1))
and rrrem h is i j = match j with
| -1 -> []
| _  -> conc (rrem h is h.(i).(j) (vect_length h.(h.(i).(j)) - 1)) (rrrem h is i (j-1));;

let invites h c =
    let n = vect_length h in
    let mcs = make_vect n (-1) in
    let is  = make_vect n false in
    let rec sumsub i =
        let m = vect_length h.(i) in
        let s = ref 0 in
        for j = 0 to m - 1 do
            s := !s + get h.(i).(j)
        done;
        !s;
    and sumsubsub i =
        let m = vect_length h.(i) in
        let s = ref 0 in
        for j = 0 to m - 1 do
            s := !s + sumsub h.(i).(j)
        done;
        !s;
    and get i =
        if mcs.(i) == -1 then begin
            let ss = sumsub i and sss = sumsubsub i in
            if c.(i) + sss > ss then begin
                is.(i) <- true;
                mcs.(i) <- c.(i) + sss
            end else mcs.(i) <- ss
        end;
        mcs.(i)
    in let g = get 0 in (g, rem h is 0);;

let c = [| 0; 50; 10; 12; 9; 10; 10; 1; 2; 8; 30|];;
let h = [| [| 1; 5 |];
           [| 2; 3; 4 |];
           [| |]; [| |]; [| |];
           [| 6; 10 |];
           [| 7; 8; 9 |];
           [| |]; [| |]; [| |]; [| |]
        |];;
invites h c;;

(* }}} *)

(* {{{ Exercice 5 *)
type Point = { x : float; y : float };;

let dist p1 p2 = sqrt ((p1.x -. p2.x) *. (p1.x -. p2.x) +. (p1.y -. p2.y) *. (p1.y -. p2.y));;

let rec chemin m i j = match j with
| 1 -> ([],[1])
| _ when i < j - 1 -> let (p1,p2) = chemin m i (j-1) in (p1,j::p2)
| _ -> let (p1,p2) = chemin m m.(j) (j-1) in (p2,j::p1);;

let rec adapt (a,b) = match a with
| []   -> b
| h::t -> adapt (t,b) @ [h];;

let commerce poly =
    let n = vect_length poly in
    let c = make_matrix n n 0.0 in
    let m = make_vect n 0 in
    for j = 1 to n - 1 do
        for i = 0 to j - 2 do c.(i).(j) <- c.(i).(j) +. dist poly.(j-1) poly.(j) done;
        c.(j-1).(j) <- c.(0).(j-1) +. dist poly.(0) poly.(j);
        for k = 1 to j - 2 do
            let t = c.(k).(j-1) +. dist poly.(k) poly.(j) in
            if t < c.(j-1).(j) then begin
                c.(j-1).(j) <- t;
                m.(j) <- k;
            end;
        done;
    done;
    (c.(n-2).(n-1) +. dist poly.(n-2) poly.(n-1), 0 :: adapt (chemin m (n-2) (n-1)));;

let p = [| {x = 1.0; y = 5.1}; {x = 2.0; y = 1.1};
           {x = 3.0; y = 2.1}; {x = 4.0; y = 1.1};
           {x = 5.0; y = 4.1}; {x = 7.0; y = 2.1}
        |];;
commerce p;;

(* }}} *)

