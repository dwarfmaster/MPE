
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

let triangulation poly =
    let n = vect_length poly in
    let m = make_matrix n n 0.0 in
    m.(n-1).(n-1) <- 0.0;
    for i = n-2 downto 1 do
        m.(i).(i) <- 0.0;
        m.(i).(i+1) <- 0.0;
        for j = i+2 to n-1 do
            for k = i to j do
                let t = m.(i).(k) +. m.(i+1).(j) +. longueur poly (i-1) k +. longueur poly k j in
                if t > m.(i).(j) then m.(i).(j) <- t;
            done;
        done;
    done;
    m.(1).(n-1);;
(* O(n^3) *)

(* }}} *)

