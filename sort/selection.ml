
(* Worst case : O(n^2)
 * Best case  : O(n^2)
 * Average    : O(n^2)
 *)

let min_v v s =
    let n = vect_length v in
    let m = ref s in
    for i = s to n - 1 do
        if v.(i) < v.(!m) then m := i
    done;
    !m;;

let swap v a b =
    let t = v.(a) in
    v.(a) <- v.(b);
    v.(b) <- t;;

let sort v =
    for i = 0 to vect_length v - 2 do
        let m = min_v v i in swap v i m
    done;;

let v = [|2; 5; 6; 8; 9; 3; 1; 4; 5; 3; 7; 6; 2; 1; 6|];;
sort v;;
v;;

