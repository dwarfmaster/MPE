(* vim:set foldmethod=marker: *)

(* Worst case : O(n^2)
 * Average    : O(n*ln n)
 *)

(* {{{ Quicksort on lists *)
let rec split_l l x (l1,l2) = match l with
| []   -> (l1,l2)
| h::t -> if h < x then split_l t x (h::l1,l2)
                   else split_l t x (l1,h::l2);;

let rec sort_l_a l acc = match l with
| []    -> acc
| h::[] -> h :: acc
| h::t  -> let (l1,l2) = split_l t h ([],[]) in
           let l3 = sort_l_a l2 acc in
           sort_l_a l1 (h :: l3);;

let sort_l l = sort_l_a l [];;

sort_l [2; 8; 9; 25; 6; 3; 7; 1; 5; 9; 6; 1; 3; 8; 5; 2; 23; 65; 10];;

(* }}} *)

(* {{{ Quicksort on vectors *)
let swap v a b =
    let t = v.(a) in
    v.(a) <- v.(b);
    v.(b) <- t;;

(* Split vector between [a;b[ *)
let split_v v x a b =
    let n = vect_length v in
    let mn = ref a and mx = ref (b - 1) and i = ref a in
    while !i <= !mx do
        if v.(!i) = x then i := !i + 1
        else if v.(!i) < x then begin
            swap v !i !mn;
            mn := !mn + 1;
            i := !i + 1
        end else begin
            swap v !i !mx;
            mx := !mx - 1
        end
    done;
    (!mn,!mx + 1);;
(* Values under x in [a; mn[
 * Values above x in [mx + 1; b[
 *)

(* Sort between [a;b[ *)
let rec sort_v_r v a b =
    if b - a > 1 then begin
        let (mn,mx) = split_v v v.(a) a b in
        sort_v_r v a mn;
        sort_v_r v mx b
    end;;

let sort_v v = sort_v_r v 0 (vect_length v);;

let v = [|1; 5; 6; 3; 8; 9; 2; 56; 20; 85; 65; 41; 23; 96; 32; 52; 10; 45|];;
sort_v v;;
v;;

(* }}} *)

