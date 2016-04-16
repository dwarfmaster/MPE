(* vim:set foldmethod=marker: *)

(* Worst case : O(n * ln n)
 * Best case  : O(n * ln n)
 * Average    : O(n * ln n)
 *)

(* {{{ On lists *)

let rec merge_l l1 l2 = match (l1,l2) with
| ([],    [])     -> []
| ([],    _)      -> l2
| (_,     [])     -> l1
| (h1::t1,h2::t2) -> if h1 < h2 then h1 :: merge_l t1 l2
                                else h2 :: merge_l l1 t2;;

(* Split list l in two sublists *)
let rec split l (a1,a2) = match l with
| []   -> (a1,a2)
| h::t -> split t (h::a2,a1);;

let rec sort l = match l with
| []  -> []
| [h] -> [h]
| _   -> let (l1,l2) = split l ([],[]) in
         merge_l (sort l1) (sort l2);;

sort [2; 5; 6; 3; 8; 4; 1; 9; 6; 7; 8; 0; 2; 3; 4; 0; 6; 9; 7];;

(* }}} *)

(* {{{ On vectors *)
(* Merge [a,b[ and [b;c[ parts of v into [a;c[ part *)
let merge_v v a b c =
    let i1 = ref a and i2 = ref b in
    let v2 = make_vect (c-a) 0 and i = ref 0 in
    while !i1 <> b || !i2 <> c do
        if !i1 = b then begin
            v2.(!i) <- v.(!i2);
            i2 := !i2 + 1
        end else if !i2 = c then begin
            v2.(!i) <- v.(!i1);
            i1 := !i1 + 1
        end else if v.(!i1) < v.(!i2) then begin
            v2.(!i) <- v.(!i1);
            i1 := !i1 + 1
        end else begin
            v2.(!i) <- v.(!i2);
            i2 := !i2 + 1;
        end;
        i := !i + 1;
    done;
    for j = 0 to c - a - 1 do
        v.(j + a) <- v2.(j)
    done;;

let rec sort_v_r v a b =
    if b - a > 1 then begin
        let m = (a + b) / 2 in
        sort_v_r v a m;
        sort_v_r v m b;
        merge_v  v a m b
    end;;

let sort_v v = sort_v_r v 0 (vect_length v);;

let v = [|2; 5; 9; 6; 3; 4; 8; 7; 5; 6; 1; 2; 3; 0; 5; 9|];;
sort_v v;;
v;;

(* }}} *)

