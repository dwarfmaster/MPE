(* vim:set foldmethod=marker: *)

(* Worst case : O(n^2) (cas d'une liste triée par ordre décroissant)
 * Best case  : O(n) (cas d'une liste triée par ordre croissant)
 * Average    : O(n^2) (cf https://en.wikipedia.org/wiki/Insertion_sort)
 *)

let move v i =
    let x = v.(i) in
    let j = ref i in
    while !j > 0 && x < v.(!j - 1) do
        v.(!j) <- v.(!j-1);
        j := !j - 1;
    done;
    v.(!j) <- x;;

let sort v =
    let n = vect_length v in
    for i = 1 to n - 1 do
        move v i;
    done;;

let v = [|5; 6; 9; 7; 2; 3; 1; 8; 5; 10; 2; 14; 15; 1; 16; 12; 30|];;
sort v;;
v;;

