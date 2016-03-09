
(* On nomme chevauchement de x,y le plus long mot qui est à la fois suffixe
 * strict de y et préfixe strict de x.
 *)

(* L'algorithme de Knuth Morris Pratt cherche le motif t dans s.  Retourne
 * l'indice de t dans s si trouvé, -1 sinon.
 * Voir [0] pour une description pas à pas.
 * [0] https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm
 * Complexité en O(lt + ls)
 *)
let knuth_morris_pratt s t =
    let ls = string_length s and lt = string_length t in
    (* r.(j) stocke le chevauchement de t.(0:j-1) avec lui-même. Plus
     * précisément, on a t.(0:r(j)) = t.(j-1-r(j):j-1)
     *)
    let r = make_vect lt 0 in
    r.(0) <- -1;
    (* j marque la fin du chevauchement initial dans t.(0:i-1) *)
    let j = ref (-1) in
    for i = 1 to lt - 1 do
        while !j >= 0 && t.[i-1] != t.[!j] do
            j := r.(!j)
        done;
        j := !j + 1;
        r.(i) <- !j
    done;
    (* À partir de maintenant, j indique l'indice dans t que l'on considère *)
    j := 0;
    let ret = ref (-1) in
    (* i est l'indice de s que l'on considère *)
    let i = ref 0 in
    while !i < ls && !j < lt do
        (* Si s.[!i] != t.[!j], il n'est pas nécessaire de recommencer la
         * recherche de 0. En effet, si t.[0:!j-1] est un suffixe de
         * s.[0:!i-1], le chevauchement de t.[0:!j-1] avec lui même permet
         * de savoir jusqu'où t coïnide déjà.
         *)
        while !j >=0 && s.[!i] != t.[!j] do
            j := r.(!j)
        done;
        j := !j + 1;
        if !j = lt then ret := !i - lt + 1;
        i := !i + 1
    done;
    !ret;;

knuth_morris_pratt "atitatitita" "titi";;
knuth_morris_pratt "atitatitita" "tita";;
knuth_morris_pratt "atitatitita" "tata";;

