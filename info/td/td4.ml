(* vim:set foldmethod=marker: *)

(* {{{ Exercice 1 *)
let mkadj l n =
    let adj = make_vect n [] in
    let r = ref l in
    while !r != [] do
        let (a,b) :: t = !r in
        adj.(a) <- b :: adj.(a);
        r := t;
    done;
    adj;;

mkadj [(0,2); (1,2); (2,3); (1,3)] 4;;

let coadj adj =
    let n = vect_length adj in
    let cadj = make_vect n [] in
    for i = 0 to n-1 do
        let l = ref adj.(i) in
        while !l != [] do
            let h::t = !l in
            cadj.(h) <- i :: cadj.(h);
            l := t;
        done;
    done;
    cadj;;

coadj (mkadj [(0,2); (1,2); (2,3); (1,3)] 4);;
mkadj [(2,0); (2,1); (3,2); (3,1)] 4;;

let rec has l x = match l with
| []   -> false
| h::t -> if x == h then true else has t x;;

let rec cat l1 l2 = match l1 with
| []   -> l2
| h::t -> if has l2 h then cat t l2
                      else cat t (h::l2);;

(* TODO: Peut s'accélérer en triant au passage les listes en les remplissant
 *)
let unorient adj =
    let n = vect_length adj in
    let c = coadj adj in
    let un = make_vect n [] in
    for i = 0 to n-1 do
        un.(i) <- cat adj.(i) c.(i);
    done;
    un;;

unorient (mkadj [(0,2); (2,0); (1,2); (2,3); (1,3)] 4);;

(* Pour un graphe donnée par sa matrice d'adjacence, on or'd la
 * matrice avec sa transposée : complexité O(n^2)
 *)

(* Ajout/retrait d'une arête :
 * - sur une matrice d'adjacence : O(1)
 * - sur des listes d'adjacence : O(A) dans le pire des cas, car il
 *      faut tester si l'arête n'y est pas déjà.
 *)

(* Ajout d'un sommet :
 * - sur une matrice d'adjacence : O(S^2)
 * - sur des listes d'adjacence : O(S)
 *)

(* Retrait d'un sommet :
 * - sur des listes d'adjacences : O(A*S), il faut supprimer la liste d'ajacence
 *      et les arêtes allant vers lui, puis renoméroter les sommets.
 * - sur une matrice d'adjacence : O(S^2), supprimer une ligne et une colonne de
 *      la matrice.
 *)

let rec llen l = match l with
| []   -> 0
| h::t -> 1 + llen t;;

let rec exhas l1 l2 = match l1 with
| []   -> []
| h::t -> if has l2 h then h :: exhas t l2
                      else exhas t l2;;

let extract g l =
    let ln = llen l in
    let adj = make_vect ln [] in
    let i = ref 0 in
    let r = ref l in
    while !r != [] do
        let h::t = !r in
        adj.(!i) <- exhas g.(h) l;
        i := !i + 1;
        r := t;
    done;
    adj;;

let rec exmap l1 m = match l1 with
| []   -> []
| h::t -> if m.(h) == -1 then exmap t m
                         else m.(h) :: exmap t m;;

let extract2 g l =
    let n = vect_length g in
    let nvals = make_vect n (-1) in
    let r = ref l in
    let i = ref 0 in
    while !r != [] do
        let h::t = !r in
        nvals.(h) <- !i;
        i := !i + 1;
        r := t;
    done;
    let adj = make_vect !i [] in
    for j = 0 to n-1 do
        if nvals.(j) != -1 then adj.(nvals.(j)) <- exmap g.(j) nvals
    done;
    adj;;

mkadj [(0,2); (1,2); (2,3); (1,3)] 4;;
extract (mkadj [(0,2); (1,2); (2,3); (1,3)] 4) [0; 2; 3];;
extract2 (mkadj [(0,2); (1,2); (2,3); (1,3)] 4) [0; 2; 3];;

(* }}} *)

(* {{{ Exercice 2 *)
let poids_minimal p i j =
    let n = vect_length p in
    let acc = make_matrix n n 0 in
    let aux = make_matrix n n 0 in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            acc.(i).(j) <- p.(i).(j);
        done;
    done;
    for k = 1 to n do
        for i = 0 to n-1 do
            for j = 0 to n-1 do
                aux.(i).(j) <- p.(i).(k) + p.(k).(j);
            done;
        done;
        for i = 0 to n-1 do
            for j = 0 to n-1 do
                acc.(i).(j) <- min acc.(i).(j) aux.(i).(j);
            done;
        done;
    done;
    acc.(i).(j);;

(* TODO chemin minimal *)
(* }}} *)

(* {{{ Exercice 3 *)
let trou_noir adj =
    let c = coadj adj in (* O(A) *)
    let n = vect_length adj in
    let v = make_vect n false in
    v.(0) <- true;
    let i = ref 0 in
    let ed = ref false in
    while not !ed && adj.(!i) != [] do
        i := hd adj.(!i);
        if v.(!i) then ed := true;
    done;
    not !ed && llen c.(!i) == n - 1;;

(* }}} *)

