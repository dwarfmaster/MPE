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

let rec insert x l = match l with
| []     -> x::[]
| h :: t -> if h < x       then h :: insert x t
            else if h == x then l
                           else x :: l;;

let unorient adj =
    let n = vect_length adj in
    let un = make_vect n [] in
    for i = 0 to n-1 do
        let l = ref adj.(i) in
        while !l <> [] do
            let h::t = !l in
            un.(i) <- insert h un.(i);
            un.(h) <- insert i un.(h);
            l := t;
        done;
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

let chemins_minimal p i j =
    let n = vect_length p in
    let acc = make_matrix n n 0 in
    let aux = make_matrix n n 0 in
    let ant = make_matrix n n 0 in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            acc.(i).(j) <- p.(i).(j);
            ant.(i).(j) <- i;
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
                if acc.(i).(j) < aux.(i).(j) then
                    acc.(i).(j) <- acc.(i).(j)
                else begin
                    acc.(i).(j) <- aux.(i).(j);
                    ant.(i).(j) <- k;
                end;
            done;
        done;
    done;
    let rec rem a b = if b = a then []
    else b :: rem a ant.(a).(b)
    in rem i j;;

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

let trou_noir_df adj =
    let n = vect_length adj in
    let count = make_vect n 0 in
    let rec parcours i =
        count.(i) <- count.(i) + 1;
        if count.(i) = 1 then begin
            let nxts = ref adj.(i) in
            while !nxts <> [] do
                let h::t = !nxts in
                nxts := t;
                parcours h;
            done;
        end;
    in for i = 0 to n-1 do
        if count.(i) = 0 then parcours i
    done;
    let tn = ref (-1) in
    let i = ref 0 in
    while !tn < 0 && !i < n do
        if count.(!i) = n-1 && adj.(!i) = [] then tn := !i;
        i := !i + 1;
    done;
    !tn;;

let trou_noir_matrix m =
    let n = vect_length m in
    let i = ref 0 in
    let ed = ref false in
    (* Boucle exécutée en O(A) (les deux boucle ne sont pas indépendantes) *)
    while not !ed && !i < n do
        let j = ref !i in
        while !j < n && m.(!i).(!j) == 0 do
            j := !j + 1;
        done;
        if !j == n then ed := true
                   else i := !j;
    done;
    if !i == n then (-1) else begin
        let is = ref true in
        (* Autre boucle en O(A) *)
        for k = 0 to n-1 do
            is := !is && m.(!i).(k) = 0;
            if k <> !i then is := !is && m.(k).(!i) = 1;
        done;
        if !is then !i else (-1)
    end;;

(* }}} *)

(* {{{ Exercice 4 *)

let rec rev_cat l1 l2 = match l1 with
| []   -> l2
| h::t -> rev_cat t (h::l2);;

let cycle_eulerien g =
    let n = vect_length g in
    let adj = g in
    let rec cycle n =
        let ccl = ref [n] in
        let rec follow i = if adj.(i) <> [] then begin
            let h::t = adj.(i) in
            adj.(i) <- t;
            if h <> n then begin
                ccl := h :: !ccl;
                follow h
            end;
        end;
        in let rec rem l = match l with
        | []   -> []
        | h::t -> if adj.(h) = [] then h :: rem t
                  else let c2 = cycle h in rev_cat c2 (h :: rem t)
        in follow n; rem !ccl
    in 0::cycle 0;;

cycle_eulerien [| [1;3]; [0;2]; [3]; [0;1] |];;

(* }}} *)

