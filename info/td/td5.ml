(* vim:set foldmethod=marker: *)

(* {{{ Exercice 1 *)
let cc g =
    let n = vect_length g in
    let i = ref 0 in
    let c = make_vect n (-1) in
    let rec mark node comp = (* L'argument comp n'est pas nécessaire
                              * il suffirait d'utiliser !i
                              *)
        c.(node) <- comp;
        let nxts = ref g.(node) in
        while !nxts <> [] do
            let (h::t) = !nxts in
            if c.(h) == -1 then mark h comp;
            nxts := t
        done;
    in for j = 0 to n-1 do
        if c.(j) == -1 then begin
            mark j !i;
            i := !i + 1;
        end;
    done;
    c;;

cc [| [2]; [3]; [0]; [1] |];;

(* }}} *)

(* {{{ Exercice 2 *)
let has_cycle g =
    let n = vect_length g in
    let t = make_vect n false in
    let ok = ref true in
    let rec explore nd fr =
        if t.(nd) then ok := false
        else begin
            t.(nd) <- true;
            let nxts = ref g.(nd) in
            while !ok && !nxts <> [] do
                let (h::t) = !nxts in
                if h <> fr then explore h nd;
                nxts := t
            done;
        end;
    in let i = ref 0 in
    while !ok && !i < n do
        if t.(!i) == false then
            explore !i (-1)
        i := !i + 1;
    done;
    !ok ;;

let g = [| [1; 2; 3]; [0]; [0; 3]; [0; 2]|];;
has_cycle g;;

(* }}} *)

(* {{{ Exercice 3 *)
type file = { rgh : int list;
              lft : int list
            };;
let empty_file = { rgh = []; lft = [] } ;;
let enfile f e = { rgh = e::f.rgh; lft = f.lft };;
let move f = 
    let rec emp t1 t2 = match t1 with
    | h::t -> emp t (h::t2)
    | []   -> t2
    in { rgh = []; lft = emp f.rgh f.lft };;
let rec defile f = match f.lft with
| (h::t) -> (h, {rgh = f.rgh; lft = t})
| []     -> defile (move f);;
let file_empty f = f.rgh == [] && f.lft == [];;

let is_biparti g =
    let n = vect_length g in
    let m = make_vect n (-1) in
    let ok = ref true in
    let f = ref empty_file in
    let i = ref 0 in
    while !ok && !i < n do
        if m.(!i) = -1 then begin
            f := enfile !f !i;
            m.(!i) <- 0;
            while !ok && file_empty !f <> true do
                let (nd,f2) = defile !f in
                f := f2;
                let nxts = ref g.(nd) in
                while !ok && !nxts <> [] do
                    let h::t = !nxts in
                    if m.(nd) = m.(h) then ok := false
                    else if m.(h) <> 1 - m.(nd) then begin
                        f := enfile !f h;
                        m.(h) <- 1 - m.(nd);
                    end;
                    nxts := t;
                done;
            done;
        end;
        i := !i + 1
    done;
    !ok;;

let g = [| [4; 5]; [3; 5; 4]; [4]; [1; 4]; [0; 2; 3; 1]; [0; 1] |];;
is_biparti g;;

(* }}} *)

