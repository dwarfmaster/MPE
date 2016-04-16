(* vim:set foldmethod=marker: *)

type terme = V | F
           | Var of string
           | Non of terme
           | Et  of terme*terme
           | Ou  of terme*terme;;

let P = Var "P" and Q = Var "Q" and R = Var("R");;
let prefix +   f g = Ou (f,g);;
let prefix *   f g = Et (f,g);;
let prefix =>  f g = Ou (Non(f),g);;
let prefix <=> f g = Ou (Et(f,g),Et(Non f,Non g));;

let F1 = (P => Q) <=> (Non Q => Non P);;

(* {{{ I - Forme infixe d'une formule logique *)
let rec print_terme t = match t with
| V          -> print_string " V "
| F          -> print_string " F "
| Var s      -> print_string " "; print_string s; print_string " "
| Non t      -> print_string " -"; print_terme t
| Et (t1,t2) -> print_string " (";
                print_terme t1; print_string "+"; print_terme t2;
                print_string ") "
| Ou (t1,t2) -> print_string " (";
                print_terme t1; print_string "*"; print_terme t2;
                print_string ") ";;
print_terme F1;;

let rec drop_spaces ls = match ls with
| []     -> failwith "Unexpected End-Of-String"
| ` `::t -> drop_spaces t
| _      -> ls;;

let rec parse_terme ls = match ls with
| []   -> failwith "Unexpected End-Of-String"
| ` `::t -> parse_terme t
| `(`::t -> let (t1,l1) = parse_terme t in
            let (h::tl) = drop_spaces l1 in
            let (t2,l2) = parse_terme tl in
            let (h2::tl2) = drop_spaces l2 in
            if h2 <> `)` then failwith "Expected ')'"
            else begin match h with
                       | `+` -> (Ou (t1,t2), tl2)
                       | `.` -> (Et (t1,t2), tl2)
                 end
| `-`::t -> let (t1,l1) = parse_terme t in (Non t1,l1)
| `V`::t -> (V, t)
| `F`::t -> (F, t)
| h::t   -> (Var (string_of_char h), t);;
let parse ls = let (t,l) = parse_terme ls in
                 if l <> [] then failwith "Ill-formatted string"
                 else t;;

let list_of_string s =
    let n = string_length s in
    let r = ref [] in
    for i = n-1 downto 0 do
        r := s.[i] :: !r;
    done;
    !r;;
parse (list_of_string "((A + B) . - C)");;

(* }}} *)

(* {{{ II - Satisfiabilité *)
let rec subs v s t = match t with
| Var vr     -> if vr = v then s else Var vr
| Non t1     -> Non (subs v s t1)
| Et (t1,t2) -> Et (subs v s t1, subs v s t2)
| Ou (t1,t2) -> Ou (subs v s t1, subs v s t2)
| _          -> t;;

let rec insert a l = match l with
| []   -> [a]
| h::t -> let n = compare_strings a h in
          if n = 0      then l
          else if n < 0 then a :: l
                        else h :: insert a t;;

let rec enum_r t e = match t with
| Var vr     -> insert vr e
| Non t1     -> enum_r t1 e
| Et (t1,t2) -> let e1 = enum_r t1 e in enum_r t2 e1
| Ou (t1,t2) -> let e1 = enum_r t1 e in enum_r t2 e1
| _          -> e;;
let enum t = enum_r t [];;
enum F1;;

let rec eval t = match t with
| Var _      -> failwith "Can't evaluate with unbound variable"
| Non t1     -> not (eval t1)
| Et (t1,t2) -> (eval t1) && (eval t2)
| Ou (t1,t2) -> (eval t1) || (eval t2)
| V          -> true
| F          -> false;;

let rec sat t =
    let e = enum t in
    if e = [] then (eval t, [])
    else let (h :: tl) = e in
         let (s1,v1) = sat (subs h V t) in
         if s1 then (true, (h,true) :: v1)
         else let (s2,v2) = sat (subs h F t) in
              if s2 then (true, (h,false) :: v2)
              else (false, []);;
let issat t = fst (sat t);;
sat F1;;

let tauto t = not (issat (Non t));;
tauto F1;;

let equiv t1 t2 = tauto (t1 <=> t2);;
equiv (P => Q) (Non Q => Non P);;

(* }}} *)

