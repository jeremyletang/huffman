(*******************************************************************)
(* Langages de Programmation: IFT 3000 NRC 11775                   *)
(* TP2 HIVER 2014. Date limite: Vendredi 25 avril 2014 à 17h00     *)
(* Enseignant: Mondher Bouden (ift3000-h2014@ift.ulaval.ca)        *)
(* Compresseur de données en utilisant l'arbre de HUFFMAN          *)
(*******************************************************************)
(* Étudiant(e):                                                    *)
(* NOM: Vogeli                  PRÉNOM: Raphaël                    *)
(* MATRICULE: 111 083 727       PROGRAMME: GLO                     *)
(*                                                                 *)
(*******************************************************************)
(* Étudiant(e):                                                    *)
(* NOM: Letang                  PRÉNOM: Jeremy                     *)
(* MATRICULE: 111 079 414       PROGRAMME: GLO                     *)
(*                                                                 *)
(*******************************************************************)

(* Charger la signature HUFFMAN *)
#use "utiles.ml";;
#use "TP2-SIG-H2014.mli";;

(*******************************************************************)
(* Implantation du compresseur de données                          *)
(* en utilisant l'arbre de HUFFMAN                                 *)
(*******************************************************************)

module Huffman : HUFFMAN =
struct
  open List
  open Printf
  open Sys
  open Utiles;;

  (* Structure de données *)
  type arbre = Vide | Feuille of char | Noeud of arbre * arbre
  type bin = U | Z

  (* Classe huffman: certaines méthodes sont à compléter (voir (******)) *)

  class huffman ?str () = object(this)

    val mutable a = Vide
    (* val mutable feuille_list = [] *)

    (* method get_list = feuille_list *)

    (* method estVide : bool *)
    method estVide = match a with
      | Vide -> true
      | _ -> false

    (* method creerArbre : (char * int) list -> unit *)
    (* La variable d'instance "a" est mis à jour en conséquence *)
    (* La méthode ne prend pas pour aquis que lf est triée *)
    method creerArbre (lf:(char * int) list) =
      let less_than ((_, i1), (_, i2)) = i1 < i2 in
      let rec rec_list_of_feuille lf l = match lf with
        [] -> l
        | (c, i)::tail -> rec_list_of_feuille tail (l@[(Feuille(c), i)])
      in
      let rec rec_build_arbre lf = match lf with
        [] -> a <- Vide
        | (arb, cnt)::tail ->
          match tail with
            [] -> a <- arb
            | (arb_next, cnt_next)::tail2 ->
              rec_build_arbre (tri less_than ([(Noeud(arb, arb_next), (cnt + cnt_next))]@tail2))
      in
      let lfeuille = rec_list_of_feuille lf [] in
      match lfeuille with
      | [(arb, _)] -> a <- arb
      | _ -> rec_build_arbre (tri less_than lfeuille)

    (* method fromString : string -> unit *)
    method fromString (s:string) =
      let rec fromStringAux = function
        | '<'::r ->
          let ag,r1 = fromStringAux' r in
          (match r1 with
            | ','::r2 -> let ad,r3 = fromStringAux' r2 in
              (match r3 with
                | '>'::r4 -> Noeud(ag,ad), r4
                | _ -> failwith "Probleme dans la chaine"
              )
            | _ -> failwith "Probleme dans la chaine"
          )
        |  _ -> failwith "Probleme dans la chaine"
      and fromStringAux' = function
        | '<'::_ as l -> fromStringAux l
        | c::r when (c != ',') && (c != '<') && (c != '>') -> Feuille c, r
        | _ -> failwith "Probleme dans la chaine"
      in
      a <- match (explode s) with
        | [] -> Vide
        | [c] -> Feuille c
        | l ->  ( match fromStringAux l with
          | a',[] -> a'
          | _,_ -> failwith "Probleme dans la chaine"
        )

    (* method toList : char list *)
    method toList =
      let rec rec_to_list arb l = match arb with
        Vide -> l
        | Noeud(Feuille(c), next) ->
          rec_to_list next (l@[c])
        | Noeud(next, Feuille(c)) ->
          (rec_to_list next l)@[c]
        | Feuille(c) -> l@[c]
        | Noeud(next_l, next_r) ->
          let n = rec_to_list next_l l in
          rec_to_list next_r n
      in
      rec_to_list a []

    (* method toString : string *)
    method toString =
      let rec rec_to_string arb str = match arb with
        Vide -> str
        | Noeud(Feuille(c), next) ->
          (rec_to_string next (str ^ Char.escaped '<' ^ Char.escaped c ^ Char.escaped ',')) ^ Char.escaped '>'
        | Noeud(next, Feuille(c)) ->
          (rec_to_string next (str ^ Char.escaped '<')) ^ Char.escaped ',' ^ Char.escaped c ^ Char.escaped '>'
        | Feuille(c) -> str ^ Char.escaped c
        | Noeud(next_l, next_r) ->
          let n_l = rec_to_string next_l (str ^ Char.escaped '<') in
          let n_r = rec_to_string next_r (n_l ^ Char.escaped ',') in
          n_r ^ Char.escaped '>'
      in
      rec_to_string a ""

    (* method toStruct : string *)
    method toStruct =
      let rec rec_to_struct arb str = match arb with
        Vide -> str
        | Noeud(Feuille(c), next) ->
          (rec_to_struct next (str ^ Char.escaped '<' ^ Char.escaped ',')) ^ Char.escaped '>'
        | Noeud(next, Feuille(c)) ->
          (rec_to_struct next (str ^ Char.escaped '<')) ^ Char.escaped ',' ^ Char.escaped '>'
        | Feuille(c) -> str
        | Noeud(next_l, next_r) ->
          let n_l = rec_to_struct next_l (str ^ Char.escaped '<') in
          let n_r = rec_to_struct next_r (n_l ^ Char.escaped ',') in
          n_r ^ Char.escaped '>'
      in
      rec_to_struct a ""

    (* method appartient : char -> bool *)
    method appartient (c:char) =
      let rec rec_appartient c arb = match arb with
        Vide -> false
        | Noeud(Feuille(cur_c), next) -> if cur_c == c then true else rec_appartient c next
        | Noeud(next, Feuille(cur_c)) -> if cur_c == c then true else rec_appartient c next
        | Feuille(cur_c) -> if cur_c == c then true else false
        | Noeud(next_l, next_r) ->
          if (rec_appartient c next_l) == true then true else rec_appartient c next_r
      in
      rec_appartient c a

    (* method cheminFeuille : char -> bin list *)
    method cheminFeuille (c:char) =
      let rec parcoursArbre arb l_bin  = match arb with
      Noeud(Feuille(f),n) ->if f == c then l_bin@[Z] else parcoursArbre n (l_bin@[U])
      | Noeud(n,Feuille(f)) ->if f == c then l_bin@[U] else parcoursArbre n (l_bin@[Z])
      | Feuille(f) -> if f == c then l_bin else []
      | Noeud(next_l, next_r) ->
        (match parcoursArbre next_l (l_bin@[Z]) with
          [] -> parcoursArbre next_r (l_bin@[U])
          | some -> some)
      | Vide -> l_bin
    in
    if this#estVide then failwith "L'arbre est vide"
    else parcoursArbre a []

    (* method extraireFeuille : bin list -> char  *)
     method extraireFeuille (l_bin:bin list) =
     let rec parcoursArbre arb l_bin iter =
        if iter > (List.length l_bin) - 1 then failwith "Chemin binaire mauvais"
        else
         match (List.nth l_bin iter) with
          Z -> (match arb with Noeud(Feuille(f), Feuille(f2)) ->
                  if iter == (List.length l_bin - 1) then f
                  else failwith "Feuille introuvable dans l'arbre"
                |Noeud(Feuille(f), n) ->
                  if iter == (List.length l_bin - 1) then f
                  else failwith "Feuille introuvable dans l'arbre"
                |Noeud(n, n2) -> parcoursArbre n l_bin (iter + 1)
                | _ -> ' ')
          | U -> (match arb with Noeud(Feuille(f), Feuille(f2)) ->
                    if iter == (List.length l_bin - 1) then f2
                    else  failwith "Feuille introuvable dans l'arbre"
                  | Noeud( n, Feuille(f)) ->
                      if iter == (List.length l_bin - 1) then f
                      else failwith "Feuille introuvable dans l'arbre"
                  | Noeud(n1, n) -> parcoursArbre n l_bin (iter + 1)
                  |_ -> ' ' )
     in

    parcoursArbre a l_bin 0

    (* method map : (char -> char) -> unit *)
    (* La variable d'instance "a" est mis à jour en conséquence *)
    method map (f:(char -> char)) =
      let rec rec_map f arb = match arb with
        Vide -> Vide
        | Noeud(Feuille(c), next) ->
          let tmp = rec_map f next in
          Noeud(Feuille(f c), tmp)
        | Noeud(next, Feuille(c)) ->
          let tmp = rec_map f next in
          Noeud(tmp, Feuille(f c))
        | Feuille(c) -> Feuille(f c)
        | Noeud(next_l, next_r) ->
          let n_l = rec_map f next_l in
          let n_r = rec_map f next_r in
          Noeud(n_l, n_r)
      in
      a <- rec_map f a

    (* method subs : (char * char) list -> unit *)
    method subs (l:(char * char) list) =
      this#map (fun c -> try List.assoc c l with Not_found -> c)

    (* method coder : string -> bin list *)
    method coder (s:string) =
      if a = Vide then
      this#creerArbre (listeFreq (explode s));
      List.fold_left (fun res c -> res@(this#cheminFeuille c)) [] (explode s)

    (*method decoder : bin list -> string *)
    method decoder (l_bin:bin list) =
      let rec rec_find_in_tree arb l_bin pos = match arb with
        Feuille(c) -> (pos, c)
        | Noeud(l, r) ->
          if (List.nth l_bin pos) == U then rec_find_in_tree r l_bin (pos + 1)
          else rec_find_in_tree l l_bin (pos + 1)
        | Vide -> (pos, ' ')
      in
      let rec rec_decoder l_bin str pos = match pos with
        i when i == (List.length l_bin) -> str
        | _ ->
          let (new_pos, c) = rec_find_in_tree a l_bin pos in
          rec_decoder l_bin (str ^ Char.escaped c) new_pos
      in
      rec_decoder l_bin "" 0

    (* afficherArbre : arbre -> string -> unit *)
    method afficherArbre (file:string) =
      let print_lvl_transit fd from_lvl to_lvl =
        fprintf fd "%s -> %s\n" from_lvl to_lvl
      in
      let print_normal_line fd lvl c = match c with
        ' ' -> fprintf fd "%s -> %s\n" lvl "ESPACE";
        | _ -> fprintf fd "%s -> %c\n" lvl c;
      in
      let rec rec_afficher_arbre arb lvl fd = match arb with
        Vide -> close_out fd; raise (Err "L'arbre est vide")
        | Noeud(Feuille(l), Feuille(r)) ->
          print_normal_line fd lvl l;
          print_normal_line fd lvl r;
        | Noeud(Feuille(c), next) ->
          print_normal_line fd lvl c;
          print_lvl_transit fd lvl (lvl ^ Char.escaped 'U');
          rec_afficher_arbre next (lvl ^ Char.escaped 'U') fd
        | Noeud(next, Feuille(c)) ->
          print_lvl_transit fd lvl (lvl ^ Char.escaped 'Z');
          rec_afficher_arbre next (lvl ^ Char.escaped 'Z') fd;
          print_normal_line fd lvl c;
        | Feuille(c) -> print_normal_line fd (String.sub lvl 0 ((String.length lvl) - 1)) c
        | Noeud(next_l, next_r) ->
          fprintf fd "%s -> %s\n" lvl (lvl ^ Char.escaped 'Z');
          fprintf fd "%s -> %s\n" lvl (lvl ^ Char.escaped 'U');
          rec_afficher_arbre next_l (lvl ^ Char.escaped 'Z') fd;
          rec_afficher_arbre next_r (lvl ^ Char.escaped 'U') fd;
      in
      let fd = open_out file in
      fprintf fd "digraph G {\n";
      rec_afficher_arbre a "nv0" fd;
      fprintf fd "}\n";
      close_out fd;
      ignore(Sys.command (String.concat " " ["dotty";file]))

    initializer
        match str with
        | None -> ()
        | Some str -> this#creerArbre (listeFreq (explode str))
  end


  (* Classe zip: certaines méthodes sont à compléter (voir (******) *)
  class zip ?str () = object(this)

    inherit huffman ?str ()

    val mutable cpt' = 0

    val nb = 8

    (* method fromStruct : char list -> char list * int *)
    method fromStruct =
      let rec fromStructAux = function
  | '<'::r ->
      let ag,r1 = fromStructAux' r in
        ( match r1 with
    | ','::r2 -> let ad,r3 = fromStructAux'' r2 in
             ( match r3 with
         | '>'::r4 -> Noeud(ag,ad), r4
         | _ -> failwith "Probleme dans la liste"
             )
    | _ -> failwith "Probleme dans la liste"
        )
  | _ -> failwith "Probleme dans la liste"
      and fromStructAux' = function
  | '<'::_ as l -> fromStructAux l
  | (','::_) as l -> cpt' <- cpt' + 1; Feuille (char_of_int cpt'), l
  | _ -> failwith "Probleme dans la liste"
      and fromStructAux'' = function
  | '<'::_ as l -> fromStructAux l
  | ('>'::_) as l -> cpt' <- cpt' + 1; Feuille (char_of_int cpt'), l
  | _ -> failwith "Probleme dans la liste"
      in
  function
    | '<'::'>'::r ->
        let a' = Feuille (char_of_int cpt') in
    a <- a';
    r, 1
    | l ->
        let a', l' = cpt' <- 0; fromStructAux l in
    a <- a';
    l', cpt'

    (* extraireInfos : char list -> int * char list * char list *)
    method extraireInfos = function
      | [] -> a <- Vide; (0,[],[])
      | [_] -> a <- Vide; (0,[],[])
      | l ->
    let rec extraireN l s =
      match l with
        | ('<'::r) as l' -> (s, l')
        | c::r -> extraireN r (s ^ (implode [c]))
        | _ -> failwith "Probleme dans l'extraction des donnees"
    in
    let rec extraireNChar l n l' =
      match l,n with
        | _,0 -> (l',l)
        | c::r,n -> extraireNChar r (n-1) (l'@[c])
        | _,_ -> failwith "Probleme dans l'extraction des donnees"
    in
    let (s1,r1) = extraireN l "" in
    let n1 = try int_of_string s1 with _ -> failwith "Probleme dans l'extraction des donnees" in
    let (r2,n2) = this#fromStruct r1 in
    let (la,r3) = extraireNChar r2 n2 [] in
      (n1,la,r3)

    (* toInt : bin list -> int *)
    method toInt (l_bin:bin list) =
      let (res,_) =
        List.fold_right (fun b (res,r) -> (res + (match b with U -> 1 | Z -> 0) *
             (round (pow 2. r)), r + 1))
           l_bin (0,0)
      in
        res

    (* toBin : int -> int -> bin list *)
    method toBin (i:int) (n:int) =
      let rec fillHd b n' l = if n' = 0 then l else fillHd b (n'-1) (b::l) in
      let rec f n' =
        if n' = 0
        then []
        else
    let n'' = n' / 2 in
    let l = f n'' in
      if (n' mod 2 = 0) then l @ [Z] else l @ [U]
      in
      let l = if n = 0 then [] else f n in
      let i' = i - (List.length l) in
        if i' < 0 then l else fillHd Z i' l

    (* coderBin : int -> bin list -> char list * int *)
    method coderBin (n:int) (l_bin:bin list) =
      match l_bin with
        | [] -> [],0
        | _ ->
    let l = nPartition n l_bin in
      List.map (fun l' -> char_of_int (this#toInt l')) l,
      List.length (List.hd (List.rev l))

    (* decoderBin : int -> char list * int -> bin list *)
    method decoderBin (n':int) ((l_c,n):char list * int) =
      match l_c with
        | [] -> []
        | _ ->
    let l = List.map (fun c -> this#toBin n' (int_of_char c)) l_c in
    let l1 = List.rev (List.tl (List.rev l)) in
    let l2 = List.hd (List.rev l) in
    let l' = l1 @ [ drop ((List.length l2) - n) l2 ] in
      List.concat l'

    (* coderStr : string -> string *)
    method coderStr (s:string) =
      let l_bin = this#coder s in
      let lc,n = this#coderBin nb l_bin in
      let sa = this#toStruct in
      let sa' = List.fold_left (fun acc c -> acc ^ (String.make 1 c)) "" (this#toList) in
  (string_of_int n) ^ sa ^ sa' ^ (implode lc)

    (* decoderStr : string -> string *)
    method decoderStr (s:string) =
      let rec pop_elem_list l nb = match nb with
        0 -> l
        | _ -> pop_elem_list (List.tl l) (nb - 1)
      in
      let rec replace_char arb c_list = match arb with
        Vide -> Vide
        | Feuille(old_c) -> Feuille(List.nth c_list ((int_of_char old_c) - 1))
        | Noeud(Feuille(old_c), next) ->
          let tmp = replace_char next c_list in
          Noeud(Feuille(List.nth c_list ((int_of_char old_c) - 1)), tmp)
        | Noeud(next, Feuille(old_c)) ->
          let tmp = replace_char next c_list in
          Noeud(tmp, Feuille(List.nth c_list ((int_of_char old_c) - 1)))
        | Noeud(next_l, next_r) ->
          let n_l = replace_char next_l c_list in
          let n_r = replace_char next_r c_list in
          Noeud(n_l, n_r)
      in
      let rec to_bin_list l l_bin ext = match l with
        [] -> l_bin
        | [e] -> l_bin@(pop_elem_list e ext)
        | e::tail -> to_bin_list tail (l_bin@e) ext
      in
      let (ext, c_list, code) = this#extraireInfos (explode s) in
      a <- replace_char a c_list;
      let l_l_bin = List.map (fun i -> this#toBin 8 i) (List.map (fun i -> int_of_char i) code) in
      this#decoder (to_bin_list l_l_bin [] (8 - ext))

    (* coderFichier : string -> string -> unit *)
    method coderFichier (inFile:string) (outFile:string) =
      let ratio s1 s2 =
        int_of_float ((1.0 -. ((float_of_int (String.length s1)) /.
        (float_of_int (String.length s2)))) *. 100.0)
      in
      let load_file f =
        let ic = open_in f in
        let n = in_channel_length ic in
        let s = String.create n in
        try
          input ic s 0 n;
          close_in ic;
          s
        with
          e -> close_in ic;
          s
      in
      let write_file file str =
        let fd = open_out file in
        (* fprintf fd "%s" str; *)
        output_string fd str;
        close_out fd;
      in
      let s = load_file inFile in
      this#creerArbre (listeFreq (explode s));
      let encode_s = (this#coderStr s) in
      printf "Ratio de compression: %d\n" (ratio encode_s s);
      write_file outFile encode_s

    (*  decoderFichier : string -> string -> unit *)
    method decoderFichier (inFile:string) (outFile:string) =
      let load_file f =
        let ic = open_in f in
        let n = in_channel_length ic in
        let s = String.create n in
        input ic s 0 n;
        close_in ic;
        s
      in
      let write_file file str =
        let fd = open_out file in
        (* fprintf fd "%s" str; *)
        output_string fd str;
        close_out fd;
      in
      let s = load_file inFile in
      write_file outFile (this#decoderStr s)

  end

end;;
