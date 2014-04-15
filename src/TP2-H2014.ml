(*******************************************************************)
(* Langages de Programmation: IFT 3000 NRC 11775                   *)
(* TP2 HIVER 2014. Date limite: Vendredi 25 avril 2014 à 17h00     *)
(* Enseignant: Mondher Bouden (ift3000-h2014@ift.ulaval.ca)        *)
(* Compresseur de données en utilisant l'arbre de HUFFMAN          *)
(*******************************************************************)
(* Étudiant(e):                                                    *)
(* NOM: Vogeli                  PRÉNOM: Vogeli                     *)
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
  open Utiles

  (* Structure de données *)
  type arbre = Vide | Feuille of char | Noeud of arbre * arbre
  type bin = U | Z

  (* Classe huffman: certaines méthodes sont à compléter (voir (******)) *)

  class huffman ?str () = object(this)

    val mutable a = Vide

    (* method estVide : bool *)
    method estVide = match a with
      | Vide -> true
      | _ -> false

(******)
    (*  method creerArbre : (char * int) list -> unit *)
    (* La variable d'instance "a" est mis à jour en conséquence *)
    (* La méthode ne prend pas pour aquis que lf est triée *)
    method creerArbre (lf:(char * int) list) =

    (* method fromString : string -> unit *)
    method fromString (s:string) =
      let rec fromStringAux = function
  | '<'::r ->
      let ag,r1 = fromStringAux' r in
        ( match r1 with
    | ','::r2 -> let ad,r3 = fromStringAux' r2 in
             ( match r3 with
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
(******)
    (* method toList : char list *)
    method toList =

(******)
    (* method toString : string *)
    method toString =

(******)
    (* method toStruct : string *)
    method toStruct =

(******)
    (* method appartient : char -> bool *)
    method appartient (c:char) =

(******)
    (* method cheminFeuille : char -> bin list *)
    method cheminFeuille (c:char) =

(******)
    (* method extraireFeuille : bin list -> char  *)
    method extraireFeuille (l_bin:bin list) =

(******)
    (* method map : (char -> char) -> unit *)
    (* La variable d'instance "a" est mis à jour en conséquence *)
    method map (f:(char -> char)) =

    (* method subs : (char * char) list -> unit *)
    method subs (l:(char * char) list) =
      this#map (fun c -> try List.assoc c l with Not_found -> c)

    (* method coder : string -> bin list *)
    method coder (s:string) =
      if a = Vide
      then
  this#creerArbre (listeFreq (explode s));
      List.fold_left (fun res c -> res@(this#cheminFeuille c)) [] (explode s)

(******)
    (*method decoder : bin list -> string *)
    method decoder (l_bin:bin list) =

(******)
     (* afficherArbre : arbre -> string -> unit *)
     method afficherArbre (file:string) =

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

(******)
    (* decoderStr : string -> string *)
    method decoderStr (s:string) =

(******)
    (* coderFichier : string -> string -> unit *)
    method coderFichier (inFile:string) (outFile:string) =
      let ratio s1 s2 =
  int_of_float ((1.0 -. ((float_of_int (String.length s1)) /.
      (float_of_int (String.length s2)))) *. 100.0)
      in ...

(******)
    (*  decoderFichier : string -> string -> unit *)
    method decoderFichier (inFile:string) (outFile:string) =


  end

end;;
