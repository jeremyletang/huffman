(*******************************************************************)
(* Langages de Programmation: IFT 3000 NRC 11775                   *)
(* TP2 HIVER 2014. Date limite: Vendredi 25 avril 2014 à 17h00     *)
(* Enseignant: Mondher Bouden (ift3000-h2014@ift.ulaval.ca)        *)
(* Compresseur de données en utilisant l'arbre de HUFFMAN          *)
(*******************************************************************)

(*******************************************************************)
(* Signature du compresseur de données                             *)
(* en utilisant l'arbre de HUFFMAN                                 *)
(*******************************************************************)

module type HUFFMAN =
sig

  (* Structure de données permettant de définir un arbre *)

  type arbre = Vide | Feuille of char | Noeud of arbre * arbre
  type bin = U | Z

  (* Interface: classes et méthodes à implanter *)

  class huffman : ?str:string -> unit ->
  object
    val mutable a : arbre

    method creerArbre : (char * int) list -> unit

    method get_arbre : arbre (* A SUPPRIMER *)

    (* method get_list : (arbre * int) list (* A SUPPRIMER *) *)

    (* method fromString : string -> unit *)

    (* method toList : char list *)                                                                 

    (* method toString : string *)                                                                  

    (* method toStruct : string *)                                                                  

    method estVide : bool

    (* method appartient : char -> bool *)                                                          

    (* method cheminFeuille : char -> bin list *)                                                   

    (* method extraireFeuille : bin list -> char *)                                                 

    (* method map : (char -> char) -> unit *)                                                       

    (* method subs : (char * char) list -> unit *)

    (* method coder : string -> bin list *)

    (* method decoder : bin list -> string *)                                                       

    (* method afficherArbre : string -> unit *)                                                     

  end

  class zip: ?str:string -> unit ->
  object

    inherit huffman

    method extraireInfos : char list -> int * char list * char list

    method fromStruct : char list -> char list * int

    method toInt : bin list -> int

    method toBin : int -> int -> bin list

    (* method coderBin : int -> bin list -> char list * int *)

    method decoderBin : int -> char list * int -> bin list

    (* method coderStr : string -> string *)

    (* method decoderStr : string -> string *)

    (* method coderFichier : string -> string -> unit *)

    (* method decoderFichier : string -> string -> unit *)

  end

end;;
