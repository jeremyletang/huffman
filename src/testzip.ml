(*******************************************************************)
(* Langages de Programmation: IFT 3000 NRC 11775                   *)
(* TP2 HIVER 2014. Date limite: Vendredi 25 avril 2014 à 17h00     *)
(* Enseignant: Mondher Bouden (ift3000-h2014@ift.ulaval.ca)        *)
(* Compresseur de données en utilisant l'arbre de HUFFMAN          *)
(*******************************************************************)

(*******************************************************************) 
(* Ce fichier permet de faire un test de compression               *)
(*******************************************************************)


#use "TP2-H2014c.ml";;

open Huffman;;
open Utiles;;

let z = new zip ();;

let timeRun f x =
  let	time1 = Sys.time() in
  let r = f x in
  let time2 = Sys.time() in 
    (r,time2 -. time1);;

timeRun (z#coderFichier "testzip.ml") "test.zz";;
(*Ratio de compression: 33%
- : unit * float = ((), 12.474000000000004)*)

timeRun (z#decoderFichier "test.zz") "testzip2.ml";;
(*- : unit * float = ((), 2.1950000000000074)*)





(*

# #use "je.ml";;
*************************  DEBUT TESTS  **************************

- : unit = ()
Chargement du fichier ml du TP
- : unit = ()
module Utiles :
  sig
    val explode : string -> char list
    val implode : char list -> string
    val listeFreq : 'a list -> ('a * int) list
    val tri : ('a * 'a -> bool) -> 'a list -> 'a list
    val drop : int -> 'a list -> 'a list
    val take : int -> 'a list -> 'a list
    val pow : float -> int -> float
    val round : float -> int
    val nPartition : int -> 'a list -> 'a list list
    val lireFichier : string -> string
    val ecrireFichier : string -> string -> unit
    val compFichier : string -> string -> bool
  end
module type HUFFMAN =
  sig
    type arbre = Vide | Feuille of char | Noeud of arbre * arbre
    type bin = U | Z
    class huffman :
      ?str:string ->
      unit ->
      object
        val mutable a : arbre
        method afficherArbre : string -> unit
        method appartient : char -> bool
        method cheminFeuille : char -> bin list
        method coder : string -> bin list
        method creerArbre : (char * int) list -> unit
        method decoder : bin list -> string
        method estVide : bool
        method extraireFeuille : bin list -> char
        method fromString : string -> unit
        method map : (char -> char) -> unit
        method subs : (char * char) list -> unit
        method toList : char list
        method toString : string
        method toStruct : string
      end
    class zip :
      ?str:string ->
      unit ->
      object
        val mutable a : arbre
        method afficherArbre : string -> unit
        method appartient : char -> bool
        method cheminFeuille : char -> bin list
        method coder : string -> bin list
        method coderBin : int -> bin list -> char list * int
        method coderFichier : string -> string -> unit
        method coderStr : string -> string
        method creerArbre : (char * int) list -> unit
        method decoder : bin list -> string
        method decoderBin : int -> char list * int -> bin list
        method decoderFichier : string -> string -> unit
        method decoderStr : string -> string
        method estVide : bool
        method extraireFeuille : bin list -> char
        method extraireInfos : char list -> int * char list * char list
        method fromString : string -> unit
        method fromStruct : char list -> char list * int
        method map : (char -> char) -> unit
        method subs : (char * char) list -> unit
        method toBin : int -> int -> bin list
        method toInt : bin list -> int
        method toList : char list
        method toString : string
        method toStruct : string
      end
  end
module Huffman : HUFFMAN


Test de la fonction nPartition
------------------------------

- : 'a list list = []
- : char list list = []
- : char list list = [['1']; ['2']; ['3']; ['4']]
- : char list list = [['1'; '2']; ['3'; '4']]
- : char list list = [['1'; '2'; '3'; '4']]
- : char list list = [['1'; '2'; '3'; '4']]
- : string list list =
[["a"; "b"; "c"]; ["d"; "e"; "f"]; ["g"; "h"; "i"]; ["j"; "k"]]

Test de la classe huffman
- : unit = ()
-------------------------
- : unit = ()
val s1 : string = "tciaiaiacizaitaicaiaiaictaiaiaicaiaitaiiiaizitci"
val ls1 : (char * int) list =
  [('t', 5); ('c', 6); ('i', 20); ('a', 15); ('z', 2)]
val ls2 : (char * int) list =
  [('z', 2); ('t', 5); ('c', 6); ('a', 15); ('i', 20)]
val a : Huffman.huffman = <obj>
- : char list = ['i'; 'c'; 'z'; 't'; 'a']
- : string = "<i,<<c,<z,t>>,a>>"
- : string = "<,<<,<,>>,>>"
- : bool = true
- : bool = false
val l_bin1 : Huffman.bin list = [U; Z; Z]
val l_bin2 : Huffman.bin list = [U; U]
val l_bin3 : Huffman.bin list = [U; Z; U; U]
- : char = 'c'
- : char = 'a'
- : char = 't'
val l_bin4 : Huffman.bin list = [U; Z; Z; U; U; U; Z; U; U]
- : string = "cat"
- : unit = ()
- : unit = ()
- : string = "<I,<<C,<Z,T>>,A>>"
- : unit = ()
- : string = "<1,<<C,<2,T>>,3>>"
val b : Huffman.huffman = <obj>
- : unit = ()
- : string = "<a,<b,c>>"

Test de la classe zip
- : unit = ()
----------------------

- : unit = ()
val o : Huffman.zip = <obj>
- : int = 157
- : Huffman.bin list = [U; Z; Z; U; U; U; Z; U]
val l : char list = ['\157'; '\001']
val i : int = 1
- : Huffman.bin list = [U; Z; Z; U; U; U; Z; U; U]
- : string = "1<,<<,<,>>,>>iczta\157\001"
- : string = "cat"
- : int * char list * char list =
(1, ['i'; 'c'; 'z'; 't'; 'a'], ['\157'; '\001'])
- : string = "<\001,<<\002,<\003,\004>>,\005>>"
val z : Huffman.zip = <obj>
Ratio de compression: 35%
- : unit = ()
- : unit = ()
- : bool = true
*************************   FIN TESTS   **************************
- : unit = ()
******************************************************************

- : unit = ()
# 
*)
