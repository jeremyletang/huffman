(****************************************************************************)
(***********************  JEU D'ESSAI POUR TP2_H2014  ***********************)
(****************************************************************************)
(* Pour exécuter ce jeu d'essai: #use "je.ml";;                             *)
(* Il ne faut pas oublier le '#' avant le use !                             *)
(*                                                                          *)
(* Les réponses valides de ce jeu d'essai se trouvent à la fin du fichier   *)
(****************************************************************************)

print_string  "*************************  DEBUT TESTS  **************************\n\n";;

(* On charge le fichier ml du Tp après avoir implanté
les fonctions demandées pour realiser les tests  *)

print_string "Chargement du fichier ml du TP\n";;

#use "TP2-H2014.ml";;

(* On ouvre les modules disposant de fonctions et méthodes pertinentes pour nos tests *)

open Huffman;;
open Utiles;;

(* On exécute maintenant les fonctions une à une *)

print_string "\n\nTest de la fonction nPartition\n";
print_string "------------------------------\n\n";

nPartition 1 [];;
nPartition 0 ['1';'2';'3';'4'];;
nPartition 1 ['1';'2';'3';'4'];;
nPartition 2 ['1';'2';'3';'4'];;
nPartition 4 ['1';'2';'3';'4'];;
nPartition 5 ['1';'2';'3';'4'];;
nPartition 3 ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k"];;

print_string "\nTest de la classe huffman\n";;
print_string "-------------------------\n";;

let s1 = "tciaiaiacizaitaicaiaiaictaiaiaicaiaitaiiiaizitci";;
let ls1 = listeFreq (explode s1);;
let ls2 = tri (fun ((c,n),(c',n')) -> n <= n') ls1;;

let a = new huffman ~str:s1 ();;
a#toList;;
a#toString;;
a#toStruct;;
a#appartient 'i';;
a#appartient 'm';;
let l_bin1 = a#cheminFeuille 'c';;
let l_bin2 = a#cheminFeuille 'a';;
let l_bin3 = a#cheminFeuille 't';;
a#extraireFeuille l_bin1;;
a#extraireFeuille l_bin2;;
a#extraireFeuille l_bin3;;
let l_bin4 = a#coder "cat";;
a#decoder l_bin4;;
a#afficherArbre "arbre.gv";;

a#map (fun c -> Char.uppercase c);;
a#toString;;
a#subs [('I','1');('Z','2');('A','3')];;
a#toString;;

let b = new huffman ();;
b#fromString "<a,<b,c>>";;
b#toString;;

print_string "\nTest de la classe zip\n";;
print_string "----------------------\n\n";;

let o = new zip ~str:s1 ();;
o#toInt [U ;Z ;Z ;U ;U ;U ;Z ;U];;
o#toBin 8 157;;
let (l,i)= o#coderBin 8 [U ;Z ;Z ;U;U ;U ;Z ;U ;U];;
o#decoderBin 8 (l,i);;
o#coderStr "cat";;
o#decoderStr "1<,<<,<,>>,>>iczta\157\001";;
o#extraireInfos (explode "1<,<<,<,>>,>>iczta\157\001");;
o#toString;;

let s = new zip ();;
s#fromStruct (explode "<<,>,>");;
s#toStruct;;
s#toList;;
s#toString;;

let z = new zip ();;
z#coderFichier "testzip.ml" "test.zz";;
z#decoderFichier "test.zz" "testzip2.ml";;

(* On devrait constater que «testzip2.ml» est identique à testzip.ml» *)
compFichier "testzip.ml" "testzip2.ml";;

print_string "*************************   FIN TESTS   **************************\n";;
print_string "******************************************************************\n\n";;

(********************** RÉPONSES VALIDES ***************************

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
    val lireFichier : string -> string
    val ecrireFichier : string -> string -> unit
    val compFichier : string -> string -> bool
    val nPartition : int -> 'a list -> 'a list list
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
val s : Huffman.zip = <obj>
- : char list * int = ([], 3)
- : string = "<<,>,>"
- : char list = ['\001'; '\002'; '\003']
- : string = "<<\001,\002>,\003>"
val z : Huffman.zip = <obj>
Ratio de compression: 34%
- : unit = ()
- : unit = ()
- : bool = true
*************************   FIN TESTS   **************************
- : unit = ()
******************************************************************

- : unit = ()
#
*)


































