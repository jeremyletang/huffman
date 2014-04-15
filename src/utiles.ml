(*******************************************************************)
(* Langages de Programmation: IFT 3000 NRC 11775                   *)
(* TP2 HIVER 2014. Date limite: Vendredi 25 avril 2014 à 17h00     *)
(* Enseignant: Mondher Bouden (ift3000-h2014@ift.ulaval.ca)        *)
(* Compresseur de données en utilisant l'arbre de HUFFMAN          *)
(*******************************************************************)

(*******************************************************************) 
(* Fonctions utilitaires pour le compresseur de données            *)
(*******************************************************************)

module Utiles =
struct
               
  (* explode : string -> char list  *)
  let explode = function 
    | "" -> []
    | s -> let rec loop acc = function
	      |	0 -> s.[0] :: acc
	      | x -> loop (s.[x] :: acc) (x - 1)
           in
	     loop [] (String.length s - 1)
 
  (* implode : char list -> string *)
  let rec implode = function
    | [] -> ""
    | x::xs -> (String.make 1 x) ^ (implode xs)  
                                  
  (* listeFreq : 'a list -> ('a * int) list *)
  let rec listeFreq = function  
      | [] -> [] 
      | x::l -> let l1,l2 = List.partition (fun y -> x = y) l in
	        let r = listeFreq l2 in
		  (x,(List.length l1) + 1)::r
	      
  (* tri : ('a * 'a -> bool) -> 'a list -> 'a list *)
  let rec tri p l = 
    let f e l = 
      let (l1,l2) = List.partition (fun x -> p (x,e)) l in
	l1 @ [e] @ l2
    in
      List.fold_left (fun l' x -> f x l') [] l

  (* drop : int -> 'a list -> 'a list *)
  (* élimine les n premiers éléments d'une liste  *) 
  let rec drop n = function 
    | [] -> []
    | (_::r) as l -> if n <= 0 then l else drop (n-1) r

  (* take : int -> 'a list -> 'a list *)
  (* retourne les n premiers éléments d'une liste *) 
  let rec take n = function 
    | [] -> []
    | x::r -> if n <= 0 then [] else x::(take (n-1) r)

  (* pow : float -> int -> float *)
  (* calcule x à la puissance y  *) 
  let pow x n = exp ((float_of_int n) *. log(x)) 

  (* round : float -> int  *)
  (* arrondi la valeur d'un réel vers un entier *) 
  let round x = int_of_float (floor (x +. 0.5))		

  (* lireFichier : string -> char list *)
  (* Retourne la liste des caractères stockés dans un fichier *)

  let lireFichier nom = 
    let rec lireChar file l = 
      try 
	lireChar file (l @ [input_char file])
      with End_of_file -> l
    in
      if Sys.file_exists nom 
      then
	let file = open_in_bin nom in
	let l = lireChar file [] in
	let _ = close_in file in
	  implode l
      else
	failwith "fichier <" ^ nom ^ "> introuvable!"

  (* ecrireFichier : string -> string -> unit *)
  (* Écrit le contenu d'une string dans un fichier *)

  let ecrireFichier nom s =
    let file = open_out_bin nom in 
    let l = explode s in
    let _ = List.map (fun c -> output_char file c) l in
      close_out file

  (* compareFichier : string -> string -> bool *)
  (* Compare 2 fichiers passés en argument *)

  let compFichier nom1 nom2 = 
    (lireFichier nom1) = (lireFichier nom2)
  
  (* nPartition : int -> 'a list -> 'a list list *)
  (* retourne une listes contenant des listes de n éléments. Seule la *)
  (* dernière liste de cette liste peut faire exception à cette règle.*)

(******)
  let rec nPartition (n:int) (l:'a list) = 
	    
end;;
