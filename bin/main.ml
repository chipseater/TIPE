open Adn
open Village
open Mapgen

let cond = InegaliteBrute (Wood, Nouriture, MoinsBrut, 11)
let action = (InCity, Maison, Random)

let s1_array = Array.to_list (serialize_tree (Node (cond, Vide, Vide, action)))
let s1 =
  String.concat "" s1_array
  
let tree = construire_arbre s1

let s2 = Array.to_list (serialize_tree tree)

let () = print_string "1)"; print_string (String.concat "-" s1_array); print_string "\n"
let () = print_string "2)"; print_string (String.concat "-"  s2); print_string "\n"
