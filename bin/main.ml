open Adn
open Village
open Mapgen


let cond = InegaliteBrute (Wood, Nouriture, MoinsBrut, 10)
let action = InCity, Maison, Random
let s = serialize_tree (Node (cond, Node (cond, Vide, Vide, action), Vide, action))
let () = print_string s; print_string "\n"