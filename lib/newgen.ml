exception ExitLoop

open Village
open Mapgen
open Random

(* Example tree, will be changed to a randomly generated tree later *)
let tree =
  Node
    ( Equalflat (Wood, Food, 5),
      Vide,
      Node
        ( Ingpercent (People, Stone, More, 2),
          Vide,
          Vide,
          (InCity, Quarry, Random) ),
      (OutCity, House, Pref Plains) )

let stock = [ (Bed, 0); (Food, 0); (People, 0); (Stone, 0); (Wood, 0) ]
let prod = [ (Bed, 0); (Food, 0); (People, 0); (Stone, 0); (Wood, 0) ]
let logistics = (stock, prod)
let village = (1, tree, logistics, (1, 1), [ (1, 1); (1, 2) ])

let random_pos min max =
  let x_min, y_min = min in
  let x_max, y_max = max in
  (int_in_range ~min:x_min ~max:x_max, int_in_range ~min:y_min ~max:y_max)

(* Génère k positions racines de villages parmi une grille en nxn chunks
   divisée en secteurs de taille n / k *)
let gen_village_roots n k =
  let () = self_init () in
  let quadrants_per_side = 1 + (float_of_int k |> Float.sqrt |> int_of_float) in
  let quadrant_width = n / quadrants_per_side in
  (* Désolé *)
  let roots = ref [] in
  let nb_of_roots = ref 0 in
  (* Ajoute à roots une coordonée de racine aléatoire
     pour chaque secteur de map *)
  try
    for i = 0 to quadrants_per_side - 1 do
      for j = 0 to quadrants_per_side - 1 do
        (* x, y sont les coordonées du coin haut-gauche
           du quadrant en cours *)
        let x, y = (i * quadrant_width, j * quadrant_width) in
        if !nb_of_roots < k then (
          roots := random_pos (x, y) (x + quadrant_width, y + quadrant_width) :: !roots;
          nb_of_roots := !nb_of_roots + 1)
        else raise ExitLoop
      done
    done;
    raise ExitLoop
  with
  | ExitLoop -> !roots
  | _ -> !roots
