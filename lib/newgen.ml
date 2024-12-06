open Village
open Mapgen
open Random

(* A generation binds a carte with the villages that live inside this carte *)
type score = int array
type evaluation = score array
type generation = tree array * carte * position array * evaluation
type save = tree array * position array * evaluation
type game = save array

let rnd_ressource () =
  match Random.int 5 with
  | 0 -> Nouriture
  | 1 -> Main_d_oeuvre
  | 2 -> Pierre
  | 3 -> Wood
  | _ -> Bed

let rnd_inegalite_brut () =
  match Random.int 3 with 2 -> PlusBrut | 1 -> MoinBrut | _ -> EquivalentBrut

let rnd_percent_ing () =
  match Random.int 2 with 1 -> MorePercent | _ -> LessPercent

let gen_cond () =
  let ress1, ress2, ing_flat, ing_percent, threshold =
    ( rnd_ressource (),
      rnd_ressource (),
      rnd_inegalite_brut (),
      rnd_percent_ing (),
      Random.int 10 )
  in
  match Random.int 2 with
  | 1 -> InegaliteEnPourcentage (ress1, ress2, ing_percent, threshold)
  | _ -> InegaliteBrut (ress1, ress2, ing_flat, threshold)

let gen_placement () = match Random.int 2 with 1 -> InCity | _ -> OutCity

let gen_prio () =
  match Random.int 2 with
  | 1 -> Pref (Random.int 3 |> int_to_biome)
  | _ -> Random

let gen_batiment () =
  match Random.int 4 with 1 -> Carriere | 2 -> Scierie | 3 -> Ferme | _ -> Maison

let gen_action () = (gen_placement (), gen_batiment (), gen_prio ())

let gen_tree () =
  let rec tree_generator height =
    if height > 0 then
      Node
        ( gen_cond (),
          tree_generator (height - 1),
          tree_generator (height - 1),
          gen_action () )
    else Vide
  in
  Utils.rand_normal 3. 1. |> ceil |> int_of_float |> tree_generator

let gen_trees nb_of_trees = Array.init nb_of_trees (fun _ -> gen_tree ())

let random_pos min max =
  let x_min, y_min = min in
  let x_max, y_max = max in
  (int_in_range ~min:x_min ~max:x_max, int_in_range ~min:y_min ~max:y_max)

(* Génère k positions racines de villages parmi une grille en nxn troncons
   divisée en secteurs de taille n / k *)
let gen_village_roots n k =
  let () = self_init () in
  let quadrants_per_side = float_of_int k |> sqrt |> ceil |> int_of_float in
  let quadrant_width = n / quadrants_per_side in
  let roots = Array.make k (0, 0) in
  (* Ajoute à roots une coordonée de racine aléatoire
     pour chaque secteur de carte *)
  for i = 0 to k - 1 do
    (* x, y sont les coordonées du coin haut-gauche
       du quadrant en cours *)
    let x, y =
      ( i * quadrant_width mod (n - quadrant_width),
        quadrant_width * (quadrant_width * i / n) )
    in
    assert (x + quadrant_width <= n && y + quadrant_width <= n);
    roots.(i) <- random_pos (x, y) (x + quadrant_width, y + quadrant_width)
  done;
  roots
