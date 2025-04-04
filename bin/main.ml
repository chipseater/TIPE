open Mapgen
open Village

(* let _ = game 2 1 *)

(* let mock_chunk2 =
  Troncon
    ( [|
        [| Tuile (Some Ferme, 10); Tuile (Some Ferme, 11); Tuile (Some Ferme, 12); Tuile (Some Ferme, 13) |];
        [| Tuile (Some Ferme, 9); Tuile (Some Ferme, 10); Tuile (Some Ferme, 11); Tuile (Some Ferme, 12) |];
        [| Tuile (Some Ferme, 10); Tuile (Some Ferme, 10); Tuile (Some Ferme, 11); Tuile (Some Ferme, 11) |];
        [| Tuile (Some Ferme, 11); Tuile (Some Ferme, 10); Tuile (Some Ferme, 12); Tuile (None, 10) |];
      |],
      Forest )
;;
let sbatiment batiment =
  match batiment with
  |Bed  -> "Maison"
  |Nouriture  ->  "Boof"
  |Main_d_oeuvre  ->  "Main"
  |Wood  ->  "Wood"
  |Pierre -> "Pierre"
;;
let rec print_do doo = match doo with 
  |[] -> ()
  |(a,b) :: q -> print_string (sbatiment a); print_int b; print_string "\n";print_do q
;;
print_do (sum_troncon_production mock_chunk2) *)
