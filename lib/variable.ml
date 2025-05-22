
open Type
let z_max = 100.
let taille_troncon = 16
let p0 = 1.
let p1 = 0.01

let nombre_survivant = 8

let cout_maison = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -5); (Pierre, -15); (Wood, -20) ]

let cout_carriere = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, -40) ]

let cout_scierie = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, -20); (Wood, -10) ]

let cout_ferme = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -10); (Pierre, 0); (Wood, 0) ]

let cout_statue = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -20); (Pierre, -100); (Wood, 0) ]

let cout_auberge = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, -100) ]

let cout_puit = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, -50); (Wood, 0) ]

let cout_vide = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ]

(* Un objet de type donne vide *)
let void_donne : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ]

(* Les valeurs de production des différents bâtiments *)
let maison_donne_prodution : donne =
  [ (Bed, 5); (Nouriture, 0); (Main_d_oeuvre, -1); (Pierre, 0); (Wood, 0) ]

let carriere_donne_prodution : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -20); (Pierre, 20); (Wood, 0) ]

let ferme_donne_prodution : donne =
  [ (Bed, 0); (Nouriture, 10); (Main_d_oeuvre, -25); (Pierre, 0); (Wood, 0) ]

let scierie_donne_prodution : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -10); (Pierre, 0); (Wood, 50) ]

let puit_donne : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ]

let statue_donne : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -10); (Pierre, 0); (Wood, 0) ]

let auberge_donne : donne =
  [ (Bed, 10); (Nouriture, -20); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ]

let void_donne : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ]

  
let ferme_modif b = 
  let n = float_of_int b in 
  [|1.;sqrt n;1.;1.;1.|]
  

let puit_modif b =  let t = ( match b with |0 -> 1. |1 -> 1.75 |2 -> 2.5 |3 -> 1.2 |4 -> 0.5 |_ -> 0. ) in 
  [|1.;t;1.;1.;1.|] 


let void_modif = [|1.;1.;1.;1.;1.|] 
  
  

let nombre_de_tours_par_simulation = 10





