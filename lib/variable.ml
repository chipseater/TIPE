open Type

let z_max = 100.
let taille_troncon = 8
let ratio = 4
let p0 = 1.
let p1 = 0.9

let cout_maison = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -5); (Pierre, -15); (Wood, -20); (Bonheur,0) ]

let cout_carriere = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, -40) ; (Bonheur,0)]

let cout_scierie = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, -20); (Wood, -10) ; (Bonheur,0)]

let cout_ferme = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -10); (Pierre, 0); (Wood, 0) ; (Bonheur,0)]

let cout_statue = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -20); (Pierre, -100); (Wood, 0) ; (Bonheur,0)]

let cout_auberge = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, -100) ; (Bonheur,0)]

let cout_puit = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, -50); (Wood, 0) ; (Bonheur,0)]

let cout_vide = [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ; (Bonheur,0)]


(* Un objet de type donne vide *)
let void_donne : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ; (Bonheur,0)]

(* Les valeurs de production des différents bâtiments *)
let maison_donne_prodution : donne =
  [ (Bed, 5); (Nouriture, 0); (Main_d_oeuvre, -1); (Pierre, 0); (Wood, 0) ; (Bonheur,0)]

let carriere_donne_prodution : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -20); (Pierre, 100); (Wood, 0) ; (Bonheur,0)]

let ferme_donne_prodution : donne =
  [ (Bed, 0); (Nouriture, 10); (Main_d_oeuvre, -25); (Pierre, 0); (Wood, 0) ; (Bonheur,0)]

let scierie_donne_prodution : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -10); (Pierre, 0); (Wood, 50) ; (Bonheur,0)]
let puit_donne : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ; (Bonheur,0)]

let statue_donne : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -10); (Pierre, 0); (Wood, 0) ; (Bonheur,10)]

let auberge_donne : donne =
  [ (Bed, 10); (Nouriture, -20); (Main_d_oeuvre, -10); (Pierre, 0); (Wood, 0) ; (Bonheur,1)]
  
let init_logistique () =
  ( [ (Bed, 5); (Nouriture, 20); (Main_d_oeuvre, 50); (Pierre, 100); (Wood, 100) ; (Bonheur,0) ],
    void_donne )

let logistique_pete () =
  ( [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -1); (Pierre, 0); (Wood, 0) ; (Bonheur,0) ],
    void_donne )



let ferme_modif b = 
  let n = float_of_int b in 
  [|1.;sqrt n;1.;1.;1.;1.|]

let puit_modif b =  let t = ( match b with |0 -> 1. |1 -> 1.75 |2 -> 2.5 |3 -> 1.2 |4 -> 0.5 |_ -> 0. ) in 
  [|1.;t;1.;1.;1.;1.|] 

let void_modif = [|1.;1.;1.;1.;1.;1.|] 

let nb_ress = Array.length ressource_list
let nb_bat = Array.length batiment_list


let nombre_de_tours_par_simulation = 1





