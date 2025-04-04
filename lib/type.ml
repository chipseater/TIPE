let z_max = 100.
let taille_troncon = 4

type biome = Forest | Desert | Plains
type batiment = Maison | Carriere | Scierie | Ferme

(* A tuile is made out of the eventual batiment it contains associated with its elevation *)
type tuile = Tuile of batiment option * int

(* A troncon is a 4*4 tuile matrix associated with its biome *)
type troncon = Troncon of tuile array array * biome

(* A n*n carte is a n/4*n/4 troncon matrix *)
type carte = troncon array array

type ressource = Nouriture | Main_d_oeuvre | Pierre | Wood | Bed
let ressource_list = [|Nouriture;Main_d_oeuvre;Pierre;Wood;Bed|]
(* Dictionnaire contenant des ressources et leur quantités *)
type donne = (ressource * int) list

(* Contient à la fois les stocks du village et les ressources produites*)
type logistique = donne * donne
type position = int * int

type inegalite_brut = PlusBrut | MoinBrut | EquivalentBrut
type percent_ing = MorePercent | LessPercent

type condition =
  | InegaliteBrute of ressource * ressource * inegalite_brut * int
  | InegaliteEnPourcentage of ressource * ressource * percent_ing * int

(* Un arbre de décision est soit vide, soit constitué d'une condition
   qui décidera si le premier ou le deuxième sous-arbre sera évalué:
   à gauche si la condition est remplie, à droite sinon. Si la condition
   du noeud est vérifié, alors l'action de ce noeud sera exécutée.
*)
type tree = Vide | Node of condition * tree * tree * batiment

(* Le premier batiment  *)
type treepos = Nil | Nodi of (batiment * batiment * int * bool) list * treepos  

(* type village = int * tree * logistique * position * position list *)
type village = {
  id : int;
  tree : tree;
  treepos : treepos;
  mutable logistique : logistique;
  root_position : position;
  mutable position_list : position list;
}
(* A generation binds a carte with the villages that live inside this carte *)
type score = int array
type evaluation = score array
type generation = tree array * treepos array * carte * position array * evaluation
type save = tree array * treepos array * position array * evaluation
type game = save array


(* Un objet de type donne vide *)
let void_donne : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ]

(* Les valeurs de production des différents bâtiments *)
let maison_donne_prodution : donne =
  [ (Bed, 5); (Nouriture, 0); (Main_d_oeuvre, -1); (Pierre, 0); (Wood, 0) ]

let carriere_donne_prodution : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -20); (Pierre, 100); (Wood, 0) ]

let ferme_donne_prodution : donne =
  [ (Bed, 0); (Nouriture, 10); (Main_d_oeuvre, -25); (Pierre, 0); (Wood, 0) ]

let scierie_donne_prodution : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -10); (Pierre, 0); (Wood, 50) ]

let void_donne : donne =
  [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, 0); (Pierre, 0); (Wood, 0) ]
    
let void_modif = [|1.;1.;1.;1.;1.|] 
  
  

let nombre_de_tours_par_simulation = 20
