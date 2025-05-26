
exception Bloque
type biome = Forest | Desert | Plains
type batiment = Maison | Carriere | Scierie | Ferme | Puit | Auberge | Statue
let batiment_list = [|Maison;Carriere;Scierie;Ferme;Puit;Auberge;Statue|]
(* A tuile is made out of the eventual batiment it contains associated with its elevation *)
type tuile = Tuile of batiment option * int

(* A troncon is a 4*4 tuile matrix associated with its biome *)
type troncon = Troncon of tuile array array * biome

(* A n*n carte is a n/4*n/4 troncon matrix *)
type carte = troncon array array

type ressource = Nouriture | Main_d_oeuvre | Pierre | Wood | Bed | Bonheur
let ressource_list = [|Nouriture;Main_d_oeuvre;Pierre;Wood;Bed;Bonheur|]
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


