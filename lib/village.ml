open Mapgen
open Mapmanage

type ressource = Nouriture | Main_d_oeuvre | Pierre | Wood | Bed

(* Dictionnaire contenant des ressources et leur quantités *)
type donne = (ressource * int) list

(* Contient à la fois les stocks du village et les ressources produites*)
type logistics = donne * donne
type position = int * int

(* Arbre *)
(* Une égalité sur des rapports n'ayant pas de sens,
   des types d'inégalité différents sont utilisés
   pour InegaliteEnPourcentage et pour InegaliteBrut *)
type inegalite_brut = PlusBrut | MoinBrut | EquivalentBrut
type percent_ing = MorePercent | LessPercent

(* Action *)
type argument = InCity | OutCity
type prio = Random | Pref of biome
type action = argument * batiment * prio

(* InegaliteEnPourcentage représente une inégalité en pourcentage de stocks tandis que
   InegaliteBrut représente une inégalité en quantité de ressources
   La première ressource sera comparée avec la deuxième d'après
   les constructeurs de ing
*)
type condition =
  | InegaliteBrut of ressource * ressource * inegalite_brut * int
  | InegaliteEnPourcentage of ressource * ressource * percent_ing * int

(* Un arbre de décision est soit vide, soit constitué d'une condition
   qui décidera si le premier ou le deuxième sous-arbre sera évalué:
   à gauche si la condition est remplie, à droite sinon. Si la condition
   du noeud est vérifié, alors l'action de ce noeud sera exécutée.
*)
type tree = Vide | Node of condition * tree * tree * action

(* Un village est caractérisé par son identifiant, son arbre de décision,
   son état de logistique et la liste des troncons qu'il possède.
*)
(* type village = int * tree * logistics * position * position list *)
type village = {
  id : int;
  tree : tree;
  mutable logistics : logistics;
  root_position : position;
  mutable position_list : position list;
}

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

(* Fonction *)
(* Additionne deux dictionnaires de ressources *)
let rec sum_donne (l1 : donne) (l2 : donne) : donne =
  match (l1, l2) with
  | (r1, _) :: _, (r2, _) :: _ when r1 <> r2 ->
      raise (Invalid_argument "Not the same ressource's place")
  | [], [] -> []
  | _, [] | [], _ -> raise (Invalid_argument "Not the same size")
  | (r1, v1) :: q1, (_, v2) :: q2 -> (r1, v1 + v2) :: sum_donne q1 q2

(* Renvoie la production de la tuile d'après le batiment qu'il contient *)
let get_production_from_tuile (tuile : tuile) : donne =
  match get_tuile_batiment tuile with
  | Some Maison -> maison_donne_prodution
  | Some Carriere -> carriere_donne_prodution
  | Some Ferme -> ferme_donne_prodution
  | Some Scierie -> scierie_donne_prodution
  | None -> void_donne

(* Somme la prodution dans un troncon *)
let sum_troncon_production troncon =
  let troncon_production = ref void_donne in
  for i = 0 to taille_troncon - 1 do
    for j = 0 to taille_troncon - 1 do
      let tuile = (get_troncon_tuiles troncon).(i).(j) in
      let tuile_production = get_production_from_tuile tuile in
      troncon_production := sum_donne tuile_production !troncon_production
    done
  done;
  !troncon_production

(* Sums the production of the troncon contained in the list *)
let rec somme_troncon_list_production (troncon_list : position list) (carte : carte) =
  match troncon_list with
  | (i, j) :: q ->
      let production = sum_troncon_production carte.(i).(j) in
      sum_donne production (somme_troncon_list_production q carte)
  | [] -> void_donne

(* Evaluates to the amount of the passed ressource that is con/cal *)
let rec recherche (donne : donne) ressource =
  match donne with
  | [] -> raise (Invalid_argument "Ressource not found in donne dict")
  | (e, x) :: _ when e = ressource -> x
  | _ :: q -> recherche q ressource

(* Inititalisation d'un objet logistique *)
let rec update_logistics (logistics : logistics) : logistics =
  match logistics with
  | [], _ :: _ | _ :: _, [] -> failwith "2.Lack ressource"
  | (e, _) :: _, (r, _) :: _ when e <> r -> failwith "3.Not the same ressource"
  | [], [] -> ([], [])
  | (e, d) :: q, (_, f) :: s ->
      let nouvel_stock, prod = ((e, d + f), (e, 0)) in
      let a, b = update_logistics (q, s) in
      (nouvel_stock :: a, prod :: b)

let calcul_of_main_d_oeuvre (donne)  =
  let nouriture = recherche donne Nouriture in
  let bed = recherche donne Bed in
  let main_d_oeuvre = recherche donne Main_d_oeuvre in
  if main_d_oeuvre > bed*10 then
    begin 
    if bed > nouriture then (
      sum_donne donne
  [ (Bed, -bed); (Nouriture, -nouriture); (Main_d_oeuvre, -main_d_oeuvre + nouriture*10 ); (Pierre, 0); (Wood, 0) ])
  else (
    sum_donne donne
    [ (Bed, -bed); (Nouriture, -bed); (Main_d_oeuvre, -main_d_oeuvre + bed*10 ); (Pierre, 0); (Wood, 0) ])
  end
  else 
    if main_d_oeuvre > nouriture *10 then (
      sum_donne donne
    [ (Bed, -bed); (Nouriture, -nouriture); (Main_d_oeuvre, -main_d_oeuvre + nouriture*10 ); (Pierre, 0); (Wood, 0) ])
  else begin  
    let remaining_nouriture = nouriture - (main_d_oeuvre/10) in 
    let remaining_beds = bed - (main_d_oeuvre/10) in
    let last_gen_main_d_oeuvre = (main_d_oeuvre/10)*10 in 
    if remaining_beds *2 > remaining_nouriture then ( 
      sum_donne donne
    [ (Bed, -bed); (Nouriture, -nouriture + remaining_nouriture mod 2); (Main_d_oeuvre, -main_d_oeuvre + last_gen_main_d_oeuvre + (remaining_nouriture/2)*10 ); (Pierre, 0); (Wood, 0) ])
  else (
    sum_donne donne
    [ (Bed, -bed); (Nouriture, -nouriture + remaining_nouriture - 2 * remaining_beds); (Main_d_oeuvre, -main_d_oeuvre + last_gen_main_d_oeuvre + (2* remaining_beds)*10 ); (Pierre, 0); (Wood, 0) ])
  end


let update_main_d_oeuvre (logistics : logistics) : logistics =
  match logistics with stock, prod -> ((calcul_of_main_d_oeuvre stock : donne), prod)

(* Calcul la nouvelle table de donne *)
let update_all_logistics (logistics : logistics) position_list carte =
  let (a,_) =  logistics in
  let b =  somme_troncon_list_production position_list carte in 
  let nouvel_logistics = update_logistics (a,b) in
  (nouvel_logistics : logistics)

(* Calcule la nouvelle table de donnée en modifiant la carte *)
(* Calcule la logistics à chaque tuile et a chaque fois que la
   resource main d'oeuvre devient négative je change la case en none
   et je recalcule la nouvelle table
*)
let destroy_batiment (logistics : logistics) (position_list : position list)
    (carte : carte) : logistics =
  let stoc, _ = logistics in
  let parcours_troncon (troncon : troncon) (stock : donne) =
    let main_d_oeuvre = ref (recherche stock Main_d_oeuvre) in
    let temp_stock = ref stock in
    for i = 0 to taille_troncon - 1 do
      for j = 0 to taille_troncon - 1 do
        let tuile_donne =
          get_production_from_tuile (get_troncon_tuiles troncon).(i).(j)
        in
        let main_d_oeuvre_need = recherche tuile_donne Main_d_oeuvre in
        if !main_d_oeuvre > -main_d_oeuvre_need then (
          main_d_oeuvre := !main_d_oeuvre - main_d_oeuvre_need;
          temp_stock := sum_donne !temp_stock tuile_donne)
        else modifie_batiment_dans_troncon carte troncon None i j
      done
    done;
    !temp_stock
  in
  let rec parcours_list (l : position list) (stock : donne) =
    match l with
    | [] -> failwith "Invalid Arg d.1"
    | (x, y) :: [] -> parcours_troncon carte.(x).(y) (stock : donne)
    | (x, y) :: q -> parcours_list q (parcours_troncon carte.(x).(y) (stock : donne))
  in
  let nouvel_prod = parcours_list position_list stoc in
  update_all_logistics (stoc, nouvel_prod) position_list carte

let lack_of_main_d_oeuvre (logistics : logistics) (old_logistics : logistics)
    (troncon_list : position list) (carte : carte) =
  let donne, _ = logistics in
  if recherche donne Main_d_oeuvre < 0 then destroy_batiment old_logistics troncon_list carte
  else logistics
