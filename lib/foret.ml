open Yojson
open Yojson.Safe.Util
open Village
open Mapgen

(* type ressource = Nouriture | Main_d_oeuvre | Pierre | Wood | Bed
type donne = (ressource * int) list
type logistics = donne * donne
type position = int * int
type inegalite_brut = PlusBrut | MoinBrut | EquivalentBrut
type percent_ing = MorePercent | LessPercent

type biome = Forest | Desert | Plains
type argument = InCity | OutCity
type prio = Random | Pref of biome
type batiment = Maison | Carriere | Scierie | Ferme
type action = argument * batiment * prio

type condition =
  | InegaliteBrut of ressource * ressource * inegalite_brut * int
  | InegaliteEnPourcentage of ressource * ressource * percent_ing * int

type tree = Vide | Node of condition * tree * tree * action *)

let echange_ressource str = match str with
  |"Nouriture" -> Nouriture
  |"Main_d_oeuvre" -> Main_d_oeuvre
  |"Pierre" -> Pierre 
  |"Wood" -> Wood
  |"Bed" -> Bed
  |_ -> failwith "Non Defini1"

let echange_ing_pour str = match str with 
  |"MorePercent" -> MorePercent
  |"LessPercent" -> LessPercent
  |_ -> failwith "Non Defini2"

let echange_ing_brut str = match str with 
  |"PlusBrut" -> PlusBrut
  |"MoinBrut"-> MoinBrut
  |"EquivalentBrut" -> EquivalentBrut
  |_ -> failwith "Non Defini3"

let echange_arg str = match str with 
  |"InCity" -> InCity
  |"OutCity" -> OutCity
  |_ -> failwith "Non Defini4"

let echange_prio str = match str with 
  |"Forest" -> Pref(Forest)
  |"Desert" -> Pref(Desert)
  |"Plains" -> Pref(Plains)
  |"Random" -> Random
  |_ -> failwith "Non Defini5"

let echange_bat str = match str with 
  |"Maison" -> Maison
  |"Carriere" -> Carriere
  |"Scierie" -> Scierie
  |"Ferme" -> Ferme
  |_ -> failwith "Non Defini6"

let const_cond t = match to_string(member "type" t) with 
  |"InegaliteBrute" -> InegaliteBrute((echange_ressource (to_string(member "ressource1" t))),
                                    (echange_ressource (to_string(member "ressource2" t))),
                                    (echange_ing_brut (to_string(member "ing" t))),
                                    to_int(member "int" t)) 

  |"InegaliteEnPourcentage" -> InegaliteEnPourcentage((echange_ressource (to_string(member "ressource1" t))),
                                                      (echange_ressource (to_string(member "ressource2" t))),
                                                      (echange_ing_pour (to_string(member "ing" t))),
                                                      to_int(member "int" t))
  |_ ->failwith "Non Defini7"
;;
let const_act t = ((echange_arg(to_string(member "arg" t))),
                  (echange_bat (to_string(member "bat" t))),
                  (echange_prio(to_string(member "prio" t))))


let rec  construc (t:Safe.t) = Node(const_cond (member "condition" t),
                      construc (member "l_child" t),
                      construc (member "r_child" t), 
                      const_act (member "action" t))

let const () =
  let t = Yojson.Safe.from_file "tree.json" in 
  let tab = Array.make 100 Vide in 
  let rec parc t c = 
    match  t with 
    |e::[] -> tab.(c) <- construc e 
    |e::q ->( (tab.(c) <- construc e);parc q (c+1))
    |[] -> failwith "no"
  in parc (to_list t) 0; 
  tab

