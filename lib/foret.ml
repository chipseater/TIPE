open Yojson
type ressource = Nouriture | Main_d_oeuvre | Pierre | Wood | Bed
type donne = (ressource * int) list
type logistics = donne * donne
type position = int * int
type inegalite_brut = PlusBrut | MoinBrut | EquivalentBrut
type percent_ing = MorePercent | LessPercent
type argument = InCity | OutCity
type prio = Random | Pref of biome
type action = argument * batiment * prio
type condition =
  | InegaliteBrut of ressource * ressource * inegalite_brut * int
  | InegaliteEnPourcentage of ressource * ressource * percent_ing * int

type tree = Vide | Node of condition * tree * tree * action


let const_cond t = match Yojson.Safe.Util.to_string(Yojson.Safe.Util.member "type" t) with 
  |"InegaliteBrut" -> InegaliteBrut(Yojson.Safe.Util.to_string(Yojson.Safe.Util.member "ressource1" t),
                                    Yojson.Safe.Util.to_string(Yojson.Safe.Util.member "ressource2" t),
                                    Yojson.Safe.Util.to_string(Yojson.Safe.Util.member "ing" t),
                                    Yojson.Safe.Util.to_int(Yojson.Safe.Util.member "int" t) )

  |"InegaliteEnPourcentage" -> InegaliteEnPourcentage(Yojson.Safe.Util.to_string(Yojson.Safe.Util.member "ressource1" t),
                                                      Yojson.Safe.Util.to_string(Yojson.Safe.Util.member "ressource2" t),
                                                      Yojson.Safe.Util.to_string(Yojson.Safe.Util.member "ing" t),
                                                      Yojson.Safe.Util.to_int(Yojson.Safe.Util.member "int" t) )
let construc t = Node(const_cond (Yojson.Safe.Util.member "condition" t),
                      construc (Yojson.Safe.Util.member "l_child" t),
                      construc (Yojson.Safe.Util.member "r_child" t), 
                      const_act (Yojson.Safe.Util.member "action") t)

let const () =
  let t = Yojson.from_file "tree.json" in 
  let tab = Array.make 100 Vide in 
  let rec parc t c = 
    match Yojson.Safe.Util.to_list t with 
    |e::[] -> tab.(c) <- construc e 
    |e::q -> tab.(c) <- construc e;parc q (c+1)
    |[] -> failwith "no"
  in parc t 0

