open Mapgen

(* Village *)
type ressource = Food | People | Stone | Wood | Bed

(* Contains a dictionnary with the ressources status *)
type data = (ressource * int) list

(* Contains both the needed ressources and the village's stockpiles *)
type logistics = data * data
type id = int

(* Tree *)
type ing = Surplus | Lack
type verb = Build | Overwrite
type action = verb * building
type condition = int * ing * ressource
type tree = Vide | Node of condition * tree * tree * action
type village = id * tree * logistics * (int * int)

(* Computes the diffrence ratio between the stockpiles and the needs *)
let rec get_ratios logistics =
  match logistics with
  | [], _ :: _ | _ :: _, [] -> failwith "2.Lack ressource"
  | (e, _) :: _, (r, _) :: _ when e <> r -> failwith "3.Not the same ressource"
  | [], [] -> []
  | (e, d) :: q, (_, f) :: s -> (e, (d - f) * 100 / d) :: get_ratios (q, s)

(* Evaluates to the amount of the passed ressource that is con/cal *)
let rec search (data : data) ressource =
  match data with
  | [] -> failwith "4.Not Defined"
  | (e, x) :: _ when e = ressource -> x
  | _ :: q -> search q ressource

(* Tests if the passed condition is fullfilled *)
let test difference condition =
  let needed_percent, inequality, ressource = condition in
  (* Retrieves the diffrence between the stocks and the needs *)
  let ressource_diff = search difference ressource in
  let ratio = ressource_diff * 100 / needed_percent in
  match (ratio, inequality) with
  | x, Lack when x > 0 -> false
  | x, Surplus when x < 0 -> false
  | x, _ -> x > needed_percent

let evolution village =
  let _, tree, logistics, _ = village in
  let ratios = get_ratios logistics in
  (* Do action defined by the node, lack of the implementation of the village *)
  let to_do action = () in
  let rec eval tree =
    match tree with
    | Vide -> failwith "1.Invalid Argument"
    | Node (cond, tree_verif, tree_not_verif, act) -> (
        let is_condition_fulfilled = test ratios cond in
        match (tree_verif, tree_not_verif) with
        | Vide, Vide -> to_do act
        | Vide, a -> if is_condition_fulfilled then to_do act else eval a
        | a, Vide -> if is_condition_fulfilled then eval a else to_do act
        | a, b -> if is_condition_fulfilled then eval a else eval b)
  in
  eval tree
