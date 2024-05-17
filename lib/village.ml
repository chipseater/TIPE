
(*                        
                TEMP   
*)
type biome = Forest | Desert | Plains
type building = House | Quarry | Sawmill | Farm 
type tile = (building option) * int
type chunk = ((tile array) array) * biome * int
let chunk_width =4 ;; 
type map = ((chunk array) array)




(* open Mapgen *)



(* 
              Type   
*)
(* Village *)
type ressource = Food | People | Stone | Wood | Bed 
(* Contains a dictionnary with the ressources status *)
type data = ((ressource * int) list)
(* Contains both the needed ressources and the village's stockpiles *)
type logistics = data * data
type position = int * int

(*  Tree *)

type ing = Surplus | Lack 
(*More laiter*)
type verb = Build 

type action = verb * building

type condition = int * ing * ressource

type tree = Vide | Node of condition * tree * tree * action  
(* ID / Decision tree / Ressource table / Center coordonate / Chunk's coordonate list *)
type village = int * tree * logistics * position *  position list 




(* 
          Global Value   
Divide by 10 to have real value
          *)
let void_data:data = [(Bed,0);(Food,0);(People,0);(Stone,0);(Wood,0)];;



          (* Need change *)


let house_data_prodution:data = [  (Bed,50); (Food,0); (People,-1);  (Stone,0);  (Wood,0)];;
let quarry_data_prodution:data = [ (Bed,0);  (Food,0); (People,-20); (Stone,100);(Wood,0)];;
let farm_data_prodution:data = [   (Bed,0);  (Food,10);(People,-25); (Stone,0);  (Wood,0)];;
let sawmill_data_prodution:data = [(Bed,0);  (Food,0); (People,-10); (Stone,0);  (Wood,50)];;
(*  
          Fonction   
*)
(*Combine two data type*)
let rec addition_data (l1:data) (l2:data) = match l1,l2 with
  |((r1,_)::_),((r2,_)::_) when r1 != r2 -> raise (Invalid_argument "Not same ressource's place")
  |e::q,[] |[],e::q -> raise (Invalid_argument("Not same size"))
  |((r1,v1)::q1),((_,v2)::q2) -> (r1,(v1+v2))::(addition_data q1 q2)
  |[],[] -> []

let checkup_tile (tile:tile) = 
  match tile with 
    |None,_ -> void_data
    |Some e,_ -> match e with 
            |House -> house_data_prodution
            |Quarry ->  quarry_data_prodution
            |Farm ->  farm_data_prodution
            |Sawmill -> sawmill_data_prodution

let get_tile (chunk:chunk) = match chunk with
| t,_,_ -> t
;;


(* Chunk parcours *)
let checkup_chunk (chunk:chunk) = 
  let rec parcours_chunk (i:int) j  = match i,j with
    |i ,_ when i=0 -> void_data     
    |i, j when j=0 -> parcours_chunk (i-1) chunk_width 
    |i,j -> addition_data (checkup_tile((get_tile chunk).(i).(j))) (parcours_chunk i (j-1))
in
parcours_chunk chunk_width chunk_width 
;;

let rec chunk_list_parcour (liste:position list) (map:map) = match liste with
  |(i,j) :: q -> addition_data (checkup_chunk map.(i).(j)) ( chunk_list_parcour q map)
  |[] -> void_data





