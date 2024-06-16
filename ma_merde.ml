(* 
  let stock_exp:data = [(Bed,0);(Food,0);(People,0);(Stone,0);(Wood,0)]
let needed_exp:data = [(Bed,0);  (Food,100);(People,-25); (Stone,0);  (Wood,0)]
let needed_exp_false:data = [(Food,0);  (Food,100);(People,-25); (Stone,0);  (Wood,0)]
let village_exp:village = 1, Vide, (stock_exp, needed_exp), (0, 0), [] 
(* let a = addition_data needed_exp needed_exp_false  *)
let () = assert(addition_data stock_exp needed_exp = [(Bed,0);(Food,100);(People,-25);(Stone,0);(Wood,0)]) 
let mock_chunk = Chunk ( [| [| Tile (None, 10);       Tile (None, 11); Tile (Some House, 12);   Tile (Some Sawmill, 13) |]; 
                            [| Tile (Some House, 9);  Tile (None, 10); Tile (Some Quarry, 11);  Tile (Some Sawmill, 12) |]; 
                            [| Tile (None, 10);       Tile (None, 10); Tile (Some Farm, 11);    Tile (None, 11) |]; 
                            [| Tile (None, 11);       Tile (None, 10); Tile (Some House, 12);   Tile (Some House, 10) |]; |], Forest );;
let mock_chunk2 = Chunk ( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |]; 
                            [| Tile (None, 9);  Tile (None, 10); Tile (None, 11); Tile (None, 12) |]; 
                            [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |]; 
                            [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |]; |], Forest );;
let map = [| [| mock_chunk2;mock_chunk2|];
            [| mock_chunk2;mock_chunk|]|]
let chunk_list = [(0,0);(1,1);(1,1)] 
let chunk_list3 = [(1,1)]
let chunk_list2 = [(0,1)] 
let a = chunk_list_parcour chunk_list map 
let c = chunk_list_parcour chunk_list3 map 
let b = chunk_list_parcour chunk_list2 map 
(* let a = checkup_tile (Tile (Some House, 9) ) *)
(* let b = checkup_tile (Tile (None,9 )) *)
(* let a = checkup_chunk mock_chunk *)


 *)
 type biome = Forest | Desert | Plains
 type building = House | Quarry | Sawmill | Farm 
 
 (* A tile is made out of the eventual building it 
    contains associated with its elevation *)
 type tile = (building option) * int
 
 (* A chunk is a 4*4 tile matrix associated with its biome *)
 type chunk = ((tile array) array) * biome * int
 (* A n*n map is a n/4*n/4 chunk matrix *)
 type map = ((chunk array) array)
 


type ressource = Food | People | Stone | Wood | Bed 
(* Contains a dictionnary with the ressources status *)
type data = ((ressource * int) list)
(* Contains both the needed ressources and the village's stockpiles *)
type logistics = data * data
type position = int * int

(*  Tree *)

type ing = Surplus | Lack 
(*More laiter or to vanish*)
type action = building
type condition = int * ing * ressource
type tree = Vide | Node of condition * tree * tree * action  
(* ID / Decision tree / Ressource table / Center coordonate / Chunk's coordonate list *)
type village = int * tree * logistics * position *  position list 



let cond_a = (0, Lack,Food) 
let cond_b = (10,Surplus,Food)
let cond_c = (50,Surplus,Wood)


let mock_tree = Node(cond_a, Vide,Node( cond_b,Vide,Node(cond_c,Vide,Node(cond_c,Vide,Vide,Sawmill),Quarry),House),Farm )


let stock_exp:data = [(Bed,0);(Food,0);(People,0);(Stone,0);(Wood,0)]

let needed_exp:data = [(Bed,0);  (Food,100);(People,-25); (Stone,0);  (Wood,0)]

;;
let mock_chunk1 = Chunk ( [|[| Tile (None, 10);       Tile (None, 11); Tile (Some House, 12);   Tile (None, 13) |]; 
                            [| Tile (None, 9);        Tile (None, 10); Tile (Some Quarry, 11);  Tile (None, 12) |]; 
                            [| Tile (None, 10);       Tile (None, 10); Tile (Some Farm, 11);    Tile (None, 11) |]; 
                            [| Tile (None, 11);       Tile (None, 10); Tile (Some House, 12);   Tile (None, 10) |]; |], Forest );;

;;
let mock_chunk2 = Chunk ( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |]; 
                            [| Tile (None, 9);  Tile (None, 10); Tile (None, 11); Tile (None, 12) |]; 
                            [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |]; 
                            [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |]; |], Forest );;
;;
let mock_chunk3 = Chunk ( [|[| Tile (None, 10); Tile (Some Farm, 11); Tile (None, 12);        Tile (None, 13) |]; 
                            [| Tile (None, 9);  Tile (Some Farm, 10); Tile (Some House, 11);  Tile (None, 12) |]; 
                            [| Tile (None, 10); Tile (Some Farm, 10); Tile (Some House, 11);  Tile (None, 11) |]; 
                            [| Tile (None, 11); Tile (Some Farm, 10); Tile (None, 12);        Tile (None, 10) |]; |], Forest );;
;;
let map = [|  [| mock_chunk1;mock_chunk2;mock_chunk2|];
              [| mock_chunk2;mock_chunk3;mock_chunk2|];
              [| mock_chunk2;mock_chunk2;mock_chunk2|]|]
;;
let village_exp:village = 1, Vide, (stock_exp, needed_exp), (1, 1), [(1,1);(0,0)] 

let a = destroy_build (stock_exp,needed_exp) ([(1,1);(0,0)]) map 
;;
map 
Hashtbl