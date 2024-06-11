(*open Decision*)

(*              Temp *)
type biome = Forest | Desert | Plains
type building = House | Quarry | Sawmill | Farm 
type tile = (building option) * int
type chunk = ((tile array) array) * biome * int
let chunk_width =4 ;; 
type map = ((chunk array) array)
type ressource = Food | People | Stone | Wood | Bed 
type data = ((ressource * int) list)
type logistics = data * data
type position = int * int
type ing = Surplus | Lack 
type verb = Build 
type action = verb * building
type condition = int * ing * ressource
type tree = Vide | Node of condition * tree * tree * action  
type village = int * tree * logistics * position *  position list 
<<<<<<< HEAD
=
=======

let save  = 
>>>>>>> e3ff7ed (trying to pull the map code)
