(* Nothing to see here, nothing has been yet implemented *)

"""
Map
"""
(* Contains the heat and humiditity values of a biome, aka h and q *)
type value = Low | Medium | High

type biome = value * value

type tile = building * hauteur

type chunk = ((tile array) array ) * biome 

type position = int * int 

"""
Village
"""

type village = id * tree * logistics * position 
  
type ressource = Food | People | Stone

(* Contains a dictionnary with the ressources status *)
type data = ((ressource * int) list)

(* Contains both the needed ressources and the village's stockpiles *)
type logistics = data * data
 
"""
Tree
"""

type building = House | Quarry | Farm 

type ing = Surplus | Lack 

type verb = Build | Overwrite

type action = verb * building

type condition = int * ing * building

type tree = Vide | Node of condition * tree * tree * action 


