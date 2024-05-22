open Mapgen;;

let random_map = perlin_map 1000 500 1;;
print_float_map (random_map);;

