open Mapgen;;

let random_map = gen_random_values 5;;
print_int_map (random_map);;
print_int (average_adjacent random_map 1 1);;
