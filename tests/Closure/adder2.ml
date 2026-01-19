let x =1 in
let rec add_x y = x + y in
let rec apply_to_one u = u 1 in
print_int (apply_to_one add_x) 