let x=1 in
let rec add_x y = x + y in 
let rec add_ing_0 u = u 0 in
print_int (add_ing_0 add_x)