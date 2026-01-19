let rec make_adder x =
  let rec add_x y = x+y in
  add_x in
let tmp_1= 1 in
let ret_1 = make_adder 1 in
let tmp_2= 5 in
let ret_5 = make_adder 5 in
let x = 0 in
print_int ((ret_5 x) + (ret_1 1))