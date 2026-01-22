let rec build_tupple x y z =
(x,y,z) in
let (x,y,z) = build_tupple 5 3 7 in
let _ = print_int x in
let _ = print_int y in
print_int z