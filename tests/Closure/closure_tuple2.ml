let tuple = (5,3,2,7) in
let (x,y,z,w) = tuple in
let rec somme_tuple_a a =
  x+y+z+w+a in
print_int (somme_tuple_a 9)