let rec f x =
  let rec g y =
    let rec h z = y + x - z in
    h in
  g in
print_int (((f 3) 5) 9)
