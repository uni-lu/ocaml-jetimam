let power x n =
  let rec tail x n acc =
    if n = 0 then
      acc
    else
      tail x (n-1) (acc*x)
  in tail x n 1

let _ =
  let x = read_int () in
  let y = read_int () in
  let z = power x y in

  Printf.printf "%d\n" z