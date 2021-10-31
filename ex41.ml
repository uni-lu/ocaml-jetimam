let filterOdd x =
  if x mod 2 = 1 then true else false

let filterEven x =
  if x mod 2 = 0 then true else false

let rec filter f = function
  | [] -> []
  | h::t -> if (f h) then h::(filter f t) else filter f t

let _ = List.iter (Printf.printf "%d ") (filter filterOdd [1;2;3;4;5])
let _ = List.iter (Printf.printf "%d ") (filter filterEven [1;2;3;4;5])