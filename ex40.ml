let rec filterOdd = function
  | [] -> []
  | h::t -> if h mod 2 = 1 then h::filterOdd t else filterOdd t

let _ = List.iter (Printf.printf "%d ") (filterOdd [1;2;3;4;5])