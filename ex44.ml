let stringCast x = "%d " x

let map f l =
  let rec tail f l acc =
    match l with
    | [] -> acc
    | h::t -> tail f t (f h)::acc
  in tail f l []

let _ =
  let l = [1;2;3] in
  List.iter (Printf.printf) "%s " (map stringCast l)