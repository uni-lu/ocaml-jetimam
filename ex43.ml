let reverse l =
  let rec tail l acc =
    match l with
    | [] -> acc
    | h::t -> tail t (h::acc)
  in tail l []

let append l1 l2 =
  let rec loop l1 l2 =
    match l2 with
    | [] -> l1
    | h::t -> loop (h::l1) t
    in loop l1 l2

let _ = List.iter (Printf.printf "%d ") (append (reverse [1;2;3]) [4;5;6])