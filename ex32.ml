let rec fact n =
	if n = 0 then
		1
	else
		n * fact(n-1)

let _ =
	let x = Scanf.scanf "%d" (fun x -> x) in

	let y = fact x in

	let _ = Printf.printf "%d\n" y in ()