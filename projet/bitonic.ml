module Bitonic (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let read_tab () =
  	let n = read_int() in
  	let tab = Array.make n 0 in
  	for i = 0 to (n-1) do
  		tab.(i) <- read_int()
  	done;
  	tab
  
  let tab = read_tab ()

	(* todo : paralleliser ici, a priori je crois qu'il faut juste doco *)
  let rec bitonic_sort sens tab = 
  	let n = Array.length tab in
  	if n <= 1 then tab
  	else
  	(
  		let gauche = bitonic_sort true (Array.sub tab 0 (n/2)) in
  		let droite = bitonic_sort false (Array.sub (n/2) (n-1)) in
  		bitonic_merge sens (Array.append gauche droite)
  	)
  
  let bitonic_compare sens tab = 
  	let dist = (Array.length tab) / 2 in
  	for i=0 to (dist-1) do
  		if (tab.(i) > tab.(i+dist)) = sens then
  		(
  			let a = tab.(i) in
  			tab.(i) <- tab.(i+dist);
  			tab.(i+dist) <- a
  		)
  	done
  
  let bitonic_merge sens tab = 
  	let n = Array.length tab in
  	if n = 1 then
  		tab
  	else
  	(
  		bitonic_compare sens tab;
  		let gauche = bitonic_merge sens (Array.sub tab 0 (n/2)) in
  		let droite = bitonic_merge sens (Array.sub (n/2) (n-1)) in
  		Array.append gauche droite
  	)

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
    in
    loop ()

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])

end

module E = Bitonic(Kahn.Th)

let () = E.K.run E.main