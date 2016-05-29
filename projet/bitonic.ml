open Obj

(* hypothèse : l'entrée est de taille n=2^k *)
module Bitonic (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  (*let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2
    *)

  let read_tab () =
  	let n = read_int() in
  	let buffer = Scanf.Scanning.from_string (read_line ()) in
    let tab = Array.init n (fun _ -> Scanf.bscanf buffer "%d " (fun x -> x)) in
  	tab
  
  let tab = read_tab ()
  
  let bitonic_compare sens beg sz = 
  	let dist = sz / 2 in
  	for i=0 to (dist-1) do
  		if (tab.(i+beg) > tab.(i+beg+dist)) = sens then
  		(
  			let a = tab.(i+beg) in
  			tab.(i+beg) <- tab.(i+beg+dist);
  			tab.(i+beg+dist) <- a
  		)
  	done
  
  let rec bitonic_merge sens beg sz = 
  	if sz = 1 then ()
  	else
  	(
  		bitonic_compare sens beg sz;
  		let gauche = bitonic_merge sens beg (sz/2) in
  		let droite = bitonic_merge sens (beg+(sz/2)) (sz-(sz/2)) in
  		(*Array.append gauche droite*)
  		()
  	)

	(* todo : paralleliser ici, a priori je crois qu'il faut juste doco *)
  let rec bitonic_sort sens beg sz = 
  	let n = sz in
  	if n <= 1 then K.doco []
  	else
  	(
  		let gauche_tc : (unit K.process) = Obj.magic (fun () -> bitonic_sort true beg (n/2); ()) in
  		let droite_tc : (unit K.process) = Obj.magic (fun () -> bitonic_sort false (beg+(n/2)) (n-(n/2)); ()) in
  		
  		K.run gauche_tc;
  		K.run droite_tc;
  		(*K.doco [gauche_tc; droite_tc];*)
  		bitonic_merge sens beg sz;
  		K.doco []
  	)

  (*
  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
    in
    loop ()*)
    
  let output () = 
  	let n = Array.length tab in
  	for i = 0 to (n-1) do
  		Printf.printf "%d " (tab.(i))
  	done;
  	Printf.printf "\n";
  	K.doco []

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> bitonic_sort true 0 (Array.length tab); output())

end

module E = Bitonic(Kahn.Th)

let () = E.K.run E.main