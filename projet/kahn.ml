module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port

  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process

  val doco: unit process list -> unit process

  val return: 'a -> 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process

  val run: 'a process -> 'a
end

module Lib (K : S) = struct

  let ( >>= ) x f = K.bind x f

  let delay f x =
    K.bind (K.return ()) (fun () -> K.return (f x))

  let par_map f l =
    let rec build_workers l (ports, workers) =
      match l with
      | [] -> (ports, workers)
      | x :: l ->
          let qi, qo = K.new_channel () in
          build_workers
            l
            (qi :: ports,
             ((delay f x) >>= (fun v -> K.put v qo)) :: workers)
    in
    let ports, workers = build_workers l ([], []) in
    let rec collect l acc qo =
      match l with
      | [] -> K.put acc qo
      | qi :: l -> (K.get qi) >>= (fun v -> collect l (v :: acc) qo)
    in
    let qi, qo = K.new_channel () in
    K.run
      ((K.doco ((collect ports [] qo) :: workers)) >>= (fun _ -> K.get qi))

end


module Th: S = struct
  type 'a process = (unit -> 'a)

  type 'a channel = { q: 'a Queue.t ; m: Mutex.t; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () =
    let q = { q = Queue.create (); m = Mutex.create (); } in
    q, q

  let put v c () =
    Mutex.lock c.m;
    Queue.push v c.q;
    Mutex.unlock c.m;
    Thread.yield ()

  let rec get c () =
    try
      Mutex.lock c.m;
      let v = Queue.pop c.q in
      Mutex.unlock c.m;
      v
    with Queue.Empty ->
      Mutex.unlock c.m;
      Thread.yield ();
      get c ()

  let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

  let return v = (fun () -> v)

  let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()

  let run e = e ()
end

module Proc: S = struct
	type 'a process = (unit -> 'a)
	
	type 'a in_port = in_channel
	type 'a out_port = out_channel
	type 'a channel = 'a in_port * 'a out_port
	
	let new_channel () =
		let a, b = Unix.pipe () in
			Unix.in_channel_of_descr a, Unix.out_channel_of_descr b
	
	let put v c () =
		Marshal.to_channel c v []
	
	let rec get c () =
		Marshal.from_channel c
	
	let doco l () =
		let rec aux pids = function
			| [] -> List.iter (fun pid -> Unix.waitpid [] pid; ()) pids
			| f :: q ->
				match Unix.fork () with
				| 0 -> f ()
				| pid -> aux (pid :: pids) q
		in aux [] l
	
	let return v = (fun () -> v)
	
	let bind e e' () =
		let v = e () in
		e' v ()
	
	let run e = e ()
end

module Seq: S = struct
	type 'a process = ('a -> unit) -> unit
	let todo : (unit -> unit) Queue.t = Queue.create ()
	
	type 'a channel = 'a Queue.t
	type 'a in_port = 'a channel
	type 'a out_port = 'a channel
	
	let new_channel () =
		let q = Queue.create () in
		q,q
	
	let exec_tout () = 
		while not (Queue.is_empty todo) do
			(Queue.pop todo) ()
		done
	
	let put v c conti =
		Queue.push v c;
		conti ()
	
	let rec get c conti =
	   try
          let v = Queue.pop c in
          conti v
       with Queue.Empty ->
          Queue.push (fun () -> get c conti) todo
	
	let doco l conti = 
			let nbRest = ref (List.length l) in (* nbRest est partagé *)
			let aux () =
				decr nbRest;
				if !nbRest = 0 then (*fini*)
					conti ()
			in
			List.iter (fun p -> Queue.push (fun () -> p aux) todo) l
		(*let aux () = () in
		let pasFini = Queue.create () in
		List.iter (fun p -> Queue.push p pasFini) l;
		while (Queue.length pasFini) <> 0 do
			let p = Queue.pop pasFini in
			let tmp = Queue.create () in
			while (Queue.length todo) <> 0 do
				let a = Queue.pop todo in
				Queue.push a tmp
			done;
			while (Queue.length tmp) <> 0 do
				let a = Queue.pop tmp in
				a (aux)
			done;
			while (Queue.length todo) <> 0 do
				let a = Queue.pop todo in
				Queue.push a tmp
			done;
			p (aux);
			while (Queue.length todo) <> 0 do
				let a = Queue.pop todo in
				Queue.push a pasFini
			done;
			while (Queue.length tmp) <> 0 do
				let a = Queue.pop tmp in
				Queue.push a todo
			done;
		done;
		conti () *) (*mauvais type à cause de pasFini mais de toute façon idee plus simple*)
	
	let return v conti = 
		Queue.push (fun () -> conti v) todo
	
	let bind e e' conti =
		Queue.push (fun () -> e (fun x -> e' x conti)) todo
	
	let run e = 
		let r = ref None in (*il faut juste que e soit capable de modif r*)
		let modif_rval = (fun x -> r := Some x) in
		let proc () = e modif_rval in
		Queue.push proc todo;
		exec_tout ();
		match !r with
			| Some x -> x
end

module Socket: S = struct
	type 'a process = (unit -> 'a)

	type 'a in_port  = in_channel
	type 'a out_port = out_channel
	type 'a channel  = ('a in_port * 'a out_port)

	let return v = (fun () -> v)

	let get_my_addr () =
		(Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)

	let establish_server sockaddr =
		let domain  = Unix.domain_of_sockaddr sockaddr in
		let sock    = Unix.socket domain Unix.SOCK_STREAM 0 in
		let in_sock = Unix.socket domain Unix.SOCK_STREAM 0 in
			Unix.bind sock sockaddr;
			Unix.listen sock 1;
			Unix.connect in_sock sockaddr;
			let (out_sock, _) = Unix.accept sock in
				let in_chan  = Unix.in_channel_of_descr in_sock
				and out_chan = Unix.out_channel_of_descr out_sock in
					(in_chan, out_chan)
				
	let new_channel () =
		let my_addr = get_my_addr () in
		let rec aux port = 
			try
				let sockaddr = (Unix.ADDR_INET (my_addr, port))
				in establish_server sockaddr
			with _ ->
				aux (port + 1)
			in aux 541073
	
	let put v c () =
		Marshal.to_channel c v []

	let get c () =
		Marshal.from_channel c

	let doco l () = 
		let pids = List.map (fun f -> match Unix.fork () with | 0 -> f () | pid -> pid) l in
		List.iter (fun pid -> Unix.waitpid [] pid; ()) pids

	let bind e e' () = 
		let v = e () in 
			e' v ()

	let run e = e ()
end

module Server: S = struct
	type 'a process = (unit -> 'a)

	type 'a in_port  = in_channel
	type 'a out_port = out_channel
	type 'a channel  = ('a in_port * 'a out_port)
	let clients = ref channel list

	let return v = (fun () -> v)

	let new_channel () =
		match !clients with 
			| []   -> raise "Not enough client"
			| a::l -> begin clients := l; a end
	
	let put v c () =
		Marshal.to_channel c v []

	let get c () =
		Marshal.from_channel c

	let doco l () = 
		let rec dispatch l =
			match l with
				| [a] | [] -> ()
				| (_, cout)::(cin, cout_)::l -> put (get (cout)) cout (); dispatch ((cin, cout_)::l)
			in
		let n = List.length l in
		
		let mesClients =
		List.fold_left (fun lis p -> let chan = new_channel() in Marshal.to_channel (snd chan) p [Marshal.Closures]; chan::lis) [] l in
		
		let nbMorts = ref 0 in 
		while (!nbMorts <> n) do
			nbMorts := 0;
			dispatch mesClients;
		done


	let bind e e' () = 
		let v = e () in 
			e' v ()

	let run e = e ()

	let get_my_addr () =
		(Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)

	let init_server_for_client_collecting () =
		let my_addr = get_my_addr () in
		let rec aux port = 
			try
				let sockaddr = (Unix.ADDR_INET (my_addr, port)) in
				let domain  = Unix.domain_of_sockaddr sockaddr in
				let sock    = Unix.socket domain Unix.SOCK_STREAM 0 in
					Unix.bind sock sockaddr;
					Unix.listen sock 3;
					sock 
			with _ ->
				aux (port + 1)
			in aux 84742

	let collect_clients () =
		let ic, oc = new_channel () in
			let sock = init_server_for_client_collecting () in
				let rec aux i =
					if i < 10 then
						begin
						let (s, _) = Unix.accept sock in
							let in_chan  = Unix.in_channel_of_descr s
							and out_chan = Unix.out_channel_of_descr s in
								(in_chan, out_chan) :: aux (i + 1)
						end
					else [] in
		aux 0

	let init_server () = clients := collect_clients (); ()
		
end

module Client: S = struct
	type 'a process = (unit -> 'a)

	type 'a in_port  = in_channel
	type 'a out_port = out_channel
	type 'a channel  = ('a in_port * 'a out_port)

	let return v = (fun () -> v)

	let get_my_addr () =
		(Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)

	let establish_server sockaddr =
		let domain  = Unix.domain_of_sockaddr sockaddr in
		let sock    = Unix.socket domain Unix.SOCK_STREAM 0 in
		let in_sock = Unix.socket domain Unix.SOCK_STREAM 0 in
			Unix.bind sock sockaddr;
			Unix.listen sock 1;
			Unix.connect in_sock sockaddr;
			let (out_sock, _) = Unix.accept sock in
				let in_chan  = Unix.in_channel_of_descr in_sock
				and out_chan = Unix.out_channel_of_descr out_sock in
					(in_chan, out_chan)
				
	let new_channel () =
		let my_addr = get_my_addr () in
		let rec aux port = 
			try
				let sockaddr = (Unix.ADDR_INET (my_addr, port))
				in establish_server sockaddr
			with _ ->
				aux (port + 1)
			in aux 541073
	
	let put v c () =
		Marshal.to_channel c v []

	let get c () =
		Marshal.from_channel c

	let doco l () = 
		let pids = List.map (fun f -> match Unix.fork () with | 0 -> f () | pid -> pid) l in
		List.iter (fun pid -> Unix.waitpid [] pid; ()) pids

	let bind e e' () = 
		let v = e () in 
			e' v ()

	let run e = e ()

	let open_connection sockaddr =
	   let domain = Unix.domain_of_sockaddr sockaddr in
	   let sock = Unix.socket domain Unix.SOCK_STREAM 0 
	   in try Unix.connect sock sockaddr ;
			  (Unix.in_channel_of_descr sock , Unix.out_channel_of_descr sock)
		 with exn -> Unix.close sock ; raise exn

	let shutdown_connection inchan =
		Unix.shutdown (Unix.descr_of_in_channel inchan) Unix.SHUTDOWN_SEND

	let wait_server_instruction () = 
		let server = "IP IPI PI PIPI" in
		let port = 541073 in
		let server_addr = (
			try Unix.inet_addr_of_string server
			with Failure ("inet_addr_of_string") ->
				try (Unix.gethostbyname server).Unix.h_addr_list.(0)
				with Not_found ->
					Printf.eprintf ("%s : Unknown server\n", server);
					exit -1
		   )
			in 
			(
			try (
				let sockaddr = (Unix.ADDR_INET (server_addr, port)) in
				let domain  = Unix.domain_of_sockaddr sockaddr in
				let sock    = Unix.socket domain Unix.SOCK_STREAM 0 in
				let in_chan, out_chan = open_connection sockaddr in

					let wait_my_function () =
						let fu = Marshal.from_channel in_chan in	
							fu in_chan out_chan;
							shutdown_connection in_chan
					in
					wait_my_function ();
				)
			with Failure ("int_of_string") -> Printf.eprintf "bad port number";
			                                  exit -1
			                                  )
end

