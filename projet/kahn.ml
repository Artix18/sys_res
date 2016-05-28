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
	
	let put v c conti =
		Queue.push v c;
		conti ()
	
	let rec get c conti =
	   try
          let v = Queue.pop c in
          conti v
       with Queue.Empty ->
          Queue.push (fun () -> get c conti) todo
	
	let doco l = 
		fun conti ->
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
		let r = ref None in
		while (Queue.length todo) <> 0 do
			if (!r) <> None then ()
		done;
		assert(false)
end
