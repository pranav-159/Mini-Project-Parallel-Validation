open Printf;;
let n = 6;;
let m = 6;;

(* making adjacency list *)
let adj_list = Array.make n [];;

adj_list.(1) <- [2;3] ;
adj_list.(2) <- [4] ;
adj_list.(3) <- [4;5] ;
adj_list.(4) <- [] ;
adj_list.(5) <- [];;

(* function to make in-degree array *)
(* makes an atomic in_degree array from adjacency list*)
let make_in_degree adj deg =
  for i = 1 to (Array.length adj - 1) do 
    let ls = adj.(i) in 
      List.iter (fun x -> Atomic.incr deg.(x)) ls
  done 
    

(* initializing in_degree array*)
let in_degree = Array.make n (Atomic.make 0);;
for i = 1 to (Array.length in_degree -1) do
  in_degree.(i) <- Atomic.make 0
done;;

(* making an in degree array*)  
make_in_degree adj_list in_degree;;

(* in_degree.(1) <- 0 ;
in_degree.(2) <- 1 ; 
in_degree.(3) <- 1 ; 
in_degree.(4) <- 2 ; 
in_degree.(5) <- 1;; *)

(* let counter = Atomic.make 0;; *)

(* module T = Domainslib.Task;;


let pool = T.setup_pool ~num_additional_domains:7 ();; *)

(* let fun_queue =  Queue.empty;; *)

let functions = Lockfree.Mpmc_relaxed_queue.create ~size_exponent:3 ()
;;
module MPMCQueue = Lockfree.Mpmc_relaxed_queue.Not_lockfree;;

let counter = Atomic.make 0;;

let rec execute x =
  printf "started %d\n%!" x;
  Unix.sleep 2;
  let ls = adj_list.(x) in
  List.iter (fun x -> in_degree.(x) <- in_degree.(x)-1; 
  if in_degree.(x) = 0 then ignore (MPMCQueue.push functions x)
   ) 
  ls;
  printf "ended %d\n%!" x;
  Atomic.fetch_and_add counter 1
;;

let  waste = ref 1;;



let thread_loop () =
  while (Atomic.get counter) != (n-1) do 
    let x = MPMCQueue.pop functions in 
      match x with
      | None -> ()
      | Some n -> ignore (execute n)
  done;
  printf "exit %!"


let main () =
  for i = 1 to (n-1) do
    if in_degree.(i) = 0 then ignore (MPMCQueue.push functions i) 
  done;
  print_newline ();
  let p = Array.make m (Domain.spawn (fun () -> thread_loop ()))  in
  for a=2 to m do 
    p.(a-1) <- Domain.spawn (fun () -> thread_loop ()) 
  done;
  for a=1 to m do 
    Domain.join p.(a-1)
  done;
  printf "joined%!";;
let _ = main ()

