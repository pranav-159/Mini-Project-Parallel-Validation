open Printf;;
let n = 6;;
let m = 2;;

(* making adjacency list *)
let adj_list = Array.make n [];;

adj_list.(1) <- [2;3] ;
adj_list.(2) <- [4] ;
adj_list.(3) <- [4;5] ;
adj_list.(4) <- [] ;
adj_list.(5) <- [];;

(* function to make in-degree array *)
let make_in_degree adj deg =
  for i = 1 to (Array.length adj - 1) do 
    let ls = adj.(i) in 
      List.iter (fun x -> deg.(x) <- deg.(x)+1) ls
  done 
    


(* in degree array *)

let in_degree = Array.make n 0;;

make_in_degree adj_list in_degree;;

for i = 1 to (Array.length in_degree - 1) do 
  printf "%d - %d\n" i in_degree.(i)
done;;

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

let ind = Atomic.make 0;;
let back = ref 0;;

let rec execute x =
  printf "started %d\n" x;
  Unix.sleep 2;
  let ls = adj_list.(x) in
  List.iter (fun x -> in_degree.(x) <- in_degree.(x)-1; 
  if in_degree.(x) = 0 then begin
    functions.(!back) <- x;
    back := !back+1;
  end ) 
  ls;
  printf "ended %d\n" x;;
  (* Atomic.fetch_and_add counter 1;
  Atomic.compare_and_set *)

let  waste = ref 1;;

let lock = Mutex.create ();;
let thread_loop () =
  let ext = ref 0 in
  while Atomic.get ind < (n-1) do 
    Mutex.lock lock;
    while functions.(Atomic.get ind) = 0 do
      waste := !waste+1
    done;
    ext := Atomic.get ind;
    ind := Atomic.get ind+1;
    execute functions.(!ext);
    printf "completed %d - %d\n%!" (Atomic.get ind) functions.(!ext);
    Mutex.unlock lock
  done;
  printf "exit %!"


let main () =
  for i = 1 to (n-1) do
    if in_degree.(i) = 0 then begin
      functions.(!back) <- i;
      (* printf "%d " functions.(!back); *)
      back := !back+1;
    end
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

