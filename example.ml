open Printf;;
(* length of adjacency list *)
let n = 1501;;
(*validation time*)
let validation_time = 0.02;;
let adj_list = Array.make n [];;

let rec screener a l=
  match l with
  [] -> []
  | h::t -> if h>a then h::screener a t else screener a t


  (* making a core for test *)
let file = open_in "testfile.txt";;
let quit_loop = ref false in
  while not !quit_loop do
    try
    let line = input_line file in
      let ls = List.map int_of_string (String.split_on_char ' ' line) in 
        let h::t = ls in
        adj_list.(h) <- screener h t
    with End_of_file -> quit_loop := true
  done;;

(* for i = 1 to 6 do 
  printf "%d\n%!" (List.length adj_list.(i))
done ;; *)
(* adj_list.(1) <- [2;3] ;
adj_list.(2) <- [4] ;
adj_list.(3) <- [4;5] ;
adj_list.(4) <- [] ;
adj_list.(5) <- [];
adj_list.(6) <- [] *)


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

(* for i = 1 to (Array.length in_degree - 1) do 
  printf "%d - %d\n%!" i (Atomic.get in_degree.(i))
done;; *)

(* in_degree.(1) <- 0 ;
in_degree.(2) <- 1 ; 
in_degree.(3) <- 1 ; 
in_degree.(4) <- 2 ; 
in_degree.(5) <- 1;; *)

(* let counter = Atomic.make 0;; *)

module T = Domainslib.Task;;


(* let fun_queue =  Queue.empty;; *)


let count = ref 0;;

let minisleep (sec: float) =
  ignore (Unix.select [] [] [] sec)

(* a wrapper around the validate function which is the core part for parallelism *)
let rec execute pool x =
  printf "started %d\n%!" x;
  (* validation *)
  minisleep validation_time;
  count := !count + 1;
  printf "ended %d\n%!" x;
  let ls = adj_list.(x) in
  let rec exec ls =
    match ls with
    | [] -> []
    | h::t -> if (Atomic.fetch_and_add in_degree.(h) (-1)) = 1 then begin
                printf "loaded %d\n%!" h;
              (T.async pool (fun _ -> execute pool h)):: exec t
              end
              else exec t
            in
  let sol = exec ls in
  List.for_all (fun x -> T.await pool x) sol;
          ;;
  (* Atomic.fetch_and_add counter 1;
  Atomic.compare_and_set *)


(* rec part of start function *)
let rec start_rec pool ls =
  match ls with
  | [] -> []
  | h::t -> printf "loaded %d\n%!" h;
            (T.async pool (fun _ -> execute pool h))::(start_rec pool t)
;;

(* starts recursion safely handling exceptions *)
let rec start pool ls =
      let poll = start_rec pool ls in
      List.for_all (fun x -> T.await pool x) poll

(* returns the list of nodes with in_degree 0 in the DAG*)
let rec basis n =
  if n = 0 then []
  else if (Atomic.get in_degree.(n)) = 0 then n::basis (n-1)
  else basis (n-1)

(*returns the main *)
let main () =
  
  let pool = T.setup_pool ~num_domains:7 () in
  let res = T.run pool (fun _ -> start pool (basis 6)) in
  T.teardown_pool pool;
  printf "res - %b" res;;
  (* for i = 1 to (n-1) do
    if in_degree.(i) = 0 then
      T.async pool (fun _ -> execute pool i)
  done *)


let _ = main ()