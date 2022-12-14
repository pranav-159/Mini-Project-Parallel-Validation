module Ws_deque = Lockfree.Mpmc_relaxed_queue.Not_lockfree;;

let q = Lockfree.Mpmc_relaxed_queue.create ~size_exponent:3 ()

let _ = Ws_deque.push q 100

let () = assert (Ws_deque.pop q = Some 100)
