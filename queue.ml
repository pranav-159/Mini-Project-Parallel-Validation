module Ws_deque = Lockfree.Mpmc_relaxed_queue.Spin;;

let q = Lockfree.Mpmc_relaxed_queue.create ~size_exponent:3 ()

let () = Ws_deque.push q 100

let () = assert (Ws_deque.pop q = 10)
