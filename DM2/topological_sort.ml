(* Definition of a general signature for a graph which vertices are integers 0,...,n-1 *)

module type DAG_t = 
  sig
    exception Not_DAG_Error
    type graph
    val size : graph -> int
    val get_neighbours : graph -> int -> int list 
  end 

(* Implementation of topological sort on a module verifying the signature above, via a functor *)

module Topo_sort (X:DAG_t) = 
  struct 
    let topo_sort g = 
      let n = X.size g in 
      let rev_order = Array.make n (-1) in
      let rev_top_counter = ref 0 in 
      (* DFS where the vertices are marked the last time they are seen *)
      let rec dfs to_visit = 
        match to_visit with
        | [] -> ()
        | t::q when rev_order.(t) = (-1) ->
          rev_order.(t) <-(-2); (* To detct cycle *)
          dfs (X.get_neighbours g t);
          dfs q;
          rev_order.(t)<-(!rev_top_counter); (* Marked the last time it is seen *)
          incr rev_top_counter;
        | t::q when rev_order.(t) = (-2) -> 
          raise X.Not_DAG_Error (* If a cycle is detected *)
        | _ -> ()
      in
      (* Do the DFS until all vertices are seen *)
      for i = 0 to (n-1) do
        if rev_order.(i) = -1 then 
          dfs [i]
      done;
      (* Reverse the rev_order to get a topological order *)
      for i = 0 to (n-1) do
        rev_order.(i) <- n-rev_order.(i)-1
      done;
      rev_order
  end 


(* Implementation of a graph with an array of lists *)
module G = struct
  exception Not_DAG_Error
  type graph = int list array
  let size g = Array.length g
  let get_neighbours g v = g.(v)
end

module D = Topo_sort(G)

(* Test0: Graph with a cycle *)
let test0 () =
  let graph = [| [1; 4]; [4; 2]; [3]; [1;4]; [] |] in
  let result = D.topo_sort graph  in
  Printf.printf "Test 0 : \n";
  for i = 0 to (Array.length result) do 
    Printf.printf "%d : %d | " i result.(i);
  done;
  Printf.printf "\n\n"


(* Test1: A simple DAG *)
let test1 () =
  let graph = [| [1; 2]; [3]; [3]; [] |] in
  let result = D.topo_sort graph  in
  Printf.printf "Test 1 : (vertex : top_order) \n";
  for i = 0 to (Array.length result)-1 do 
    Printf.printf "%d : %d | " i result.(i);
  done;
  Printf.printf "\n\n"

(* Test2: A slighlty more complex DAG *)
let test2 () =
  let graph = [| [1; 2]; [3; 4]; [4]; [4; 5]; [5]; [6]; [] |] in
  let result = D.topo_sort graph  in
  Printf.printf "Test 2 : (vertex : top_order) \n";
  for i = 0 to (Array.length result)-1 do 
    Printf.printf "%d : %d | " i result.(i);
  done;
  Printf.printf "\n\n"


(* Test3: A slighlty more complex DAG *)
let test3 () =
  let graph = [| [1; 2]; [3; 4]; []; [5;2]; [6]; []; [7]; [8]; []; [8] |] in
  let result = D.topo_sort graph  in
  Printf.printf "Test 3 : (vertex : top_order) \n";
  for i = 0 to (Array.length result)-1 do 
    Printf.printf "%d : %d | " i result.(i);
  done;
  Printf.printf "\n\n"



let () =
  (*test0()*)
  test1();
  test2();
  test3()