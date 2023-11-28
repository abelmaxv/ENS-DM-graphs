(* Definition of a general signature for a graph which vertices are integers 0,...,n-1 *)

module type Graph_t = 
  sig
    type graph
    val size : graph -> int
    val get_neighbours : graph -> int -> int list 
  end 

(* Implementation of DFS on a module verifying the signature above, via a functor *)

module DFS (X:Graph_t) = 
  struct 
    let dfs g v = 
      let marked = Array.make (X.size g) false in
      (* I first program a reversed dfs to avoid puting elements at the begining of a list *)
      let rec dfs_rev to_visit path = 
        match to_visit with
        | [] -> path
        | t::q when marked.(t) -> 
          dfs_rev q path
        | t::q -> 
          marked.(t) <- true; 
          let path_bis = dfs_rev (X.get_neighbours g t) (t::path) in
          dfs_rev to_visit path_bis
      in

      if (v>= 0 && v < X.size g ) then 
        begin
          marked.(v) <- true;
          (* Returns the reversed list given by dfs_rev *)
          List.rev (dfs_rev (X.get_neighbours g v) [v])
        end
      else
        failwith "The vertex given is not correct"
  end 


(* Implementation of a graph with an array of lists *)
module G = struct
  type graph = int list array
  let size g = Array.length g
  let get_neighbours g v = g.(v)
end

module D = DFS(G)

(* Test0: Unvalid input *)
let test0 () =
  let graph = [| [1; 2]; [0; 3]; [0; 3]; [1; 2] |] in
  let start_vertex = 6 in
  let result = D.dfs graph start_vertex in
  Printf.printf "Test 1 : \n";
  List.iter (Printf.printf "%d") result;
  Printf.printf "\n"

(* Test1: Small correct input *)
let test1 () =
  let graph = [| [1; 2]; [0; 3]; [0; 3]; [1; 2] |] in
  let start_vertex = 0 in
  let result = D.dfs graph start_vertex in
  Printf.printf "Test 1 : \n";
  List.iter (Printf.printf "%d") result;
  Printf.printf "\n"

(* Test 2: graph with disconnected components  *)
let test2 () =
  let graph = [| [1; 2]; [0]; [0]; [3]; [4]; [3] |] in
  let start_vertex = 0 in
  let result = D.dfs graph start_vertex in
  Printf.printf "Test 2 : \n";
  List.iter (Printf.printf "%d") result;
  Printf.printf "\n"

let () =
  (*test0();*) 
  test1();
  test2()