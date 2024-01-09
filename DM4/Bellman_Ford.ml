
(* Definition of an extended float type (with infty) *)
type ext_float = Infty | Num of float

(* returns true iif n1 < n2 *)
let comp n1 n2 = 
  match n1, n2 with 
  | _, Infty -> true
  | Infty, _ -> false
  | Num a1, Num a2 -> a1 < a2


let add n1 n2 = 
  match n1, n2 with 
  | Infty, _ | _ , Infty -> Infty
  | Num n1, Num n2 -> Num (n1 +. n2)

(* General signature for a weighted graph which vertices are integers 0,...,n-1 *)

module type W_graph = 
  sig
    exception NegCycleError of string
    type graph
    (*weight(s1 -> s2). Infty if s1 and s2 are disconnected*)
    val weight : graph -> int -> int -> ext_float  
    val size : graph -> int
    val get_neighbours : graph -> int -> int list 
  end 

(* Implementation of Bellman-Ford algorithm on a module verifying the signature above, via a functor *)

module Bellman_ford (X:W_graph) = 
  struct 
    let bellman_ford g s =
  
      (* An auxilary function for the core of the algorithm : update d[x,k]*)
      let improve d y k x = 
        let new_cost = add d.(y).(k-1) (X.weight g y x) in
        if comp new_cost d.(x).(k) then 
          d.(x).(k) <- new_cost
      in 
      (* Initialization *)
      let n = X.size g in 
      let d = Array.make_matrix n (n+2) (Infty) in 
      d.(s).(0) <- Num 0.;
      
      (* Core of the algorithm *)
      for k = 1 to (n+1)  do 
        for x = 0 to (n-1) do 
          d.(x).(k) <- d.(x).(k-1)
        done;
        for y = 0 to (n-1) do
          List.iter (improve d y k ) (X.get_neighbours g y)
        done;
      done;
      (* Check for negative weight cycle *)
      for y = 0 to (n-1) do
        if comp d.(y).(n) d.(y).(n-1) then 
          raise (X.NegCycleError "Negative weight cycle detected")
      done;

      d
  end 


(* Implementation of a graph with an array of lists *)
module G = struct

  exception NegCycleError of string

  type graph = (int*float) list array

  let weight g u v = 
    let res = ref (Infty) in
    let found (w, n) = 
      if w=v then 
        res := Num n
    in
    List.iter found g.(u);
    !res

  let size g = Array.length g

  let get_neighbours g v =
    let fst (n1,n2) = 
      n1 
    in 
    List.map fst (g.(v))
end

module B = Bellman_ford(G)



(* Function to display the results of the Bellman-Ford algorithm *)
let display_results d =
  Array.iteri (fun i row ->
    Printf.printf "Vertex %d: " i;
    Array.iter (fun cost -> match cost with
      | Infty -> Printf.printf "Infty | "
      | Num n -> Printf.printf "%.2f | " n
    ) row;
    print_newline ()
  ) d

(* Test 1: Negative cycle detection *)
let test1 () =
  Printf.printf "Test 1: Negative cycle detection\n";
  try
    let g = [|[(1, -.1.)]; [(3, -.1.)]; [(0, -.1.)]; [(2, -.1.)]|] in
    let d = B.bellman_ford g 0 in
    display_results d
  with
  | G.NegCycleError msg -> Printf.printf "Error: %s\n" msg

(* Test 2: Shortest paths in a weighted graph *)
let test2 () =
  Printf.printf "\nTest 2: Shortest paths in a weighted graph\n";
  try 
    let g = [|[(1, 1.); (2, 2.)]; [(2, 3.)]; [(0, 4.)]|] in
    let d = B.bellman_ford g 0 in
    display_results d
  with
  | G.NegCycleError msg -> Printf.printf "Error: %s\n" msg

  (* Test 3: Shortest paths in a more complicated graph without negative cycle *)
let test3 () =
  Printf.printf "\nTest 3: Shortest paths in a more complicated graph without negative cycle\n";
  try
    let g =
      [|
        [(1, 1.); (2, 2.)];      
        [(3, 2.)];                
        [(1, -.2.); (3, 1.); (4, 3.)]; 
        [(2, 1.); (4, 4.)];       
        [(0, 3.); (2, -.3.)]           
      |] in
    let d = B.bellman_ford g 0 in
    display_results d
  with
  | G.NegCycleError msg -> Printf.printf "Error: %s\n" msg

(* Run the tests *)
let () =
  test1 ();
  test2 ();
  test3 ()






