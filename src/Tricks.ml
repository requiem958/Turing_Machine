(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, 2017
 *
 * CONTENT 
 *
 *   Some useful tricks for programming in Ocaml
 *
 * USAGE
 *
 *   Require    :  
 *   Compilation:  ocamlc Tricks.ml
 *   Interpreter:  ledit ocaml Tricks.cmo
 *
 * DEMO
 *
 *   See the examples below
 *)




(* TRICK 1.  (DEPRECATED) apply operators sequentialy.
 *
 *  my notation  >>  Is now available in caml as |>  (use the Ocaml notation)
 *)

let (pipe: 'a -> ('a -> 'b) -> 'b) = fun data f -> f data 

let (>>) = pipe ;;

(* Example

   let _ = 1 >> ((+) 1) >> ((/) 2) >> ((+) 3) ;; 
 *)



   
(* TRICK 2. the function composition but in the sequential order (not the mathematical one)
 * 
 *  --> can also be done using |> 
 *)

let (sequential_composition: ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)) = fun f g ->
      (fun a -> g (f a)) 
      
let (-->) = sequential_composition ;;

(* Example

let _ =  let op = ((+) 1) --> ((/) 2) --> ((+) 3)
         in op 1 ;;

let _ =  let op x = fun x -> x 
                  |> ((+) 1) 
                  |> ((/) 2) 
                  |> ((+) 3)
         in op 1 ;;
*)




(* TRICK 3. Cast to unit *)
    
let (unit: 'a -> unit) = fun _ -> () 

(* Example: 
   let (inc: int -> int) = fun i ->
	 begin
	   print_string (string_of_int i) ;
	   i+1
	 end
   in
     begin
       unit(inc 0) ; 
       inc 1 ;
     end 
*)




(* TRICK 4. Delayed computation *)

type 'a delayed_computation = unit -> 'a
    
let (force: 'a delayed_computation -> 'a) = fun delayed_computation ->
      delayed_computation ()

(* Example 

    See Tracing.ml 
 *)

