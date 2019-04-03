(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * - calls the demo function of each interesting module
 *
 *)


let execute: (unit -> 't) -> unit = fun f ->  let _ = f () in () ;;

(* DEMO *)

begin
  print_string("\nEXECUTING main.ml") ;

  execute Emulator.demo ;
        


execute UTM.demo ;

(*
execute Demo.demo ;

execute Emulator.demo ;

execute LC_by_MT.demo ;

execute Exercise.demo ;

*)

  print_string("\nEND of main.ml \n") ;
end
