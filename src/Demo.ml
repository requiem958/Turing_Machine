(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENTS 
 *
 *   Examples of usages of the Turing Machine Interpreter
 *
 *)


open Symbol
open Alphabet
open Band
open Turing_Machine
open Configuration
open Execution


   
(* Running ONE BAND Turing Machines *)
  
let incr_decr: unit -> Configuration.t = fun () ->
  let alphabet = Alphabet.make [B;Z;U;D] in
  let band1 = Band.make "B1" alphabet [U;U;Z;U] in
  let tm = Turing_Machine.sequence [ Run TM_Basic.incr ; Run TM_Basic.left_most ; Run TM_Basic.decr ] in
  let cfg = Configuration.make tm [ band1 ]in
  Execution.i_log_run cfg 


let incr: unit -> Configuration.t = fun () ->
  let alphabet = Alphabet.make [B;Z;U;D] in
  let band1 = Band.make "B1" alphabet [U;U;Z;U] in
  let cfg = Configuration.make TM_Basic.incr [ band1 ] in
  Execution.i_log_run cfg 
	      

let decr1: unit -> Configuration.t = fun () ->
  let alphabet = Alphabet.make [B;U;Z;D] in
  let band1 = Band.make "B1" alphabet [Z;Z;Z;U] in
  let cfg = Configuration.make TM_Basic.decr [ band1 ] in
  Execution.i_log_run cfg 

let decr2: unit -> Configuration.t = fun () ->
  let alphabet = Alphabet.make [B;U;Z;D] in
  let band1 = Band.make "B1" alphabet [Z;Z;Z;U;U] in
  let cfg = Configuration.make TM_Basic.decr [ band1 ] in
  Execution.i_log_run cfg 
	      

let dummy: Symbol.t list -> Configuration.t = fun symbols ->
  let alphabet = Alphabet.binary in
  let band = Band.make "B1" alphabet symbols in
  let cfg = Configuration.make TM_Basic.dummy [ band ] in
  Execution.i_log_run cfg 

  
  
(* Running a Busy Beaver *)

let busy_beaver: Turing_Machine.t -> Configuration.t = fun bb ->
  let alphabet = Alphabet.binary in
  let band = Band.make "B1" alphabet [] in
  let cfg = Configuration.make bb [ band ] in
  Execution.i_log_run cfg 

  

(* Running GENERIC Turing Machines DEPENDING ON the alphabet *)
  
let gen_dup: unit -> Configuration.t = fun () ->
  let alphabet = Alphabet.make [B;Z;U;D] in
  let dup = TM_Basic.generic_dup alphabet.symbols in
  let band1 = Band.make "B1" alphabet [U;Z;Z;U] in
  let cfg = Configuration.make dup [ band1 ] in
  Execution.i_log_run cfg 

(*  
let gen_copy: unit -> Configuration.t = fun () ->
  let alphabet = Alphabet.make [B;U;Z] in
  let copy = TM_Basic.generic_copy alphabet.symbols in
  let band1 = Band.make "B1" alphabet [Z;U;U;Z] 
  and band2 = Band.make "B2" alphabet [] in
  let cfg = Configuration.make copy [ band1 ; band2 ] in
  Execution.i_log_run cfg 

	      
let gen_reverse: unit -> Configuration.t = fun () ->
  let alphabet = Alphabet.make [B;U;Z] in
  let reverse = TM_Basic.generic_reverse alphabet.symbols in	
  let band1 = Band.make "B1" alphabet [U;Z;U;U;Z;Z] 
  and band2 = Band.make "B2" alphabet [] in
  let cfg = Configuration.make reverse [ band1 ; band2 ] in
  Execution.i_log_run cfg 
  

let gen_swap: unit -> Configuration.t = fun () ->
  let alphabet = Alphabet.make [B;U;Z] in
  let swap = TM_Basic.generic_swap alphabet.symbols in	
  let band1 = Band.make "B1" alphabet [U;Z;U;U;Z;Z] in
  let cfg = Configuration.make swap [ band1 ] in
  Execution.i_log_run cfg 
 *)


  

(* Running TWO BANDS Turing Machines *)
            
let xor: unit -> Configuration.t = fun () ->
  let alphabet = Alphabet.make [B;Z;U] in
  let band1 = Band.make "B1" alphabet [U;Z;U;U;Z;Z] 
  and band2 = Band.make "B2" alphabet [U;Z;U;U;Z;Z] in
  let cfg = Configuration.make TM_2Bands.xor [ band1 ; band2 ] in
  Execution.i_log_run cfg

  

(* DEMO *)
	        
let demo: unit -> unit = fun () ->
  begin
    print_string "\n\n* DEMO * Demo.ml:\n\n" ;
    List.iter (fun _ -> ())
      [ incr ()  ;
	decr1 () ;
	decr2 () ;
	incr_decr () ;
	gen_dup () ;
        xor () ;
	busy_beaver TM_Busy_Beaver.bb4 
        (*
	  gen_copy () ;    
	  gen_reverse () ;
	  gen_swap () ;
         *)
      ]
  end
    


(* /!\  DEMO OF NON-TERMINATING MT
 *
 * 1. Run the ocaml interpreter using:  make play 
 * 2. Execute in the ocaml interpreter, then stop execution using Ctrl-C
 *
 *   Demo.dummy [] ;;
 *
 *   Demo.dummy [Bit.unit] ; 
 * 
 *
 * /!\  TERMINATING BUT EXTREMLY LONG COMPUTATIONS ... The sun will be dead before the end of BB6.
 *
 *  Demo.busy_beaver TM_Busy_Beaver.bb5 ;;
 *
 *  Demo.busy_beaver TM_Busy_Beaver.bb6 ;;    
 *)
