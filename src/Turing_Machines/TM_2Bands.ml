(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT 
 *
 *   A collection of Two-Bands Turing Machines, reusing some One-Band Turing Machines of TM_Basic.ml
 *
 *)


open Pattern
open Symbol
open Action
open State
open Turing_Machine   


let nop : Turing_Machine.t = Turing_Machine.nop 


let generic_copy: symbols -> Turing_Machine.t = fun symbols ->
  let init = nop.initial and accept = nop.accept in
  let q = State.fresh_from init in	          	  
  let generic_transitions =
    Transition.foreach_symbol_of symbols (BUT B) (fun s ->
	[ (init, Action( Simultaneous [ RWM(Match(VAL s), No_Write, Right) ; RWM(Match ANY, Write s, Right) ]), init) ]
      )
  in 
  { nop with
    nb_bands = 2 ;
    name = "copy_" ^ (Pretty.set Symbol.to_ascii symbols) ;
    transitions = generic_transitions
		  @ [ (init, Action( Simultaneous [ RWM(Match(VAL B), No_Write, Left)  ; RWM(Match ANY, No_Write, Left) ]), q) ;
		      (q, Parallel [ Run(TM_Basic.left_most) ; Run(TM_Basic.left_most) ], accept) 
		    ] 
  }
	

let generic_reverse: symbols -> Turing_Machine.t = fun symbols ->
  let init = nop.initial and accept = nop.accept in
  let copy = State.fresh_from init in
  let head = State.fresh_from copy in
  let generic_transitions =
    Transition.foreach_symbol_of symbols (BUT B) (fun s ->
	[ (copy, Action( Simultaneous [ RWM (Match(VAL s), No_Write, Left) ; RWM (Match ANY, Write s, Right) ]), copy) ]
      )
  in 
  { nop with
    nb_bands = 2 ;
    name = "rev_" ^ (Pretty.set Symbol.to_ascii symbols) ;
    transitions = generic_transitions
		  @ [ (init, Parallel [ Run(TM_Basic.right_most) ; Action(Nop) ], copy) ;			
		      (copy, Parallel [ Action( RWM (Match(VAL B), No_Write, Right)) ; Action( RWM (Match(VAL B), No_Write, Left)) ], head) ;
		      (head, Parallel [ Action(Nop) ; Run(TM_Basic.left_most) ], accept)
		    ] 
  }	

  
let (xor: Turing_Machine.t) = 
  let init = nop.initial and accept = nop.accept in
  let q = State.next_from init in	          	  
  { nop with
    nb_bands = 2 ;
    name = "xor" ;
    transitions =
      [
	(init, Action( Simultaneous [ RWM (Match(VAL Z), Write Z, Right) ; RWM (Match(VAL Z), No_Write, Right) ]), init) ;
	(init, Action( Simultaneous [ RWM (Match(VAL Z), Write U, Right) ; RWM (Match(VAL U), No_Write, Right) ]), init) ;
	(init, Action( Simultaneous [ RWM (Match(VAL U), Write Z, Right) ; RWM (Match(VAL U), No_Write, Right) ]), init) ;
	(init, Action( Simultaneous [ RWM (Match(VAL U), Write U, Right) ; RWM (Match(VAL Z), No_Write, Right) ]), init) ;
	(init, Action( Simultaneous [ RWM (Match(VAL B), No_Write, Left) ; Nop ]), q) ;
	(q, Parallel [ Run(TM_Basic.left_most) ; Run(TM_Basic.left_most) ], accept) 
      ]
  }
  
