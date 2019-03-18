(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT 
 *
 *   The busy beavers     : https://en.wikipedia.org/wiki/Busy_beaver
 *   Les castors affairés : https://fr.wikipedia.org/wiki/Castor_affairé 
 *
 *)


open Symbol
open Alphabet
open Action
open Pattern
open State
open Turing_Machine   


let nop : Turing_Machine.t = Turing_Machine.nop 

                    
let (bb4: Turing_Machine.t) = 
  let z = Bit.zero
  and u = Bit.unit in
  let init   = nop.initial
  and accept = nop.accept in
  { nop with
    name = "BB4_stops_after_107_steps" ;
    transitions = 
      [ (init, Action( RWM (Match(VAL z), Write u, Right)), Q  2) ;
	(init, Action( RWM (Match(VAL u), Write u, Left )), Q  2) ;
	(Q  2, Action( RWM (Match(VAL z), Write u, Left )), init) ;
	(Q  2, Action( RWM (Match(VAL u), Write z, Left )), Q  3) ;
	(Q  3, Action( RWM (Match(VAL z), Write u, Here )), accept) ;
	(Q  3, Action( RWM (Match(VAL u), Write u, Left )), Q  4) ;
	(Q  4, Action( RWM (Match(VAL z), Write u, Right)), Q  4) ;
	(Q  4, Action( RWM (Match(VAL u), Write z, Right)), init) 
      ] 
  }

  
let (bb5: Turing_Machine.t) = 
  let z = Bit.zero
  and u = Bit.unit in
  let init   = nop.initial
  and accept = nop.accept in	    
  { nop with
    name = "BB5_stops_after_47_176_870_steps" ;
    transitions = 
      [ (init, Action( RWM (Match(VAL z), Write u, Left )), Q 2) ;
	(init, Action( RWM (Match(VAL u), Write u, Right)), Q 3) ;
	(Q  2, Action( RWM (Match(VAL z), Write u, Left )), Q 3) ;
	(Q  2, Action( RWM (Match(VAL u), Write u, Left )), Q 2) ;
	(Q  3, Action( RWM (Match(VAL z), Write u, Left )), Q 4) ;
	(Q  3, Action( RWM (Match(VAL u), Write z, Right)), Q 5) ;
	(Q  4, Action( RWM (Match(VAL z), Write u, Right)), init) ;
	(Q  4, Action( RWM (Match(VAL u), Write u, Right)), Q 4) ;
	(Q  5, Action( RWM (Match(VAL z), Write u, Here )), accept) ;
	(Q  5, Action( RWM (Match(VAL u), Write z, Right)), init) 	    
      ] 
  }
  
  
let (bb6: Turing_Machine.t) = 
  let z = Bit.zero
  and u = Bit.unit in
  let init   = nop.initial
  and accept = nop.accept in	          
  { nop with
    name = "BB6_discovered_in_june_2010_stops_after_3.515_*_10^18267_steps" ;
    transitions = 
      [ (init, Action( RWM (Match(VAL z), Write u, Right)), Q 2) ;
	(init, Action( RWM (Match(VAL u), Write u, Left )), Q 5) ;
	(Q  2, Action( RWM (Match(VAL z), Write u, Right)), Q 3) ;
	(Q  2, Action( RWM (Match(VAL u), Write u, Right)), Q 6) ;
	(Q  3, Action( RWM (Match(VAL z), Write u, Left )), Q 4) ;
	(Q  3, Action( RWM (Match(VAL u), Write z, Right)), Q 2) ;
	(Q  4, Action( RWM (Match(VAL z), Write u, Right)), Q 5) ;
	(Q  4, Action( RWM (Match(VAL u), Write z, Left )), Q 3) ;
	(Q  5, Action( RWM (Match(VAL z), Write u, Left )), init) ;
    	(Q  5, Action( RWM (Match(VAL u), Write z, Right)), Q 4) ;
	(Q  6, Action( RWM (Match(VAL z), Write u, Left )), accept) ;
	(Q  6, Action( RWM (Match(VAL u), Write z, Right)), Q 3) 	    
      ] 
  }

(* BB 7 and beyond are unknown *)
