(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT (PROJECT 2019)
 *
 *   Realisation of the Universal Turing Machine as a Multiple-Bands TM
 *
 *)


open Pattern
open Symbol
open Action
open State
open Transition
open Turing_Machine   


   
(*** NOP does no operation *)   
   
let nop: Turing_Machine.t = Turing_Machine.nop 



                          
(** Some turing machines for testing the Universal Turing Machine *)
                         
(*** The "executable" TM that does the negation of each bit of a binary word *)
                          
let neg: Turing_Machine.t =
  let init   = nop.initial
  and accept = nop.accept
  in 	
  { nop with
    name =  "neg" ; 
    transitions = 
      [ (init, Action(RWM(Match(VAL U), Write Z, Right)), init) ;
	(init, Action(RWM(Match(VAL Z), Write U, Right)), init) ; 
        (init, Action(RWM(Match(VAL B), Write B, Here )), accept) 
      ]
  }
  
(**** The "code" of the previous TM written on one band *)
  
let neg_code: Band.t =
  let alphabet: Alphabet.t = Alphabet.utm
  in 
  Band.make "TM_neg"
    [ Std;Z;S ; U ; Z ; R ; Std;Z;S ;
      Std;Z;S ; Z ; U ; R ; Std;Z;S ;
      Std;Z;S ; B ; B ; H ; Acc;U;S ]


  
(*** A TM that increases an little-endian binary integer by one unit: incr([n]_2) = [n+1]_2 *)
                          
let incr: Turing_Machine.t =
  let init = nop.initial and accept = nop.accept in 	
  { nop with
    name =  "incr" ; 
    transitions = 
      [ (init, Action(RWM(Match(VAL U), Write Z, Right)), init) ;
	(init, Action(RWM(Match(VAL Z), Write U, Here )), accept) ; 
        (init, Action(RWM(Match(VAL B), Write U, Here )), accept) 
      ]
  }

(**** The "code" of the previous TM written on one band *)
  
let incr_code: Band.t =
  let alphabet: Alphabet.t = Alphabet.utm
  in 
  Band.make "TM_incr"
    [ (* ... *) ]

  
  
(*** decreases an little-endian binary integer by one unit: incr([n]_2) = [n-1]_2 *)
  
let decr: Turing_Machine.t =
  let init = nop.initial and accept = nop.accept and reject = nop.reject in
  let unit = State.fresh_from init in
  let zero = State.fresh_from unit in
  let back = State.fresh_from zero in
  { nop with
    name = "decr" ;
    transitions = 
      [ (init, Action( RWM (Match(VAL Z), No_Write, Right)), unit) ;
	(init, Action( RWM (Match(VAL U), Write Z , Here )), accept) ;
	(init, Action( RWM (Match(VAL B), No_Write, Here )), reject) ;
	
	(unit , Action( RWM (Match(VAL B), No_Write, Left )), reject) ;
	(unit , Action( RWM (Match(VAL Z), No_Write, Right)), unit) ;
	(unit , Action( RWM (Match(VAL U), Write B , Right)), zero) ;
	
	(zero , Seq [ Action( RWM (Match(VAL B), No_Write, Left)) ; Action( RWM (Match(VAL B), No_Write, Left)) ], back) ;
	(zero , Seq [ Action( RWM (Match(BUT B), No_Write, Left)) ; Action( RWM (Match(VAL B), Write  Z, Left)) ], back) ;
	
	(back , Action( RWM (Match(VAL Z), Write U , Left )), back) ;
	(back , Action( RWM (Match(VAL B), No_Write, Right)), accept) 
      ]
  }

(**** The "code" of the previous TM written on one band *)
  
let decr_code: Band.t =
  let alphabet: Alphabet.t = Alphabet.utm
  in 
  Band.make "TM_decr"
    [ (* ... *) ]

                           

(** THE UNIVERSAL TURING MACHINE *)

(* UTM is a two-bands Turing Machine
 *
 * UTM(m,w) acts as an interpreter:  It runs the code of the Turing Machine m (written on band B1) onto the data w (witten on band B2) 
 *
 *)

  
let utm: Turing_Machine.t = 
  let init = nop.initial
  and accept = nop.accept
  in
  let q = State.fresh_from init in	          	  
  { nop with
    nb_bands = 2 ;
    name = "UTM" ;
    transitions =
      [
	(init, Action( Simultaneous [ RWM (Match(VAL Z), Write Z, Right) ; RWM (Match(VAL Z), No_Write, Right) ]), init) ;
      ]
  }
  
