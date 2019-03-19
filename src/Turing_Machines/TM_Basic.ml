(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENTS 
 *
 *   A collection of Standard (One-Band) Basic Turing Machines  
 *   - some TM are regardless of the alphabet
 *   - some TM depend on the alphabet, their description is given as *generic transitions* that must be instantiated with the alphabet
 *)

open Symbol
open Alphabet
open Pattern
open Action
open State
open Turing_Machine   

   
let nop: Turing_Machine.t = Turing_Machine.nop    
   
let move: Moving.t -> Turing_Machine.t = fun direction ->
  let init   = nop.initial
  and accept = nop.accept
  in 
  { nop with
    name = String.concat "_" [ "move" ; Moving.to_ascii direction ] ;
    transitions = [ (init, Action(RWM(Match(ANY), No_Write, direction)), accept) ]
  }



let find_symbol_on_the: Symbol.t -> Moving.t -> Turing_Machine.t = fun symbol direction ->
  let
    tm_name = String.concat "_" [ Symbol.verbatim symbol ; "on_the" ; Moving.to_ascii direction ] 
  in
  let init   = nop.initial
  and accept = nop.accept
  in
  Turing_Machine.naming tm_name
    { nop with
      transitions =
 	[ (init, Action(RWM(Match(BUT symbol), No_Write, direction)), init) ;
	  (init, Action(RWM(Match(VAL symbol), No_Write, Here)), accept)
	]
    }
	      

let first_blank_on_the: Moving.t -> Turing_Machine.t = find_symbol_on_the B 
      

let second_blank_on_the: Moving.t -> Turing_Machine.t = fun direction ->
  let init   = nop.initial
  and accept = nop.accept
  in
  { nop with
    name = String.concat "_" [ "second_blank_on_the" ; Moving.to_ascii direction ] ;
    transitions = [ (init, Seq [ Run (first_blank_on_the direction) ; Action(RWM(Match(ANY),No_Write, direction)) ; Run (first_blank_on_the direction) ], accept) ] 
  }
  

let goto_right_blank: Turing_Machine.t = find_symbol_on_the B Right


let goto_right_dollar: Turing_Machine.t = find_symbol_on_the D Right 
    

let most_on_the: Moving.t -> Turing_Machine.t = fun dir ->
  let rid =
    (match dir with
     | Left -> Right
     | Right -> Left
    )
  in
  let init = nop.initial and accept = nop.accept in
  let loop = State.next_from init in	      
  { nop with
    transitions = [ (init, Action( RWM (Match(ANY), No_Write, dir)), loop) ;
		    (loop, Action(RWM(Match(BUT B), No_Write, dir)), loop) ;
		    (loop, Action(RWM(Match(VAL B), No_Write, rid)), accept)			    
		  ]
  }
  

let right_most: Turing_Machine.t = Turing_Machine.naming ">?B" (most_on_the Right)


let left_most: Turing_Machine.t = Turing_Machine.naming "B?<" (most_on_the Left)


let erase: Turing_Machine.t = 
  let init = nop.initial and accept = nop.accept in
  let loop = State.next_from init in	
  { nop with
    name = "erase" ;
    transitions =
      [ (init, Run right_most, loop) ;
	(loop, Action( RWM (Match(BUT B), Write B,  Left)), loop) ;
	(loop, Action( RWM (Match(VAL B), No_Write, Here)), accept)
      ]
  }
  

let erase_backward: Turing_Machine.t = 
  let init = nop.initial and accept = nop.accept in
  let loop = State.next_from init in	
  { nop with
    name = "<erase" ;
    transitions =
      [ (init, Action( RWM (Match(ANY)  , Write B , Left )), loop) ;
	(loop, Action( RWM (Match(BUT B), Write B , Left )), loop) ;
	(loop, Action( RWM (Match(VAL B), No_Write, Right)), accept)
      ]
  }


let neg: Turing_Machine.t =
  let init   = nop.initial and accept = nop.accept in 	
  { nop with
    name =  "neg" ; 
    transitions = 
      [ (init, Action(RWM(Match(VAL U), Write Z, Right)), init) ;
	(init, Action(RWM(Match(VAL Z), Write U, Right)), init) ; 
        (init, Action(RWM(Match(VAL B), Write B, Here )), accept) 
      ]
  }


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
  
  
let decr: Turing_Machine.t =
  let init = nop.initial and accept = nop.accept and reject = nop.reject in
  let unit = State.next_from init in
  let zero = State.next_from unit in
  let back = State.next_from zero in
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

  
let permut: Alphabet.t -> Turing_Machine.t = fun alphabet ->
  let init   = nop.initial
  and accept = nop.accept
  in
  let lphabeta = (List.tl alphabet.symbols) @ [ List.hd alphabet.symbols ] in
  let permutations = MyList.zip alphabet.symbols lphabeta
  in
  let generic_transitions = MyList.foreach_in permutations (fun (s1,s2) ->
                                [ (init, Action( RWM (Match(VAL s1),  Write s2, Right)), init) ])
  in          
  { nop with
    name = "permut" ;
    transitions = [ (init, Action( RWM (Match(VAL B), No_Write, Here)), accept) ] @ generic_transitions
  }


  
(* GENERIC Turing Machines DEPENDING ON the alphabet *)

let generic_dup: symbols -> Turing_Machine.t = fun symbols ->
  let init = nop.initial and accept = nop.accept in	  
  let generic_transitions =
    Transition.foreach_symbol_of symbols (OUT [B;D]) (fun s ->
	[ (init     , Action(RWM(Match(VAL(s)), Write B, Right)) , Qs(1,[s])) ; 
	  (Qs(1,[s]), Run(second_blank_on_the Right), Qs(2,[s])) ; 
	  (Qs(2,[s]), Action(RWM(Match(VAL(B)), Write s, Here)) , Qs(3,[s])) ;
	  (Qs(3,[s]), Run (second_blank_on_the Left)            , Qs(4,[s])) ; 
	  (Qs(4,[s]), Action(RWM(Match(VAL(B)), Write s, Right)), init)
      ])
  in 
  { nop with
    name = "dup_" ^ (Pretty.set Symbol.to_ascii symbols) ;
    transitions = generic_transitions
		  @ [ (init, Action(RWM(Match(VAL B), Write D, Here)), accept) ] 
  }
  
	
let generic_swap: symbols -> Turing_Machine.t = fun symbols ->
  let init = nop.initial and accept = nop.accept in	  
  let generic_transitions =
    Transition.foreach_symbol_of symbols ANY (fun s ->
	Transition.foreach_symbol_of symbols ANY (fun l ->
	    [ (init     , Action( RWM (Match(VAL(s)), Write B, Right)), Qs(1,[s])) ; 
	      (Qs(1,[s]), Action( RWM (Match(VAL(l)), Write s, Left )), Qs(2,[l])) ;
	      (Qs(2,[l]), Action( RWM (Match(VAL(B)), Write l, Right)), accept)
      ]))
  in 
  { nop with
    name = "swap_" ^ (Pretty.set Symbol.to_ascii symbols) ; 
    transitions = generic_transitions
  }



let generic_dec: symbols -> Turing_Machine.t = fun symbols ->
  let init   = nop.initial and accept = nop.accept 
  in let generic_transitions =
       Transition.foreach_symbol_of symbols (BUT B) (fun s1 ->
           (Transition.foreach_symbol_of symbols (BUT B) (fun s2 ->
                [ (init      , Action(RWM(Match(VAL s1), Write T , Right)), Qs(1,[s1])) ;
                  (Qs(1,[s1]), Action(RWM(Match(VAL s2), Write s1, Right)), Qs(1,[s2])) ;
                  (Qs(1,[s1]), Action(RWM(Match(VAL B), Write s1, Right)), accept)
                ]
         )))
     in 
     { nop with
       name = "dec_TM" ;
       transitions =  [ (init, Action(RWM(Match(VAL B), No_Write, Here)), State.reject) ] @ generic_transitions
     }

  
(* DUMMY Turing Machines JUST FOR TESTING THE ENGINE *)


  let dummy: Turing_Machine.t =
  let z = Bit.zero
  and u = Bit.unit in
  let init   = nop.initial and accept = nop.accept 
  in
  { nop with
    name = "dummy" ;
    transitions = 
      [ (init, Action( RWM (Match(VAL u), No_Write, Right)), Q 1) ;
	(init, Action( RWM (Match(VAL z), Write u , Here )), Q 2) ;
        
	(Q 2 , Action( RWM (Match(VAL u), No_Write, Left )), Q 2) ;
	(Q 2 , Action( RWM (Match(VAL z), No_Write, Right)), Q 3) ;
        
	(Q 3 , Action( RWM (Match(VAL u), Write z , Right)), init) ;
	(Q 3 , Action( RWM (Match(VAL z), No_Write, Here )), accept) 
      ]
  }


let test_TM01: Turing_Machine.t =
  let init = nop.initial and accept = nop.accept in 	
  { nop with
    name = "incr_vec" ;
    transitions =
      [ (init, Action(RWM( Match(IN[O;S;C]), No_Write, Right)), init) ;
        (init, Action(RWM( Match(IN[Z;U])  , Write B , Right)), init) ;
        (init, Action(RWM( Match(VAL B)    , No_Write, Here )), accept)
      ]
  }
