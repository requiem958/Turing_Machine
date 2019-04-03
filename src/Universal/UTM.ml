(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT
 *
 *   Realisation of the Universal Turing Machine as a 3-Bands TM (PROJECT 2019)
 *
 *)


(* PROJET 2019
 *
 * Contributeurs
 *
 * NOM Prénom : ANDRE Stephen
 * NOM Prénom : CHLOUS Clément
 * NOM Prénom : FREBY Laura
 * NOM Prénom : MONNIER Marius
 *
 *)


open Symbol
open Alphabet
open Band

open Pattern
open Action
open State
open Turing_Machine


open Configuration
open Execution

(** TYPE *)


type machine_code = string * (Symbol.t list) (* (name,code) where code = sequence of symbols of the Alphabet.utm *)

type data = Symbol.t list (* sequence of symbols of the Alphabet.zero_unit = {Z,U} *)




(** Some Turing Machines FOR TESTING the Universal Turing Machine *)

let nop: Turing_Machine.t = Turing_Machine.nop


(*** The "executable" TM that does the negation of each bit of a binary word *)

let neg_TM: Turing_Machine.t =
  let init   = nop.initial
  and accept = nop.accept
  in Turing_Machine.export
  { nop with
    name =  "M_neg" ;
    transitions =
      [ (init, Action(RWM(Match(VAL U), Write Z, Right)), init) ;
	      (init, Action(RWM(Match(VAL Z), Write U, Right)), init) ;
        (init, Action(RWM(Match(VAL B), Write B, Here )), accept)
      ]
  }

(**** The named "code" of the previous TM *)

let neg_code: machine_code =
  let code =
    [ O;Std;Z;C ; U ; Z ; R ; O;Std;Z;C ; (* (Std0) -U/Z:R-> (Std0) *)
      O;Std;Z;C ; Z ; U ; R ; O;Std;Z;C ; (* (Std0) -Z/U:R-> (Std0) *)
      O;Std;Z;C ; B ; B ; H ; O;Acc;U;C   (* (Std0) -B/B:H-> (Acc1) *)
    ]
  in ("m_neg",code)



(*** A TM that increases an little-endian binary integer by one unit: incr([n]_2) = [n+1]_2 *)

let incr_TM: Turing_Machine.t =
  let init = nop.initial
  and accept = nop.accept
  in Turing_Machine.export
  { nop with
    name =  "M_incr" ;
    transitions =
      [ (init, Action(RWM(Match(VAL U), Write Z, Right)), init) ;
	       (init, Action(RWM(Match(VAL Z), Write U, Here )), accept) ;
        (init, Action(RWM(Match(VAL B), Write U, Here )), accept)
      ]
  }

(**** The named "code" of the previous TM *)

let incr_code: machine_code =
  let code = [
    O;Std;Z;C ; U ; Z ; R ; O;Std;Z;C ;
    O;Std;Z;C ; Z ; U ; H ; O;Acc;U;C ;
    O;Std;Z;C ; B ; U ; H ; O;Acc;U;C
  ]
  in
  ("m_incr", code)




(*** decreases an little-endian binary integer by one unit: incr([n]_2) = [n-1]_2 *)

let decr_TM: Turing_Machine.t =
  let init = nop.initial and accept = nop.accept and reject = nop.reject in
  let unit = State.fresh_from init in
  let zero = State.fresh_from unit in
  let back = State.fresh_from zero
  in Turing_Machine.export
  { nop with
    name = "M_decr" ;
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

(*  Init := 0
    Unit := 1
    Accept := 01
    Reject := 11
    Zero := 001
    Back := 101
    TmpZero1 := 011 => Pour séquence
    TmpZero2 := 111
*)
let decr_code: machine_code =
  let code = [
    O;Std;Z;C ; Z ; Z ; R ; O;Std;U;C;
    O;Std;Z;C ; U ; Z ; H ; O;Acc;Z;U;C;
    O;Std;Z;C ; B ; B ; H ; O;Exc;U;U;C;

    O;Std;U;C ; B ; B ; L ; O;Exc;U;U;C;
    O;Std;U;C ; Z ; Z ; R ; O;Std;U;C;
    O;Std;U;C ; U ; B ; R ; O;Std;Z;Z;U;C;

    O;Std;Z;Z;U;C ; B ; B ; L ; O;Std;Z;U;U;C;
    O;Std;Z;U;U;C ; B ; B ; L ; O;Std;U;Z;U;C;

    O;Std;Z;Z;U;C ; Z ; Z ; L ; O;Std;U;U;U;C;
    O;Std;Z;Z;U;C ; U ; U ; L ; O;Std;U;U;U;C;

    O;Std;U;U;U;C ; B ; Z ; L ; O;Std;U;Z;U;C;

    O;Std;U;Z;U;C ; Z ; U ; L ; O;Std;U;Z;U;C;
    O;Std;U;Z;U;C ; B ; B ; R ; O;Std;Z;U;C;
  ]
  in  ("m_decr", code)



(** THE UNIVERSAL TURING MACHINE *)

(* UTM is a three-bands Turing Machine
 *
 * UTM(m,w) acts as an interpreter:  It runs the code of the Turing Machine m (written on band B2) onto the data w (witten on band B1)
 * The current state of m is written on band B3.
 *
 *)


let utm: Turing_Machine.t =
	let init = nop.initial in
	let accept = nop.accept in
	let reject = nop.reject in
	let std1 = State.fresh_from init in

	let prchTransD = State.fresh_from std1 in
	let prchTrans_o = State.fresh_from std1 in
	let prchTransA = State.fresh_from std1 in

	let cmpEtatD = State.fresh_from std1 in
	let cmpEtatA_r = State.fresh_from std1 in
	let cmpEtatE_r = State.fresh_from std1 in
	let cmpEtatA = State.fresh_from std1 in
	let cmpEtatE = State.fresh_from std1 in

	let cmpReadD = State.fresh_from std1 in
	let cmpReadA = State.fresh_from std1 in
	let cmpReadE = State.fresh_from std1 in

	let rechTransD = State.fresh_from std1 in
	let rechTrans1 = State.fresh_from std1 in
	let rechTransA = State.fresh_from std1 in

	let excTransD = State.fresh_from std1 in
	let excTrans_m = State.fresh_from std1 in
	let excTransA = State.fresh_from std1 in

	let chEtatD = State.fresh_from std1 in
	let chEtat1 = State.fresh_from std1 in
	let chEtat2 = State.fresh_from std1 in
	let chEtatR = State.fresh_from std1 in
	let chEtatA = State.fresh_from std1 in

	let loopD = State.fresh_from std1 in

	(* Penser à rembobiner au tout début *)
	(* Finit sur Std *)
	in
	let prchTrans_transitions = [
		(prchTransD, Action( Simultaneous [ Nop ; RWM(Match(OUT [C; B]), No_Write, Right) ; Nop ] ), prchTransD) ;
		(prchTransD, Action( Simultaneous [ Nop ; RWM(Match(VAL B), No_Write, Here) ; Nop ] ), reject) ;
		(prchTransD, Action( Simultaneous [ Nop ; RWM(Match(VAL C), No_Write, Right) ; Nop ] ), prchTrans_o) ;
		(prchTrans_o, Action( Simultaneous [ Nop ; RWM(Match(OUT [O ;B]), No_Write, Right) ; Nop ] ), prchTransD) ;
		(prchTrans_o, Action( Simultaneous [ Nop ; RWM(Match(VAL B), No_Write, Here) ; Nop ] ), reject) ;
		(prchTrans_o, Action( Simultaneous [ Nop ; RWM(Match(VAL O), No_Write, Right) ; Nop ] ), prchTransA)
	]

	(* Commence au début du numéro d'état sur B2 *)
	(* Finit sur C et au début de B3 *)
	in
	let cmpEtat_transitions = [
		(cmpEtatD, Action( Simultaneous [ Nop ; RWM(Match(VAL Z), No_Write, Right) ; RWM(Match(VAL Z), No_Write, Right) ] ), cmpEtatD) ;
		(cmpEtatD, Action( Simultaneous [ Nop ; RWM(Match(VAL U), No_Write, Right) ; RWM(Match(VAL U), No_Write, Right) ] ), cmpEtatD) ;
		(cmpEtatD, Action( Simultaneous [ Nop ; RWM(Match(VAL C), No_Write, Right) ; RWM(Match(VAL B), No_Write, Left) ] ), cmpEtatA_r) ;
		(cmpEtatD, Action( Simultaneous [ Nop ; RWM(Match(VAL U), No_Write, Here) ; RWM(Match(VAL Z), No_Write, Here) ] ), cmpEtatE_r) ;
		(cmpEtatD, Action( Simultaneous [ Nop ; RWM(Match(VAL Z), No_Write, Here) ; RWM(Match(VAL U), No_Write, Here) ] ), cmpEtatE_r) ;
		(cmpEtatD, Action( Simultaneous [ Nop ; RWM(Match(VAL C), No_Write, Here) ; RWM(Match(BUT B), No_Write, Here) ] ), cmpEtatE_r) ;
		(cmpEtatD, Action( Simultaneous [ Nop ; RWM(Match(BUT C), No_Write, Here) ; RWM(Match(VAL B), No_Write, Left) ] ), cmpEtatE_r) ;
		(cmpEtatA_r, Parallel [ Action(Nop) ; Action(Nop) ; Run(TM_Basic.left_most) ], cmpEtatA) ;
		(cmpEtatE_r, Parallel [ Action(Nop) ; Action(Nop) ; Run(TM_Basic.left_most) ], cmpEtatE)
	]

	(* Commence sur R *)
	(* Finit sur W *)
	in
	let cmpRead_transitions = [
		(cmpReadD, Action( Simultaneous [ RWM(Match(VAL Z), No_Write, Here) ; RWM(Match(VAL Z), No_Write, Right) ; Nop ] ), cmpReadA) ;
		(cmpReadD, Action( Simultaneous [ RWM(Match(VAL U), No_Write, Here) ; RWM(Match(VAL U), No_Write, Right) ; Nop ] ), cmpReadA) ;
		(cmpReadD, Action( Simultaneous [ RWM(Match(VAL B), No_Write, Here) ; RWM(Match(VAL B), No_Write, Right) ; Nop ] ), cmpReadA) ;
		(cmpReadD, Action( Simultaneous [ RWM(Match(BUT Z), No_Write, Here) ; RWM(Match(VAL Z), No_Write, Right) ; Nop ] ), cmpReadE) ;
		(cmpReadD, Action( Simultaneous [ RWM(Match(BUT U), No_Write, Here) ; RWM(Match(VAL U), No_Write, Right) ; Nop ] ), cmpReadE) ;
		(cmpReadD, Action( Simultaneous [ RWM(Match(BUT B), No_Write, Here) ; RWM(Match(VAL B), No_Write, Right) ; Nop ] ), cmpReadE)
	]

	(* Commence sur Std *)
	(* Finit sur W *)
	in
	let rechTrans_transitions = [
        (rechTransD, Action(Simultaneous [Nop; RWM(Match ANY, No_Write, Right); Nop]), cmpEtatD);

        (rechTrans1, Action(Nop), cmpEtatD);
        (cmpEtatA, Action(Simultaneous [Nop; RWM(Match ANY, No_Write, Right); Nop]), cmpReadD);
        (cmpEtatE, Action(Nop), prchTransD);

        (cmpReadA, Action(Nop), rechTransA);
        (cmpReadE, Action(Nop), prchTransD);

        (prchTransA, Action(Nop), rechTransD)
	]

	(* Commence sur W *)
	(* Finit sur O *)
	in
	let excTrans_transitions = [
		(excTransD, Action( Simultaneous [ RWM(Match ANY, Write Z, Here) ; RWM(Match(VAL Z), No_Write, Right) ; Nop ]), excTrans_m) ;
		(excTransD, Action( Simultaneous [ RWM(Match ANY, Write U, Here) ; RWM(Match(VAL U), No_Write, Right) ; Nop ]), excTrans_m) ;
		(excTransD, Action( Simultaneous [ RWM(Match ANY, Write B, Here) ; RWM(Match(VAL B), No_Write, Right) ; Nop ]), excTrans_m) ;
		(excTransD, Action( Simultaneous [ RWM(Match ANY, Write B, Here) ; RWM(Match(VAL B), No_Write, Right) ; Nop ]), excTrans_m) ;
		(excTrans_m, Action( Simultaneous [ RWM(Match ANY, No_Write, Left) ; RWM(Match(VAL L), No_Write, Right) ; Nop ]), excTransA) ;
		(excTrans_m, Action( Simultaneous [ RWM(Match ANY, No_Write, Right) ; RWM(Match(VAL R), No_Write, Right) ; Nop ]), excTransA) ;
		(excTrans_m, Action( Simultaneous [ RWM(Match ANY, No_Write, Here) ; RWM(Match(VAL H), No_Write, Right) ; Nop ]), excTransA)
	]

	(* Commence sur Std *)
	(* Finit sur C et au début de B3 *)
	in
	let chEtat_transitions = [
        (chEtatD, Action(Simultaneous [Nop; RWM(Match(VAL Acc), No_Write, Right); Nop]), accept);
        (chEtatD, Action(Simultaneous [Nop; RWM(Match(VAL Exc), No_Write, Right); Nop]), reject);
        (chEtatD, Action(Simultaneous [Nop; RWM(Match(VAL Std), No_Write, Right); Nop]), chEtat1);

        (chEtat1, Action(Simultaneous [Nop; RWM(Match(BUT S), No_Write, Right); Nop]), reject);
        (chEtat1, Action(Simultaneous [Nop; RWM(Match(VAL S), No_Write, Right); Nop]), chEtat2);

        (chEtat2, Action(Simultaneous[Nop; RWM(Match(VAL Z), No_Write, Right); RWM(Match ANY, Write Z, Right)]), chEtat2);
        (chEtat2, Action(Simultaneous[Nop; RWM(Match(VAL U), No_Write, Right); RWM(Match ANY, Write U, Right)]), chEtat2);
        (chEtat2, Action(Simultaneous[Nop; RWM(Match(VAL C), No_Write, Here); Nop]), chEtatR);

        (chEtatR, Run_on(TM_Basic.left_most, [3]), chEtatA)
	]

	in
	let loop_transitions = [
		(loopD, Action( Simultaneous [ Nop ; RWM(Match ANY, No_Write, Right) ; Nop ] ), rechTransD) ;
		(rechTransA, Action( Simultaneous [ Nop ; Nop ; Nop ] ), excTransD) ;
		(excTransA, Action( Simultaneous [ Nop ; RWM(Match ANY, No_Write, Right) ; Nop ] ), chEtatD) ;
		(chEtatA, Parallel [ Action(Nop) ; Run(TM_Basic.left_most) ; Action(Nop) ], loopD) 
	]

	in
	let init_transitions = [
		(init, Parallel [ Run(TM_Basic.left_most) ; Run(TM_Basic.left_most) ; Nop ], std1) ;
		(std1, Action( Simultaneous [ Nop ; RWM(Match ANY, No_Write, Right) ; Nop ] ), chEtatD)
	]

(*
  in
  let macros_transitions =
    Transition.foreach_symbol_of Alphabet.utm.symbols (IN [O;Std;Acc;Exc;Z;U])
      (fun s ->
         [ (init, Action( Simultaneous [ Nop ; RWM(Match(VAL s), No_Write, Right) ; RWM(Match ANY, Write s, Right) ]), init) ]
      ) *)

	in Turing_Machine.export
	{ 	nop with
		nb_bands = 3 ;
		name = "UTM" ;
		transitions = [
        (* macros_transitions  @
        [
          (init, Action( Simultaneous [ Nop ; RWM(Match(VAL C), No_Write, Right) ; RWM(Match ANY, Write C, Right)]), std1) ;
          (std1, Parallel [ Action(Nop) ; Run(TM_Basic.left_most) ; Run(TM_Basic.left_most) ], accept)
          (* ... à compléter ... *)

        ] *)
			init_transitions @
			loop_transitions @
			rechTrans_transitions @
			prchTrans_transitions @
			cmpEtat_transitions @
			cmpRead_transitions @
			excTrans_transitions @
			chEtat_transitions @
			loop_transitions @
			init_transitions
		]
	}


(* Using the UTM as an interpreter:  UTM m w *)

let run_UTM_on: machine_code -> data -> Configuration.t = fun interpretable_m w ->
  let (machine_name,machine_code) = interpretable_m in
  let band1 = Band.make "Data" Alphabet.zero_unit  w
  and band2 = Band.make machine_name Alphabet.utm  machine_code
  and band3 = Band.make "State" Alphabet.utm []
  in
  let cfg = Configuration.make utm [ band1 ; band2 ; band3 ]
  in
  Execution.i_log_run cfg

let run_TM_on_three_bands:  Turing_Machine.t -> data-> data-> data -> Configuration.t = fun interpretable_m w1 w2 w3 ->
  let band1 = Band.make "Data" Alphabet.zero_unit  w1
  and band2 = Band.make interpretable_m.name Alphabet.utm  w2
  and band3 = Band.make "State" Alphabet.zero_unit w3
  in
  let cfg = Configuration.make  interpretable_m [ band1 ; band2 ; band3 ]
  in
  Execution.i_log_run cfg



(* Running an executable TM on a word w *)

let run_TM_on: Turing_Machine.t -> data -> Configuration.t = fun executable_M w  ->
  let band = Band.make "Data" Alphabet.zero_unit w
  in
  let cfg = Configuration.make executable_M [ band ]
  in
  Execution.i_log_run cfg



(* DEMO *)

let demo: unit -> unit = fun () ->
  begin
    print_string "\n\n* DEMO * UTM.ml:\n\n" ;
    List.iter (fun _ -> ())
      [ run_TM_on neg_TM [U;Z;Z;U] ;
        (*run_TM_on_three_bands changeEtat [] [Std;S;Z;U;U;C] [];*)
        run_UTM_on neg_code [U;Z;Z;U] ;
        (* ... à compléter ... *)
        run_UTM_on incr_code [Z;Z;U;U] ;
        run_UTM_on decr_code [Z;U;U;Z] ;
      ]
  end
