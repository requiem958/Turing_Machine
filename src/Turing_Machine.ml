(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT 
 *
 *   Turing Machines
 *
 * USAGE
 *
 *   Requirement
 *   - Module   :  MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Pattern.cmo Band.cmo Action.cmo State.cmo Band.cmo Transition.cmo 
 *   - Library  :  
 *   Compilation:  ocamlc      MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Pattern.cmo Band.cmo Action.cmo State.cmo Band.cmo Transition.cmo Turing_Machine.ml
 *   Interpreter:  ledit ocaml MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Pattern.cmo Band.cmo Action.cmo State.cmo Band.cmo Transition.cmo Turing_Machine.cmo 
 *
 *)

 
open Alphabet
open State
open Pattern
open Symbol
open Action
open Transition
open Html
open Pretty


(* TODO: put the modules Transition and Instruction from Transition.ml here *)
   
module Turing_Machine =
  (struct
    
    type t = turing_machine 
	   
    let (nop: t) = { name = "" ;
		     nb_bands = 1 ; (* TO BE REMOVED *)
                     active_bands = [1] ;
		     initial = State.initial ;
                     accept  = State.accept  ;
                     reject  = State.reject  ;
		     transitions = [ (State.initial, Action(Nop), State.accept) ]
		   }

    let operates_on: Band.indexes -> turing_machine -> turing_machine = fun indexes tm ->
      { tm with active_bands = indexes }

    let naming: string -> turing_machine -> turing_machine = fun name tm ->
      { tm with name = name }

    let sequence: Instruction.t list -> turing_machine = fun instructions ->
      let init = nop.initial and accept = nop.accept in
      { nop with
	name = Instruction.to_ascii (Seq instructions) ;
	transitions = [ (init, Seq instructions, accept) ]	    
      }

    (* PRETTY PRINTING *)

    let to_ascii: turing_machine -> string = fun tm -> tm.name

    let to_html: Html.options -> turing_machine -> Html.content = fun _ tm -> Html.italic (to_ascii tm)
	    
    (* user *)
	    
    let pretty (* user *) : t -> string =
      match Pretty.get_format() with
      | Pretty.Html  -> (to_html [])
      | Pretty.Ascii -> to_ascii
   (* | Pretty.Dot   -> TODO *)
		
		
    (* IMPERATIVE FEATURES for reusing existing turing machine *) 	    
		
    class collection_of_turing_machine =
    object
      val mutable collection: turing_machine list = []
	    
      method add: turing_machine -> unit = fun tm ->
	      collection <- tm::collection 
				  
      method find: string -> turing_machine = fun name ->
	match List.filter (fun tm -> tm.name = name) collection with
	| [tm] -> tm
	| [] -> let error_msg = String.concat "" [ "Turing_Machine.collection_of_turing_machine #find: TM " ; name ; " not found in the library." ] in failwith error_msg
	| _  -> let error_msg = String.concat "" [ "Turing_Machine.collection_of_turing_machine #find: Multiple TM " ; name ; " in the library."  ] in failwith error_msg
    end
	
	
    let global_TM_library = new collection_of_turing_machine

    (* FIXME i_store is a better name *)
	                  
    let i_store: string -> turing_machine -> turing_machine = fun name turing_machine ->
      let tm = naming name turing_machine in
      begin
	global_TM_library#add tm ;
	tm
      end
	      
    let i_find_tm_named: string -> turing_machine = fun name ->
      global_TM_library#find name
	    
  end)


