(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT 
 *
 *  - Turing Machines
 *  - Transitions of Turing Machines
 *  - Intruction  of Transition of Turing Machines
 *  
 *
 * USAGE
 *
 *   Requirement
 *   - Module   :  MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Pattern.cmo Band.cmo Action.cmo State.cmo Band.cmo
 *   - Library  :  
 *   Compilation:  ocamlc      MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Pattern.cmo Band.cmo Action.cmo State.cmo Band.cmo Turing_Machine.ml
 *   Interpreter:  ledit ocaml MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Pattern.cmo Band.cmo Action.cmo State.cmo Band.cmo Turing_Machine.cmo 
 *
 *)

 
open Symbol
open Alphabet
open Band   

open State
open Pattern
open Action

open Html
open Pretty

open MySet
   
(** Types *)  
  
type transition = State.t * instruction * State.t

and instruction =
  | Action of Action.t
  | Seq  of instruction list (* a sequence of instructions *)

  | Run_on of turing_machine * Band.indexes (* Run_on(Turing Machine, indexes of bands) *)

  | Parallel of instruction list (* N one-band instructions in paralell on N bands *)

  (* DEPRECATED *)             
  | Run  of turing_machine (* DEPRECATED *)
  | Call of string (* DEPRECATED: the name of an existing turing machine *)


and transitions = transition list
  
and turing_machine = { name        : string ;
		       nb_bands    : int ;
		       initial     : State.t ;
                       accept      : State.t ;
                       reject      : State.t ;
		       transitions : transitions ;
		     }

                   
(** INSTRUCTION *)
                   
module Instruction =
  (struct
    type t = instruction

    let (nop: instruction) = Seq []
	
    let (zip: instruction list -> Band.t list -> (instruction * Band.t) list) =  Band.zip_complete_with nop 


    (* ENABLED ONE INSTRUCTION on ONE BAND *)
                                                                                                             
    let rec is_enabled_on_this: Band.t -> instruction -> bool = fun band instruction ->
      match instruction with
      | Action action -> Action.is_enabled_on_this band action
      | Call _ | Run  _ -> true
      | Seq [] -> false (* FIXME SEMANTICS: false? or true?: is there a risk of looping by stuttering? *)
      | Seq (first_instruction::_) -> is_enabled_on_this band first_instruction


    (* ENABLED COMPLEX INSTRUCTION on MULTIPLE BANDS *)
                                                                                                             
    let rec is_enabled_on: Band.t list -> instruction -> bool = fun bands instruction ->
      (bands <> [])
      &&
	(match instruction with
	 | Action action -> Action.is_enabled_on bands action
		          
	 | Call _  | Run  _ -> true

         | Seq [] -> false 
	 | Seq (first_instruction::_) -> is_enabled_on bands first_instruction
		                       
	 | Parallel instructions ->
	    List.for_all
	      (fun (instruction,band) -> is_enabled_on_this band instruction)
	      (zip instructions bands)
	)

    (* INSTRUCTION ID used for creating DOT nodes *)
      
    let id: instruction -> string = fun instruction ->
      match instruction with
      | Action action -> Action.id action
      | Call name -> name
      | Run tm -> tm.name
      | Seq _ -> "SEQ"
      | Parallel _ -> "PAR"

      
    (* PRETTY PRINTING *)
	
    let rec to_ascii: t -> string = fun instruction ->
      match instruction with
      | Action action -> Action.to_ascii action
      | Call tm_name ->  tm_name
      | Run tm       ->  tm.name
      | Seq instructions -> "SEQ" ^ (Pretty.brace (String.concat " ; " (List.map to_ascii instructions)))
      | Parallel instructions -> String.concat " || " (List.map to_ascii instructions)
		    
    let to_html: Html.options -> instruction -> Html.cell = fun options instruction ->
      Html.cell options (to_ascii instruction)

    let rec to_dot: t -> string = fun instruction ->
      match instruction with
      | Action action -> Action.to_dot action
      | Call tm_name -> String.concat "" ["run(" ; tm_name ; ")" ]
      | Run tm       -> String.concat "" ["run(" ; tm.name ; ")" ]
      | Seq instructions -> "SEQ" ^ (Pretty.brace (String.concat " ; " (List.map to_dot instructions)))
      | Parallel instructions -> String.concat " || " (List.map to_dot instructions)

    (* USER *)

    let pretty: t -> string = fun t ->
	  match Pretty.get_format() with
	  | Pretty.Html  -> to_html [] t
	  | Pretty.Ascii -> to_ascii t
	  | Pretty.Dot   -> to_dot t

  end)


  
(** TRANSITION *)
    
module Transition =
  (struct

    type t = transition
	  
    let (nop: State.t -> State.t -> transition) = fun source target ->  (source, Action(Nop), target)


    (* INSTANCIATON of generic transitions *)
		    
    let (foreach_symbol_of: 'a list -> 'a Pattern.t -> ('a -> transitions) -> transitions)
      = fun alphabet pattern create_transitions_for
      -> let rec
	      (instantiate_transitions_foreach_symbol_in: 'a list -> transitions) = fun symbols  ->
		    match symbols with
		    | [] -> []
		    | s::ymbols ->
			    MyList.union
			      (create_transitions_for s)
			      (instantiate_transitions_foreach_symbol_in ymbols)
	  in
	    instantiate_transitions_foreach_symbol_in (Pattern.enumerate_symbols_of alphabet pattern)

          
    (* NEEDED FOR BUILDING COMPLEMENTARY of a STANDARD 1 BAND DETERMINISTIC DECIDING TURING MACHINE*)
          
    let swap_accepting_status: transition -> transition = fun (source,action,target) ->
      let target = 
        if target = State.accept then State.reject
        else if target = State.reject then State.accept
        else target
      in (source,action,target)

    let get_pattern: transition -> Symbol.t Pattern.t = fun (_,instruction,_) ->
      match instruction with
      | Action action -> Action.get_pattern action
                       

       
   (* PRETTY PRINTING *)

    let (to_ascii: t -> string) = fun (source,instruction,target) ->
      [ Pretty.parentheses (State.to_ascii source)
      ; Instruction.to_ascii instruction
      ; Pretty.parentheses (State.to_ascii target)
      ]
      |> (String.concat "  ")
	    
    let to_ascii_many: t list -> string = fun transitions ->
      transitions
      |> (List.map to_ascii)
      |> (String.concat ";")

    let to_dot: t -> Dot.labelled_node * Dot.transition * Dot.transition = fun (src,instruction,tgt) ->
      let src_node = State.to_dot src
      and tgt_node = State.to_dot tgt    
      in let instruction_node  = String.concat "_" [ src_node ; Instruction.id instruction ]
         and instruction_label = Instruction.to_dot instruction           
         in let dot_labelled_node = (instruction_node, instruction_label) 
            and link = (src_node, "", instruction_node)
            and edge = (instruction_node,"", tgt_node) 
            in (dot_labelled_node, link, edge)

    let to_dot_many: t list -> Dot.labelled_node list * Dot.transition list * Dot.transition list = fun transitions ->
      transitions 
      |> (List.map to_dot)
      |> (List.fold_left (fun (nodes,links,edges) (n,l,e) -> (n::nodes, l::links, e::edges)) ([],[],[]))
      
    (* USER *)

    let (pretty: t -> string) = fun t ->
	  match Pretty.get_format() with
	  | Pretty.Html  
	  | Pretty.Ascii -> to_ascii t

  end)
    


(** TURING MACHINES *)

  
module Turing_Machine =
  (struct
    
    type t = turing_machine 
	   
    let (nop: t) = { name = "" ;
		     nb_bands = 1 ; 
		     initial = State.initial ;
                     accept  = State.accept  ;
                     reject  = State.reject  ;
		     transitions = [ (State.initial, Action(Nop), State.accept) ]
		   }

    let naming: string -> turing_machine -> turing_machine = fun name tm ->
      { tm with name = name }

    let sequence: Instruction.t list -> turing_machine = fun instructions ->
      let init = nop.initial and accept = nop.accept in
      { nop with
	name = Instruction.to_ascii (Seq instructions) ;
	transitions = [ (init, Seq instructions, accept) ]	    
      }

    (* INFORMATION on STATES of a TURING MACHINE *)

    let targets_of: transitions -> State.t set = fun transitions ->
      MySet.union MySet.empty (List.map (fun (_,_,q) -> q) transitions)
      
    let sources_of: transitions -> State.t set = fun transitions ->
      MySet.union MySet.empty (List.map (fun (q,_,_) -> q) transitions)
      
    let all_states_of: turing_machine -> State.t set = fun tm ->
      MySet.union (sources_of tm.transitions) (targets_of tm.transitions)
      
    let control_states_of: turing_machine -> State.t set = fun tm ->
      MySet.minus (all_states_of tm) [ State.initial ; State.accept ; State.reject ]
      
    let number_of_states: turing_machine -> int = fun tm ->
      List.length (all_states_of tm)

      
    (* PRETTY PRINTING *)

    let to_ascii: turing_machine -> string = fun tm -> tm.name

    let to_html: Html.options -> turing_machine -> Html.content = fun _ tm -> Html.italic (to_ascii tm)

    let to_dot : turing_machine -> string = fun tm ->
      let control_states = List.map State.to_dot (control_states_of tm)
      and (instruction_nodes,links,edges) = Transition.to_dot_many tm.transitions
      in Dot.digraph
           tm.name
           [ State.to_dot tm.initial ]
           [ State.to_dot tm.accept  ]
           [ State.to_dot tm.reject  ]
           control_states
           instruction_nodes
           links
           edges

    (* OUTPUT in A FILE *)

    type path = string
                  
    let i_output_to_dot_file_named (directory:string) (filename:string) (machine:t) : path =
      File.i_output_in
        (directory ^ filename,"dot")
        (to_dot machine)
                  
    let to_dot_file: turing_machine -> path = fun tm ->  i_output_to_dot_file_named "_dot/" tm.name tm

    let export: turing_machine -> turing_machine = fun tm ->
      begin
        let _ = to_dot_file tm in () ;
        tm
      end
      
                                                                            
    (* USER *)
	    
    let pretty (* user *) : t -> string =
      match Pretty.get_format() with
      | Pretty.Html  -> (to_html [])
      | Pretty.Ascii -> to_ascii
      | Pretty.Dot   -> to_dot_file 



    (* COMPLETION of a STANDARD 1 BAND TURING MACHINE *)
      
    let (* NOT TESTED *) completion: Alphabet.t -> turing_machine -> turing_machine = fun alphabet tm ->
      let states = all_states_of tm in
      let rejecting_transitions =
        MyList.foreach_in states (fun state ->
            let transitions = List.filter (fun (src,_,_) -> src = state) tm.transitions
            in 
            let addressed_symbols = transitions
                                    |> (List.map Transition.get_pattern)
                                    |> (List.map (Pattern.enumerate_symbols_of alphabet.symbols))
                                    |> MySet.big_union 
            in let missing_symbols = MySet.minus alphabet.symbols addressed_symbols
               in
               if missing_symbols = MySet.empty
               then []
               else [ (state, Action(RWM( Match(IN missing_symbols), No_Write, Here)), State.reject) ]
          )
      in { tm with
           name = String.concat "" [ "Complete(" ; tm.name ; ")" ] ;
           transitions = rejecting_transitions @ tm.transitions
         }
       

    (* The complementary of a deciding TM *)
      
    let (* NOT TESTED *) swap_accepting_status: turing_machine -> turing_machine = fun tm -> 
      { tm with 
	name = "Comp(" ^ tm.name ^ ")" ;
	transitions = List.map Transition.swap_accepting_status tm.transitions
      }  


                      
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


