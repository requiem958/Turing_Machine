(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENTS 
 *
 *   Execution of single-band or multi-bands Turing Machines with logging of the execution steps
 *
 *   - the filename for logging is set in Configuration.ml
 *)


open Tricks
  
open State  
open Band    
open Action
open Turing_Machine
open Configuration  


module Execution =
  (struct

    type log = Logger.t list (* TODO explain *)
             
  
    let select_enabled_transition: Turing_Machine.t -> Configuration.t -> Transition.t option = fun tm cfg ->
      let
	enabled_transitions = List.filter (Configuration.is_transition_enabled_on cfg) tm.transitions
      in
      match enabled_transitions with
      | []           -> None
      | [transition] -> Some transition
      | transitions  ->
	 let error = "Execution.select_enabled_transition: non deterministic TM" in
	 let msg = String.concat " "
		     [ error ; cfg.tm.name ; String.concat "\n - " (List.map Transition.pretty transitions) ]
	 in begin print_string msg ; failwith error  end
          
	  
   (* mutually recursives functions to deal with the instruction Run(TM)  *)

    let rec execute_instruction_on_one_band: log -> instruction -> Band.t -> Band.t = fun log instruction band ->
	  List.hd (execute_instruction log instruction [band])
	    
    and execute_instruction: log -> instruction -> Band.t list -> Band.t list = fun log instruction bands ->
      match instruction with
      | Action action -> Action.perform action bands
		    
      | Seq [] -> bands
      | Seq (inst::instructions) ->
         bands
         |> (execute_instruction log inst)
         |> (execute_instruction log (Seq instructions))
                                  
      | Parallel instructions ->
	 List.map
	   (fun (inst,band) -> execute_instruction_on_one_band log inst band)
           (Instruction.zip instructions bands) 

      | Run_on (tm, indexes) -> (* one reason of mutual recursivity: calls the "i_log_run" function which finally calls "execute_instruction" *)
         let (selected_bands, unmodified_bands) = Band.split_wrt indexes bands in 
         let initial_cfg = Configuration.make tm selected_bands in
         let final_cfg = i_log_run ~log:log initial_cfg
         in Band.join unmodified_bands final_cfg.bands 
	
      | Call name -> run_tm_named name bands (* DEPRECATED *)                       

      | Run tm -> (* DEPRECATED *)
         let initial_cfg = Configuration.make tm bands in
	 let final_cfg = i_log_run ~log:log initial_cfg
         in final_cfg.bands

    and execute_transition: log -> Transition.t -> Configuration.t -> Configuration.t = fun log (_,instruction,target) cfg ->
      { cfg with bands = execute_instruction log instruction cfg.bands ;
                 state = target
      } 

    and one_step: int * log -> Configuration.t -> Configuration.t = fun (call_depth,log) cfg ->
      begin
	Configuration.print_using ~n_first:call_depth log cfg
      ;
	match select_enabled_transition cfg.tm cfg with
	| None            -> { cfg with status = Final }
	| Some transition -> execute_transition log transition cfg
      end

    and i_log_run ?call_depth:(call_depth=2) ?log:(log=[]) : (Configuration.t -> Configuration.t) = fun cfg ->
      let final_cfg = run (call_depth, cfg.logger::log) cfg in
      begin
	cfg.logger#close ;
	final_cfg
      end

    and run: int * log -> Configuration.t -> Configuration.t = fun ilog cfg ->
    	  if (cfg.status = Final)
	  then cfg
	  else
	    let next_cfg = one_step ilog cfg
	    in run ilog next_cfg
	      
    and (run_tm_named: string -> Band.t list -> Band.t list) = fun name bands ->
	  let tm = Turing_Machine.i_find_tm_named name in
	    let final_cfg = run (1,[]) (Configuration.make ~time:true tm bands)
	    in  final_cfg.bands

  end)

