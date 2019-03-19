(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * (PROJECT 2019)  1. Multi-Bands Turing Machines working on a an alphabet A can be simulated by a single band Turing Machine using a augmented Alphbet A'
 *
 * (PROJECT 2017)  2. A Turing Machine using an alphabet A can be simulated by a Turing Machine using the binary alphabet {B,D}
 *
 * This module provides means to write Emulator for Problems 1 and 2.
 *
*)

open State
open Action
open Turing_Machine

open Band
open Configuration

open Execution


(** Given a SINGLE transition -action->, an emulator builds a Turing Machine that simulates the effect of the transition on bands *)
   
type emulator   = State.t * Action.t * State.t -> Turing_Machine.t

(** Translator:
    The encoder converts bands of the original TM, denoted by OTM into bands for the Simulating TM, denoted by STM 
    The decoder converts bands of STM into bands of OTM

    Invariant: decoder(encoder(bands)) =~= bands,  no equality since it can have extra blank cells (B) around
 *)
                
type translator = Band.t list -> Band.t list


(** A simulator is made of 
    - one translator from OTM bands to STM bands : the encoder
    - one reverse translation : the decoder
    - an emulator which builds a STM for each transition of OTM 
 *)
                
type simulator  =
  { name: string ;
    encoder: translator ;
    decoder: translator ;
    emulator: emulator
  }

type simulators = simulator list


(** The structure of the simulator is that of the module Execution in Exection.ml 

    The simulator simulates the execution of the original TM on bands in the following way: 
    - The feasible transition of OTM is selected
    - The emulator builds the STM which simulates the selected transition
    - OBands are translated using the encoder into SBands
    - STM is executed on SBands until completion
    - The modified SBdans are translated back to OBands using the decoder

    The composition of emulators is allowed. For instance, a simulation using [ emulator1 ; emulator2 ] will lead
    
    (q0)-a->(q1) ==emulator1==> TM': (q0) -a_1-> (q') -a_2-> (q1)  which is executed step by step

    then (q0) -a_1-> (q') from TM' ==emulator2==> TM'': (q1) -a_1_1-> (q'') -a_1_2 ->(q')  which is executed
    then (q') -a_2-> (q1) from TM' ==emulator2==> TM'': (q') -a_2_1-> (q'') -a_2_2 ->(q1)  which is executed. 

*)

module Simulator =
  (struct

    type loggers = Logger.t list (* It is possible to have several simultaneously active loggers *)
                 

    let (make_tm_name: string ->  Turing_Machine.t) = fun name ->
      Turing_Machine.naming name Turing_Machine.nop

    (* RENAME: i_show ... *)
      
    let (show_bands_using: loggers -> string -> Band.t list -> Band.t list) = fun loggers name bands ->
      begin
        { (Configuration.make (make_tm_name name) bands) with status = Final } |> (Configuration.print_using loggers) ;
        bands
      end

       
    let rec run_using: simulators * loggers -> Configuration.t -> Configuration.t = fun (simulators,loggers) cfg ->

      match Execution.select_enabled_transition cfg.tm cfg with
      | None -> cfg
      | Some transition ->
         let next_cfg = execute_transition_using (simulators,loggers) transition { cfg with transition = Some transition }
         in run_using (simulators,loggers) next_cfg

    and execute_transition_using: simulators * loggers -> Transition.t -> Configuration.t -> Configuration.t = fun (simulators,loggers) (source,instruction,target) cfg ->

      let next_cfg = execute_instruction_using (simulators,loggers) (source,instruction,target) cfg
      in { next_cfg with state = target}


    and execute_instruction_using: simulators * loggers -> (State.t * Instruction.t * State.t) -> Configuration.t -> Configuration.t = fun (simulators,loggers) (source,instruction,target) cfg ->
      
      match instruction with
      | Run tm -> 
         run_using (simulators,loggers) (Configuration.make tm cfg.bands)
        
      | Seq [] -> cfg
      | Seq [inst] -> execute_instruction_using (simulators,loggers) (source, inst, target) cfg
      | Seq (inst::instructions) ->
         let intermediate_state = State.next_from source in
         cfg
         |> (execute_instruction_using (simulators,loggers) (source, inst, intermediate_state))
         |> (execute_instruction_using (simulators,loggers) (intermediate_state, Seq instructions, target))
         
      | Parallel instructions ->
         let next_bands =
           List.map
             (fun (inst,band) -> execute_single_band_instruction_using (simulators,loggers) (source,inst,target) band)
             (Instruction.zip instructions cfg.bands)
         in { cfg with bands = next_bands }
          
      | Action action -> execute_action_using (simulators,loggers) (source,action,target) cfg

                       
    and execute_single_band_instruction_using: simulators * loggers -> (State.t * Instruction.t * State.t) -> Band.t -> Band.t = fun (simulators,loggers) (src,instruction,tgt) band ->
      
      let cfg = Configuration.make (make_tm_name (Instruction.pretty instruction)) [band] in
      let next_cfg = execute_instruction_using (simulators,loggers) (src,instruction,tgt) cfg 
      in List.hd next_cfg.bands


    and execute_action_using: simulators * loggers -> (State.t * Action.t * State.t) -> Configuration.t -> Configuration.t = fun (simulators,loggers) (src,action,tgt) cfg ->

      let cfg = cfg  |> (Configuration.show_using loggers) 
      in
      let next_bands =
        match simulators with
        | [] (* all emulators have been applied resulting in the current action which can now be applied using standard execution *)
          ->
           Action.perform action cfg.bands

        | simulator :: other_simulators
          ->
           let emulation_tm = simulator.emulator (src,action,tgt)
           and emulation_bands = (simulator.encoder cfg.bands) |> (show_bands_using loggers (String.concat " " [ "call" ; simulator.name ; "on"]))
           in let emulation_cfg = Configuration.make emulation_tm emulation_bands
              in
              let e_next_cfg = log_run_using (other_simulators,loggers) emulation_cfg (* recursive call to run the other emulators *)
              in
              let bands_updated_by_emulation = (simulator.decoder e_next_cfg.bands) |> (show_bands_using loggers (String.concat " " [ simulator.name ; "returns"]))
              in
              let bands_updated_by_execution = Action.perform action cfg.bands (* for checking the correctness of the emulators *)
              in
              if (* FIXME: Band.are_equivalents (regardless of starting or ending B) instead of = *)
                bands_updated_by_execution = bands_updated_by_emulation
              then bands_updated_by_execution
              else
                begin
                  (String.concat "\n" [ Band.to_ascii_many bands_updated_by_emulation ; Band.to_ascii_many bands_updated_by_execution ; "\n" ]) |> print_string ;
                  failwith
                     (String.concat "\n" [ "Emulator.execute_action_using: simulation errors" ;
                                           Band.to_ascii_many  bands_updated_by_emulation ;
                                           "are not equivalent to" ;
                                           Band.to_ascii_many  bands_updated_by_execution ;
                     ])
                end
      in
      { cfg with bands = next_bands ; transition = Some (src,Action action,tgt) }
      
  
    and log_run_using: simulators * loggers -> Configuration.t -> Configuration.t = fun (simulators,loggers) cfg ->
      let loggers = cfg.logger :: loggers
      in
      let final_cfg = (run_using (simulators,loggers) cfg) |> (Configuration.show_using loggers)
      in
      begin
        cfg.logger#close ;
        final_cfg
      end

  end)


open State
open Symbol
open Alphabet
open Pattern
open Action
open Band
open Transition
open Turing_Machine

(* An example of a useless but correct translation that splits the effect of a transition into three steps

   (q) -- l / e : d --> (q')
   ===
   (q) -- l : H --> (q.0) -- ANY / e : H --> (q.00) -- ANY / No_Writing : d --> (q')
*)


(** EMULATORS only have to provide emulation of actions. 
    The simulation of complex instructions (sequence of actions, simultaneous actions, parrallel actions, call to TM) are adressed at the simulator.
*)


   
   
module Trace =
  struct
    
    (* BAND TRANSLATORS *)
    
    let encode: translator = fun x -> x
                                    
    let decode: translator = fun x -> x

    (* EMULATOR *)
                                    
    let trace_action: emulator = fun (source,action,target) ->
      { Turing_Machine.nop with
        name = String.concat "" [ Transition.to_ascii (source, Action action, target) ] ;
        transitions = [ (State.initial,Action action,State.accept) ]
      }
                               
    (* SIMULATOR *)

    let (* USER *) (simulator: simulator) = { name = "Trace" ; encoder = encode ;  decoder = decode ; emulator = trace_action }

end

  
  

module Split =
  (struct

    (* BAND TRANSLATORS *)

    let encode: translator = fun x -> x

    let decode: translator = fun x -> x

    (* EMULATION OF A TRANSITION *)

    let just_read: reading -> Action.t = fun reading ->
      RWM (reading, No_Write, Here)

    let just_write: writing -> Action.t = fun writing ->
      match writing with
      | No_Write     -> RWM (Match(ANY), No_Write    , Here)
      | Write symbol -> RWM (Match(ANY), Write symbol, Here)

    let just_move: moving -> Action.t = fun moving ->
      RWM (Match(ANY), No_Write, moving)
      
    let just_move_TM: moving -> Turing_Machine.t = fun moving ->
        { Turing_Machine.nop with
          name = String.concat "" [ "MOVE(" ; Moving.to_ascii moving ; ")" ] ;
          transitions = [ (State.initial, Action (just_move moving), State.accept) ]
        }

      
    let synchronize_multiple_bands: Action.t list -> Instruction.t = fun actionS ->

      let rec (rec_synchronize: ('r list * 'w list * 'm list) -> Action.t list -> ('r list * 'w list * 'm list)) = fun (reads,writes,moves) actions ->
        match actions with
        | [] -> (List.rev reads, List.rev writes, List.rev moves)
        | action::actions ->
          (match action with
           | Nop        -> rec_synchronize ( Nop::reads , Nop::writes , Nop::moves) actions
           | RWM(r,w,m) -> rec_synchronize ( (just_read r)::reads , (just_write w)::writes , (just_move m)::moves) actions
           | Simultaneous _ -> failwith "Emulator.Split.synchronize: nested Simultaneous"
          )
          
      in let (reads,writes,moves) = rec_synchronize ([],[],[]) actionS
         in  Seq[ Action(Simultaneous(reads)) ; Action(Simultaneous(writes)) ; Action(Simultaneous(moves)) ]


    let rec emulate_action: emulator = fun (source,action,target) ->
      
      let transitions = generate_transitions_emulating (State.initial,action,State.accept) in
      { Turing_Machine.nop with
        name = String.concat " " [ "Splitting" ; Transition.to_ascii (source, Action action, target) ] ;
        transitions = transitions
      }

    and generate_transitions_emulating: State.t * Action.t * State.t -> Transition.t list = fun (source,action,target) ->

      match action with
      | Nop -> [ (source, Action(Nop), target) ]
             
      | RWM(r,w,m) -> [ (source, Seq[ Action(just_read r) ; Action(just_write w) ; Action(just_move m) ], target) ]
                    
      | Simultaneous actions -> [ (source, synchronize_multiple_bands actions, target) ]

                               
    (* THE SIMULATOR *)

    let (* USER *) (simulator: simulator) = { name = "Split" ; encoder = encode ;  decoder = decode ; emulator = emulate_action }

  end)

  

 
(** The BitVector Emulator replaces symbols by bit vectors.
    It is not a real translation to a binary Turing Machine since a bit vector is treated as a single symbol.       
    Its only purpose is to show the binary encoding of bands.
 *)
  
module BitVector_Emulator = struct
  
  (* TRANSLATION OF BANDS *)

  (* modules Bit and Bits are defined in Alphabet.ml *)
    
  open Alphabet
       
  type encoding = (Symbol.t * Bits.t) list
  type decoding = (Bits.t * Symbol.t) list

  let build_encoding : Alphabet.t -> encoding = fun alphabet ->
    let size = List.length alphabet.symbols in
    let bitvectors = if size=1 then [ [Bit.unit] ] else Bits.enumerate size
    in (MyList.zip alphabet.symbols bitvectors) 

  let reverse : encoding -> decoding = fun assocs ->
    List.map (fun (s,b) -> (b,s)) assocs

  let encode_symbol_wrt : encoding -> Symbol.t -> Symbol.t = fun encoding symbol ->
    match symbol with
    | B -> B
    | _ -> Vector (List.assoc symbol encoding)
    
  let encode_wrt : encoding -> Band.t list -> Band.t list = fun encoding bands ->
    List.map
      (Band.map (encode_symbol_wrt encoding))
      bands 

  (* REVERSE TRANSLATION *)

  let decode_symbol_wrt : decoding -> Symbol.t -> Symbol.t = fun decoding symbol ->
    match symbol with
    | B -> B
    | Vector(bits) -> List.assoc bits decoding
    
  let decode_wrt : decoding -> Band.t list -> Band.t list = fun decoding bands ->
    List.map
      (Band.map (decode_symbol_wrt decoding))
      bands


  (* EMULATION OF TRANSITIONS *)

  let (emulate_action_wrt: encoding -> State.t * Action.t * State.t -> Turing_Machine.t) = fun encoding (source,action,target) ->
    let action = Action.map (encode_symbol_wrt encoding) action
    in
    { Turing_Machine.nop with
      name = String.concat "" [ "BitVector" ; Pretty.parentheses (Action.to_ascii action) ] ;
      transitions = [ (State.initial, Action(action), State.accept) ]
    }
    
  (* THE SIMULATOR *)

  let make_simulator : Alphabet.t -> simulator = fun alphabet ->
    let encoding = build_encoding alphabet in
    let decoding = reverse encoding 
    in
    { name = "BitVector" ;
      encoder = encode_wrt encoding ;
      decoder = decode_wrt decoding ;
      emulator = emulate_action_wrt encoding
    }

end



                          
module Binary_Emulator = struct
  
  (* TRANSLATION OF BANDS *)
  
  (* modules Bit and Bits are defined in Alphabet.ml *)
  
  open Alphabet

  type encoding =
    { nb_bits: int ;
      assocs: (Symbol.t * Bits.t) list 
    }

  type decoding =
    { nb_bits: int ;
      assocs: (Bits.t * Symbol.t) list
    }
                   
  let build_encoding : Alphabet.t -> encoding = fun alphabet ->
    let size = List.length alphabet.symbols in
    let n = Bits.nb_bits_for size in
    let nxNil = MyList.make n B
    in 
    let bitvectors = if size=1 then [ [Bit.unit] ] else Bits.enumerate size
    in { nb_bits  = n ;
         assocs = (B,nxNil) :: (MyList.zip alphabet.symbols bitvectors)
       }

  let reverse : encoding -> decoding = fun encoding ->
    { nb_bits = encoding.nb_bits ;
      assocs = List.map (fun (symbol,bits) -> (bits,symbol)) encoding.assocs
    }
    
  let encode_symbol_wrt : encoding -> Symbol.t -> Symbol.t list = fun encoding symbol ->
    List.assoc symbol encoding.assocs
         
  let encode_wrt : encoding -> Band.t list -> Band.t list = fun encoding bands ->
    List.map
      (Band.map_concat (encode_symbol_wrt encoding))
      bands 

    
  (* REVERSE TRANSLATION *)
    
  let decode_symbol_wrt : decoding -> Symbol.t list -> Symbol.t option = fun decoding symbols ->
    try
      Some (List.assoc symbols decoding.assocs)
    with
      Not_found -> None

  let decode_wrt : decoding -> Band.t list -> Band.t list = fun decoding bands ->
    List.map
      (Band.apply (decode_symbol_wrt decoding))
      bands
    

  (** THE SIMULATOR *)

  (*** Emulating the reading of a binary word *)

  (*** The case of simple patterns recognizing anything or just one binary word *)
    
  let make_reading_SEQ: encoding -> State.t * State.t -> Bits.t pattern -> Transition.t list
    = fun encoding (source,target) word_pattern
    -> let moves  = (MyList.make (encoding.nb_bits -1) Right) @ [Here]
       and bit_patterns =
         match word_pattern with
         | ANY -> MyList.make encoding.nb_bits ANY
         | VAL word -> List.map (fun bit -> VAL(bit)) word
       in
       let reads = List.map
                     (fun (pattern,move) -> Action( RWM(Match(pattern), No_Write, move)) )
                     (MyList.zip bit_patterns moves)
       in [ (source, Seq(reads), target) ]


  (*** The case of complex patterns wich recognize of a set of words requires a TM built from a Binary Tree.
       The binary tree is used to represent the set of binary words and to share common prefixes *)
        
  module BTree = Binary_Tree.Made_Of(Bit)

  let make_node_from: State.t -> BTree.label -> State.t = fun state label ->
    let bit =
      match label with
      | BTree.Any     -> A
      | BTree.Bit bit -> bit
    in match state with
       | Q(i) -> Qs(i,[bit])
       | Qs(i,bits) -> Qs(i,bits@[bit])
       | Qc(s,ints) -> Qc(s,ints@[if bit=Bit.zero then 0 else if bit=Bit.unit then 1 else 2 ])
                     

  let make_reading_TM: encoding -> State.t * State.t -> Bits.t pattern -> Transition.t list 
    = fun encoding (source,target) word_pattern
    -> let (good,words) =
         match word_pattern with
         | IN words  -> (true , words )
         | BUT word  -> (false, [word])
         | OUT words -> (false, words )
       in
       let binary_tree = BTree.build_from words
       in let edges =
            if good
            then BTree.to_complete_graph_with make_node_from source target       State.reject binary_tree
            else BTree.to_complete_graph_with make_node_from source State.reject target       binary_tree
          in edges
             |> (List.map
                   (fun (src,label,tgt) ->
                     let move = if (tgt = target || tgt = State.reject) then Here else Right
                     and pattern = match label with
                       | BTree.Any     -> ANY
                       | BTree.Bit bit -> VAL(bit)
                     in let action = RWM( Match(pattern), No_Write, move)
                        in (src, Action(action), tgt))
                )
               
  let (emulate_reading_wrt: encoding -> State.t * State.t -> reading -> Transition.t list) = fun encoding (source,target) ->
    function Match(pattern) ->
              let word_pattern = Pattern.map (encode_symbol_wrt encoding) pattern
              in match word_pattern with
                 | ANY | VAL _
                   -> make_reading_SEQ encoding (source,target) word_pattern
                 | IN _ | BUT _ | OUT _
                   -> make_reading_TM  encoding (source,target) word_pattern


  let (just_move: moving  -> Instruction.t) = fun moving -> Action(RWM(Match(ANY),No_Write,moving))

       
  let (emulate_action_wrt: encoding -> State.t * Action.t * State.t -> Turing_Machine.t) = fun encoding (_,action,_) ->
    let istate = State.next_from State.initial
    in match action with 
       | RWM(reading,writing,moving)
         ->
          let writes =
            match writing with
            | No_Write -> (* When must yet replace the head *)
               MyList.make (encoding.nb_bits -1) (just_move Left)

            | Write(symbol) -> (* In order to reduce the number of head moves, bits are written in opposite ordrer from right to left *)
               let write_symbols = (encode_symbol_wrt encoding symbol) |> List.rev
               and write_moves   = (MyList.make (encoding.nb_bits -1) Left ) @ [Here]
               in List.map
                    (fun (symbol,move) -> Action(RWM(Match(ANY),Write(symbol),move)))
                    (MyList.zip write_symbols write_moves)
          and moves = match moving with
            | Here -> []
            | _ -> MyList.make encoding.nb_bits (just_move moving)
          in 
          let read_transitions = emulate_reading_wrt encoding (State.initial,istate) reading
          and write_move_transition =  (istate, Seq (writes @ moves), State.accept)
          in
          { Turing_Machine.nop with transitions = read_transitions @ [ write_move_transition ] }

          
  let make_simulator : Alphabet.t -> simulator = fun alphabet ->
    let encoding = build_encoding alphabet in
    let decoding = reverse encoding 
    in
    { name = "Binary" ;
      encoder = encode_wrt encoding ;
      decoder = decode_wrt decoding ;
      emulator = emulate_action_wrt encoding 
    }

end
                          

                       
(* DEMO *)

open Alphabet


let demo4: unit -> unit = fun () ->
  let loggers = [] 
  and alphabet = Alphabet.make [O;Z;S;U;C;A;T]
  in let emulators =
       [
         Binary_Emulator.make_simulator alphabet
       ]
     in List.iter (fun cfg -> let _ = Simulator.log_run_using (emulators,loggers) cfg in ())
          [
            Configuration.make (TM_Basic.test_TM01)
              [ Band.make "Data" alphabet [O;Z;Z;S;Z;U;S;U;Z;S;U;U;C] ]
          ;
            Configuration.make (TM_Basic.test_TM02)
              [ Band.make "Data" alphabet [O;A;T;S;Z;U;S;U;Z;S;U;U;C] ]

          ]

      
let demo3: unit -> unit = fun () ->
  let loggers = [] 
  and alphabet = Alphabet.full
  in let emulators =
       [
         Binary_Emulator.make_simulator alphabet
       ]
     in List.iter (fun cfg -> let _ = Simulator.log_run_using (emulators,loggers) cfg in ())
          [
            Configuration.make (TM_Basic.generic_dec alphabet.symbols)
              [ Band.make "Data" alphabet [F;L;U;T;Exc;D;Z;U;T;D;Acc;L;O;R;S] ]
          ]


let demo2: unit -> unit = fun () ->
  let loggers = [] 
  and alphabet4 = Alphabet.make [I 1;I 2;I 3;I 4]
  in let emulators =
       [
         (* BitVector_Emulator.make_simulator alphabet4 *)
         Binary_Emulator.make_simulator alphabet4
       ]
     in List.iter (fun cfg -> let _ = Simulator.log_run_using (emulators,loggers) cfg in ())
          [
            Configuration.make (TM_Basic.permut alphabet4) [ Band.make "Data" alphabet4 alphabet4.symbols ] 
          ]

  
let demo1: unit -> unit = fun () ->
  let loggers = []
  and alphabet2 = Alphabet.make [Z;U] 
  in let emulators =
       [ (* Trace.simulator *)
         (* Split.simulator *)
         BitVector_Emulator.make_simulator alphabet2 
       ]
     in List.iter (fun cfg -> let _ = Simulator.log_run_using (emulators,loggers) cfg in ())
          [
            Configuration.make TM_Basic.neg  [ Band.make "Data" alphabet2 [Z;Z;Z;U] ] ;
            Configuration.make TM_Basic.incr [ Band.make "Data" alphabet2 [U;U;Z;U] ]
          ]


let demo: unit -> unit = fun () ->
  begin
    (* demo1() ; *)
    (* demo2() ;  Binary_Emulator IS UNDER TESTING *)
    (* demo3() ; *)
    demo4()
  end
