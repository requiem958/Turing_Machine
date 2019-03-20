(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Mars 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT 
 *
 *   Outputs a control flow graph in a .dot format for Graphviz
 *
 * DEMO
 * 
 *   At the end of this file
 *)


type options = string list

type node = string
type label = string
type labelled_node = node * label
type transition = (node * label * node)

let dot_sequence: ('t -> string) -> 't list -> string = fun pretty ts ->
  String.concat " ;\n" (List.map pretty ts)
                
let dot_options : options -> string = fun options ->
  "[" ^ (String.concat "," options) ^ "]"
  
let dot_label: options -> label -> string = fun options label ->
  let label_def = "label=" ^ (MyString.string_in_string label)
  in  dot_options (label_def :: options)

let dot_node: node -> string = fun node ->
  MyString.string_in_string node

let dot_labelled_node: options -> (node * label) -> string = fun options (node,label) ->
  String.concat " " [ dot_node node ; dot_label options label ]

let dot_nolabel_node: node -> string = fun node ->
  String.concat " " [ dot_node node ; dot_label [] "" ]
 
let dot_transition: options -> transition -> string = fun options (source_node,label,target_node) ->
  String.concat ""
    [ dot_node (source_node)
    ; "->"
    ; dot_node (target_node)
    ; dot_label options label
    ] 
 
let digraph
      (name : string)
      (initial_states    : node list)
      (acception_states  : node list)
      (rejecting_states  : node list)
      (control_states    : node list)
      (instruction_nodes : (node * label) list)
      (transitions       : transition list)
  =
  String.concat "\n  "
    [ "digraph cfg{" 
    ; "node [shape=plaintext]; // MACHINE NAME" 
    ;    dot_labelled_node [ "fontname=comic" ; "fontsize=18" ] (name,name)
    ; "\n node [shape=circle]; // STATES" 
    ; "\n node [peripheries=1, style=bold]; // INITIAL STATES" 
    ;    dot_sequence dot_node initial_states
    ; "\n node [peripheries=2, style=solid , color=green, fontcolor=black]; // ACCEPTING STATES" 
    ;    dot_sequence dot_node acception_states
    ; "\n node [peripheries=1, style=filled, color=red  , fontcolor=white]; // REJECTING STATES" 
    ;    dot_sequence dot_node rejecting_states
    ; "\n node [shape=circle, peripheries=1, style=solid, color=black, fontcolor=blue]; // OTHER CONTROL STATES" 
    ;    dot_sequence dot_nolabel_node control_states
    ; "\n node [shape=box, peripheries=1] // INSTRUCTION NODES"
    ;    dot_sequence (dot_labelled_node []) instruction_nodes
    ; "// TRANSITIONS" 
    ;    dot_sequence (fun istate -> (dot_node name) ^ " -> " ^ (dot_node istate)) initial_states
    ;    dot_sequence (dot_transition []) transitions
    ; "}"
    ]
  

        
(* DANS TM 
  
let mk_transition: options -> Transition.t -> string = fun options (src,instruction,tgt) ->
  dot_transition
    options
    (State.to_dot src , Instruction.to_dot instruction , State.to_dot tgt)


 let i_output_to_dot_file_named (directory:string) (filename:string) (machine:t) : File.filename =
  File.output_in
    (directory ^ filename,"dot")
    (to_dot machine)

 let i_output_to_dot_file (directory:string) (machine:t) : File.filename =
  i_output_to_dot_file directory mahcine.name machine

  *)
