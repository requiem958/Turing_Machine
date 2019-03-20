(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT 
 *
 *   Representation of the states of a Turing Machine
 *
 * USAGE
 *
 *   Requirement
 *   - Module   :  MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo
 *   - Library  :  
 *   Compilation:  ocamlc      MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo State.ml
 *   Interpreter:  ledit ocaml MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo State.cmo
 *
 * DEMO
 *
 *   See the Turing Machine project, the Automata project
 *)


open Symbol
  
type state =
  | Q of int

 (* for storing symbol in a state *)
  | Qs of (int * Symbol.t list)
	
 (* for inlining of high-level construction such as SEQ or TM *)
  | Qc of (string * int list)

 (* Examples 
  *   M  =def= Q1 -SEQ[a;b;c]-> Q2  becomes  Qc("M",[1]) -a-> Qc("M",[1;0]) -b-> Qc("M",[1;1]) -c-> Qc("M",[2]) 
  *   M' =def= Q1 -TM(M)-> Q2       becomes  Qc("M",[1]) -a-> Qc("M",[1;0]) -b-> Qc("M",[1;1]) -c-> Qc("M",[2]) 	
  *)
	
    
module State =
  (struct
    
    type t = state

    (* CONVENTION *)
           
    let initial: state = Q(1)
    let accept:  state = Q(0)
    let reject:  state = Q(-1)	
	   
    (* AVOIDING CLASH BETWEEN STATES *)	

    let next_from: state -> state = function
      | Q int -> Q(int+1)
      | Qs(int,symbols) -> Qs(int+1, symbols)
      | Qc(string,ints) -> Qc(string, 1::ints)

    let fresh_from: state -> state = function
      | Q int -> Qs(int,[])
      | Qs(int,symbols) -> Qs(int, B::symbols)
      | Qc(string,ints) -> Qc(string, 0::ints)
	
    (* PRETTY PRINTING *)

    let rec to_ascii: state -> string = fun state ->
	  match state with
	  | Q(i) -> "Q" ^ (string_of_int i)
	  | Qs(i,symbols) -> String.concat "" [ "Q" ; string_of_int i ; Pretty.list Symbol.to_ascii symbols ]
	  | Qc(name,integers) ->
		  match integers with
		  | [] -> name
		  | i::integers ->
			  String.concat "."
			    ([ String.concat ":" [ name ; to_ascii (Q i) ] ]
			     @ (List.map string_of_int integers)
			    )

    let to_dot: state -> string = to_ascii
                                
    let to_html: Html.options -> state -> Html.content = fun options state ->
	  Html.cell
	    (options @ [("align", Html.Option "center")])
	    (to_ascii state)

        
        
    (* user *)
	    
    let pretty (*USER*) : t -> string =
      match Pretty.get_format() with
      | Pretty.Html  -> (to_html [])
      | Pretty.Ascii -> to_ascii
      | Pretty.Dot   -> to_dot

  end)
