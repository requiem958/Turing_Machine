(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT 
 *
 *   Basic actions of Turing Machines
 *
 * USAGE
 *
 *   Requirement
 *   - Module   :  MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Pattern.cmo Band.cmo
 *   - Library  :  
 *   Compilation:  ocamlc      MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Pattern.cmo Band.cmo Action.ml
 *   Interpreter:  ledit ocaml MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Pattern.cmo Band.cmo Action.cmo 
 *
 *)


open Symbol
open Band
open Pattern
  
  
type reading = Match of Symbol.t pattern

type writing =
  | No_Write
  | Write of Symbol.t

type moving  = Left | Here | Right

type action =
  | RWM of (reading * writing * moving)
  | Simultaneous of action list (* simultaneous actions on multiple bands *)
  | Nop (* no operation *)

      
module Reading =
  (struct
    type t = reading
	  
    let to_ascii: t -> string = function
      | Match(pattern) -> Pattern.to_ascii_wrt Symbol.to_ascii pattern
                 
    let map: (Symbol.t -> Symbol.t) -> t -> t = fun f reading ->
      match reading with
      | Match pattern -> Match (Pattern.map f pattern)

  end)


module Writing =
  (struct
    type t = writing
	  
    let (to_ascii: t -> string) = function
      | No_Write -> ""
      | Write symbol -> Symbol.to_ascii symbol

    let (map: (Symbol.t -> Symbol.t) -> t -> t) = fun f writing ->
	  match writing with
	  | No_Write -> No_Write
	  | Write symbol -> Write (f symbol)
		
  end)

    
module Moving =
  (struct
    type t = moving
	  
    let (to_ascii: t -> string) = function
      | Left  -> "L"
      | Here  -> "H"
      | Right -> "R"
		
  end)

    
	  
module Action =
  (struct

    type t = action

    let zip: action list -> Band.t list -> (action * Band.t) list =  Band.zip_complete_with Nop ;;

    (* INFORMATION on READING PATTERN of STANDARD 1 BAND TURING MACHINE *)

    let get_pattern: action -> Symbol.t Pattern.t = function
      | Nop -> ANY
      | RWM(Match(pattern),_,_) -> pattern
                          

    (* ACTION ID used for creating UNIQUE DOT node *)

    let rec id : action -> string = function
      | Nop -> "Nop"
      | RWM(reading,_,_) -> Reading.to_ascii reading
      | Simultaneous actions ->  String.concat "&" (List.map id actions)
          
    (* ENABLED ONE ACTION on ONE BAND *)	

    let (is_enabled_on_this: Band.t -> action -> bool) = fun band action ->
      match action with
      | Nop -> true
      | RWM (Match(pattern),_,_) -> Pattern.matches pattern band.head                                  


    (* ENABLED COMPLEX ACTION on MULTIPLE BANDS *)
                                  
    let rec (is_enabled_on: Band.t list -> action  -> bool) = fun bands action ->
      (bands <> [])
      &&
        (let actions =
           match action with 
           | Nop -> []
           | RWM _ -> [action]
           | Simultaneous actions -> actions (* one-band actions *)
         in
         List.for_all
	   (fun (action,band) -> is_enabled_on_this band action)
           (zip actions bands) 
        )
	    
    (* PERFORMING an action on ONE BAND *)
	    
    let (do_move: moving -> Band.t -> Band.t) = fun moving band ->
	  match moving with
	  | Left  -> Band.move_head_left  band
	  | Right -> Band.move_head_right band
	  | Here  -> band
		    
    let (do_write: writing -> Band.t -> Band.t) = fun writing band ->
	  match writing with
	  | Write s -> Band.write s band
	  | _       -> band

    let rec (perform_on_one_band: action -> Band.t -> Band.t) = fun action band ->
	  match action with
	  | Nop -> band
	  | RWM (_,writing,moving) -> band |> (do_write writing) |> (do_move moving)

                                    
    (* PERFORMING INDEXED ACTIONS on MULTIPLE BANDS *)
                                    
    let (perform:  action -> Band.t list -> Band.t list) = fun action bands ->
      let actions =
      	match action with
	  | Nop -> []
          | RWM _ -> [action]
	  | Simultaneous actions -> actions
      in             
      List.map
	(fun (action,band) -> perform_on_one_band action band)
	(zip actions bands)

               
    (* PRETTY PRINTING *)

    let rec to_ascii: t -> string = function
      | Nop -> "Nop"
      | RWM(reading,writing,moving) ->
	      let r = Reading.to_ascii reading
	      and w = Writing.to_ascii writing
	      and m = Moving.to_ascii moving 
	      in String.concat "" [ r ; if w="" then "" else "\\"^w ; ":"^m ]
		  
      | Simultaneous actions -> Pretty.brace (String.concat "," (List.map to_ascii actions))

    let rec to_dot: t -> string = function
      | Nop -> "Nop"
      | RWM(reading,writing,moving) ->
	      let r = Reading.to_ascii reading
	      and w = Writing.to_ascii writing
	      and m = Moving.to_ascii moving 
	      in String.concat "" [ r ; if w="" then "" else "/"^w ; ":"^m ]

      | Simultaneous actions -> String.concat " & " (List.map to_dot actions)

                              
    (* USER *)
      
    let (pretty: t -> string) = fun t ->
	  match Pretty.get_format() with
	  | Pretty.Html  
	  | Pretty.Ascii -> to_ascii t


    (* MAP *)
		    
    let rec (map: (Symbol.t -> Symbol.t) -> t -> t) = fun f action ->
	  match action with
	  | Nop -> Nop
	  | Simultaneous actions -> Simultaneous (List.map (map f) actions)
	  | RWM(r,w,m) -> RWM(Reading.map f r, Writing.map f w, m)

end)


    
    
(*
    let (write_on_ith_band: symbol -> int -> bands -> bands) = fun symbol ith bands ->
	  let (before, band_i, after) = MyList.split_at (ith-1) bands
	  in before @ [ { band_i with head = symbol } ] @ after
 *)		
	    
(* information 

    let (alphabets_of: action -> Alphabet.t list) = fun action ->
	  let (get_alphabet: action -> Alphabet.t) = fun action ->
		match action with
		| RWM (reading,writing,_) -> Alphabet.union (Reading.alphabet_of reading) (Writing.alphabet_of writing) 
		| Nop -> Alphabet.empty
	  in
	    match action with
	    | Simultaneous actions -> List.map get_alphabet actions
       |  _ -> [ get_alphabet action ]
   
*)
