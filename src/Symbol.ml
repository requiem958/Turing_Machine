(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT 
 *
 *   Symbols of the alphabet used by the TM
 *
 * USAGE
 *
 *   Requirement
 *    - Module  :  MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo
 *    - Library :  
 *   Compilation:  ocamlc      MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.ml
 *   Interpreter:  ledit ocaml MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Graphics.cma Symbol.cmo
 *
 *)


type symbol =
  | B (* Blank *)
  | D (* Dark  *)

  | U (* the 1 bit *)
  | Z (* the 0 bit *)

  | S (* a separator *)

  (* LAMBDA-CALCULUS: additional symbols for simulating the beta-reducton with MT, required by LC_by_MT *)

 (* L    lambda *)
  | O (* the opening parenthesis *)
  | C (* the closing parenthesis *)
  | X (* the variable symbol followed by a identifier as a sequence of bits *)

  | V of (string * int) (* /!\ a infinite number of symbols: This is cheating, but convenient *)

  (* For simulating a Turing Machine (operating on a given alphabet) by a Binary Turing Machine *)

  | Vector of symbols

  (* For simulation of a k-Bands Turing Machine by a One-Band Turing Machine *)

  | Column of symbols

                (* UNIVERSAL TURING MACHINE: see Universal/UTM.ml *)
            
  | Std  (* standard state *)
  | Acc  (* accepting state *)
  | Exc  (* exception state *)

  | L    (* Left  move *)
  | H    (* Here  move *)
  | R    (* Right move *)

  
  
 

and symbols = symbol list



module Symbol =
  (struct

    type t = symbol

    let (compare: symbol -> symbol -> int) = Pervasives.compare

    (* PRETTY PRINTING *)

    let rec verbatim : t -> string
      = function
        | B -> "B"
        | D -> "D"
        | U -> "U"
        | Z -> "Z"
        | S -> "S"

        | L -> "L"
        | O -> "O"
        | C -> "C"
        | X -> "X"

        | V(string,int) -> String.concat "" [ "V" ; Pretty.parentheses (string ^ "," ^ (string_of_int int)) ]
        | Vector symbols  -> String.concat "" [ "Vector" ; Pretty.bracket (String.concat ";" (List.map verbatim symbols)) ]
        | Column symbols  -> String.concat "" [ "Column" ; Pretty.bracket (String.concat ";" (List.map verbatim symbols)) ]

        | Std -> "Std"
        | Acc -> "Acc"
        | Exc -> "Exc"
        | H -> "H"
        | R -> "R"
          
                           
    (* ascii output *)

    let rec to_ascii : symbol -> string
      = function
        | B -> "_"
        | D -> "$"

        | U -> "U"
        | Z -> "Z"

        | S -> "#"

        | L -> "L"
        | O -> "("
        | C -> ")"
        | X -> "x"
        | V(string,int) -> string ^ (if int<0 then "" else string_of_int int)

        | Vector symbols -> Pretty.parentheses (String.concat "," (List.map to_ascii symbols))

        | Column symbols -> Pretty.bracket (String.concat "|" (List.map to_ascii symbols))

        | symbol -> verbatim symbol
        

    (* html output *)

    let color : symbol -> Color.t * Color.t
      = fun symbol ->
        match symbol with (* back_ground_color, font_color *)
        | B -> (Color.white , Color.white)
        | Z -> (Color.blue  , Color.yellow)
        | U -> (Color.yellow, Color.blue)
        | D -> (Color.black , Color.black)
        | S -> (Color.red   , Color.white)
        | _ -> (Color.white , Color.black)

    let ft_color : symbol -> Color.t = fun symbol -> snd (color symbol)
    let bg_color : symbol -> Color.t = fun symbol -> fst (color symbol)


    let rec to_html : Html.options -> symbol -> Html.cell
      = fun options symbol ->
        match symbol with
        | Vector symbols -> Html.tuple  options (List.map (to_html []) symbols)
        | Column symbols -> Html.column options (List.map (to_html []) symbols)
        | _ ->
          Html.cell [ ("align", Html.Option "center") ; ("bgcolor", Html.Color (bg_color symbol)) ]
            (Html.font [ ("color", Html.Color (ft_color symbol)) ]
               (Html.bold (to_ascii symbol)))


    (* user *)

    let pretty (*USER*) : t -> string
      = fun t ->
        match Pretty.get_format() with
        | Pretty.Html  -> to_html [] t
        | Pretty.Ascii -> to_ascii t

  end)
