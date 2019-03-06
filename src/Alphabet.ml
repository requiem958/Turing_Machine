(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENTS 
 *
 *   - The alphabet of symbols used by a Turing Machine
 *   - Its conversion into vectors of bits for Turing Machines which operate on binary alphabet {B,D} 
 *     
 *     FIXME 2019: why not {Z,U} + Blank ?
 *     IMPORTANT : The default symbol of the band (the blank symbol B) must be one of the two bits
 *               I chose  B <-> 0 , D <-> 1
 *
 * USAGE
 *
 *   Requirement
 *   - Module   :  MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo
 *   - Library  :  
 *   Compilation:  ocamlc      MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.ml
 *   Interpreter:  ledit ocaml MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo
 *
 * DEMO
 *
 *   See the Turing Machine project, the Automata project
 *)


open Symbol


module Bit =
struct
  type t = symbol
  let zero :t = B
  let unit :t = D
  let pretty : t -> string = Symbol.pretty
end

module Bits = Bit_Vector.Made_Of(Bit)


type alphabet = { symbols: symbols ;
                  symbol_size_in_bits: int }

module Alphabet =
  (struct

    type t = alphabet

    let empty : alphabet = { symbols = [B] ; symbol_size_in_bits = 1 }

    let make : symbols -> alphabet
      = fun symbols ->
        let symbols = MyList.union [B] symbols
        in
        { symbols = symbols ;
          symbol_size_in_bits = Bits.nb_bits_for (List.length symbols)
        }

    let symbols_of : alphabet -> Symbol.t list
      = fun alphabet -> alphabet.symbols

    let baton : alphabet = make [Bit.unit]
                         
    let binary : alphabet = make [Bit.zero ; Bit.unit]

    let full : alphabet = make [D;U;Z;S;L;O;C;X]


  end)


(* DEMO *)

open Pretty
open Tricks (* provides >> *)

let _ = Pretty.print ~format:Ascii (fun _ -> (String.concat "\n" [ "\n\n* DEMO * Alphabet.ml:\n" ; (Bits.enumerate 8) >> Bits.prettys ]))
