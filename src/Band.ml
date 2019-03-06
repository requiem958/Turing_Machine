(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * CONTENT 
 *
 *   Representation of Bands of Turing Machines
 *
 * USAGE
 *
 *   Requirement
 *    - Module  :  MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo 
 *    - Library :
 *   Compilation:  ocamlc      MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Band.ml
 *   Interpreter:  ledit ocaml MyList.cmo MyString.cmo Tricks.cmo Pretty.cmo Color.cmo Html.cmo Symbol.cmo Bit_Vector.cmo Alphabet.cmo Band.cmo 
 *)

open Symbol
open Alphabet


type indexes = int list
   
type band =
  {
    name: string ;
    
    left: Symbol.t list ; head: Symbol.t ; right: Symbol.t list ;

    color: Color.t ;

    alphabet: Alphabet.t

    (* 
     * The alphabet is needed for translation into a binary representation of the symbols.
     * The alphabet is associated with the band (instead of being associated with the TM) for two reasons:
     *  1. outside the execution of a TM on the band, we would not know the alphabet and we would not be able to perform translation
     *  2. we will apply several TM in sequence on the band so they should agree and exchange the alphabet: complicated...
     *  3. Some of these TM can be generic, meaning that their instanciations depends on the alphabet
     *
     * Thus, it seems more convenient that the band sets the alphabet at creation.
     *)
  }

  
module Band =
  (struct

    type t = band

    let (empty: band) = { name = "" ; left = [] ; head = B ; right = [] ; color = Color.COL "LightGray" ; alphabet = Alphabet.empty }


    let (make: string -> Alphabet.t -> symbols -> band) = fun name alphabet symbols ->
	  match symbols with
	  | []    -> { empty with name = name ; alphabet = alphabet ; right = [] }
	  | s::ymbols -> { empty with name = name ; alphabet = alphabet ; head = s ; right = ymbols }


    let (nb_cells: band -> int * int) = fun band -> (List.length band.left, List.length band.right)

    let (map: (Symbol.t -> Symbol.t) -> t -> t) = fun f band ->
       { band with left = List.map f band.left ; head = f band.head ; right = List.map f band.right }


    (* /!\ The left part of the band is written in the reverse ordrer. It is easier to implement this way.
     *  A band containing  a b c d (e) f g h with the head on (e) will be encoded by
     *    { left = [d;c;b;a] ; head = e ; right = [f;g;h] }
     *)

    let (move_head_right: band -> band) = fun band ->
	  match band.right with
	  | []    -> { band with left = band.head::band.left ; head = B ; right = [] }
	  | s::ymbols -> { band with left = band.head::band.left ; head = s ; right = ymbols }

    let (move_head_left: band -> band) = fun band ->
	  match band.left with
	  | []    -> { band with left = [] ; head = B ; right = band.head::band.right }
	  | s::lobmys -> { band with left = lobmys ; head = s ; right = band.head::band.right }


    let (write: symbol -> band -> band) = fun symbol band ->
	  { band with head = symbol }


    (* FIXME 2019 : Translation to binary : TODO in module Emulator.ml using module Binary

    let (translate_to_binary: band -> band) = fun band ->
	  let map = Binary.create_binary_map band.alphabet
	  in Binary.translate_using map band
     *)

        
    (* ASSIGNMENT of OPERATIONS to BANDS *)    
        
    (* This assignement algorithm uses the indexes of active_bands to associate actions to bands and nop to unused bands 
     * /!\ The ordering of bands must be preserved for futrhter use of List.map on bands
     *
     *  For instance,
     *    assign_wrt NOP [3 ; 1] [action_a ; action_b ] [B1;B2;B3] --> [(action_b,B1) ; (NOP,B2) ; (action_a,B3)]
     *)
                                    
    let assign_indexed_operation_with_default: 'operation -> indexes -> 'operation list -> band list -> ('operation * band) list = fun nop indexes operations bands ->
      let indexed_operations =
        MyList.zip indexes operations
      in List.mapi
           (fun i band ->
             let index = i+1 in
             let operation = try List.assoc index indexed_operations with Not_found -> nop (* no operation *)
             in (operation,band)
           )
           bands

    (* OLD, USELESS ?
    let rec (zip_complete_with: 'op -> 'op list -> band list -> ('op * band) list) = fun nop operations bands ->
      match operations,bands with
      | [], [] -> []
      | o::operations, b::bands -> (o,b) :: (zip_complete_with nop operations bands)
      | [], b::bands -> (nop,b) :: (zip_complete_with nop [] bands)
      | _, [] -> failwith "Band.zip: missing band"

    let (zip_action_band: action list -> Band.t list -> (action * Band.t) list) =  Band.zip_complete_with Nop ;;
     *)


    (* SELECT *)

    let select: indexes -> band list -> band list = fun indexes bands ->
      List.map (fun i -> List.nth bands i) indexes
      

    (* EQUIVALENCE *)

    let rec remove_left_blanks: Symbol.t list -> Symbol.t list = fun symbols ->
	  match symbols with
	  | [] -> []
	  | s::ymbols -> if s=B then remove_left_blanks ymbols else symbols

    let rec (remove_right_blanks: Symbol.t list -> Symbol.t list) = fun symbols ->
      symbols
      |> List.rev
      |> remove_left_blanks
      |> List.rev

    let (symbols_of: band -> Symbol.t list) = fun band ->
      let right = remove_right_blanks (band.head :: band.right)
      and left  = remove_left_blanks  (List.rev band.left)
      in left @ right

    let (equivalent: band -> band -> bool) = fun band1 band2 ->
	  (symbols_of band1) = (symbols_of band2)


    (* PRETTY PRINTING *)

    (* ascii *)

    let (to_ascii: band -> string) = fun band ->
	  let strings =
	    List.map Symbol.to_ascii (List.rev band.left)
	    @
	      [ Pretty.ascii_underline (Pretty.ascii_green (Symbol.to_ascii band.head))  ]
	    @
	      List.map Symbol.to_ascii (band.right)
	  in
             String.concat "|" strings

    let (to_ascii_many: band list -> string) = fun bands ->
	  String.concat "\n" (List.map to_ascii bands)


    (* html *)

    let (cell_to_html: band -> symbol -> Html.cell) = fun band symbol  ->
	  Symbol.to_html [ ("colspan", Html.Int band.alphabet.symbol_size_in_bits) ] symbol


    let (head_to_html: band -> symbol -> Html.cell) = fun band symbol ->
	  Html.cell []
	    (Html.table [("bordercolor", Html.Color Color.green)]
	       [ Html.row [] [ cell_to_html band band.head ] ]
	    )

    let (name_to_html: band -> Html.cell) = fun band ->
	  Html.cell []
	    (Html.table [("bordercolor", Html.Color Color.gray) ; ("border", Html.Int 1) ;  ("cellpadding",Html.Int 4) ]
	       [Html.row []
                  [Html.cell
    	             [("align", Html.Option "center") ]
                     (Html.font [ ("color", Html.Color Color.gray) ; ("size", Html.Int 2) ] ("&nbsp;&nbsp;" ^band.name ^ "&nbsp;&nbsp;"))
            ]])

        
    let to_html: Html.options -> band -> Html.row = fun options band ->
      let cells =
        [ name_to_html band ] 
        @
	  (List.map (cell_to_html band) (List.rev band.left))
	@
	  [ head_to_html band band.head ]
	@
	  (List.map (cell_to_html band) (band.right))
      in
      Html.row options cells

    let to_html_many: Html.options -> band list -> indexes -> Html.table = fun options bands active_bands ->
      let
        highlight_band: int -> band -> Html.row = fun i ->
        if List.mem i active_bands
        then (to_html [])
        else (to_html [("bgcolor", Html.Color Color.gray)])
      and
        focus_on_band: int -> band -> Html.row = fun i band ->
        if List.mem i active_bands
        then Html.nil
        else (to_html [] band)
      in
      let rows = List.mapi (fun i -> highlight_band (i+1)) bands
      in
      Html.table
	(options @ [ ("bordercolor", Html.Color Color.white) ; ("cellpadding",Html.Int 1) ; ("cellspacing",Html.Int 1) ; ("border",Html.Int 1) ])
	rows

    (* user *)

    let pretty (*USER*) : t -> string = fun t ->
	  match Pretty.get_format() with
	  | Pretty.Html  -> to_html [] t
	  | Pretty.Ascii -> to_ascii t

end)


(* Example of html produced by translation of a Band [A;B] into [ Tuple[1;0;0;1] ; Tuple[0;1;1;0] ] then into Band [1;0;0;1;0;1;1;0]

<TABLE bordercolor=black cellpadding=1 cellspacing=0 border=1>

  <TR>
    <TD colspan=4 align=center>A</TD>
    <TD colspan=4 align=center>B</TD>
  </TR>

  <TR>
    <!-- A = Tuple[1;0;0;1] -->
    <TD COLSPAN=4>
      <TABLE bordercolor=gray>
	<TR>
	  <TD>1</TD>
	  <TD>0</TD>
	  <TD>0</TD>
	  <TD>1</TD>
	</TR>
      </TABLE>
    </TD>

    <!-- B = Tuple[0;1;1;0] -->
    <TD COLSPAN=4>
      <TABLE bordercolor=gray>
	<TR>
	  <TD>1</TD>
	  <TD>0</TD>
	  <TD>0</TD>
	  <TD>1</TD>
	</TR>
      </TABLE>
    </TD>

  </TR>

  <TR>
    <TD>1</TD>
    <TD>0</TD>
    <TD align=center><TABLE bordercolor=green><TR><TD>0</TD></TR></TABLE></TD>
    <TD>1</TD>

    <TD align=center>0</TD>
    <TD>1</TD>
    <TD>1</TD>
    <TD>0</TD>
  </TR>

</TABLE>
*)
