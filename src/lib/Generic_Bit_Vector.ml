(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * A implementation of operation on bit vectors parameterized by the representation of bits 
 * 
 * - avoid functor 
 *   BUT 
 * - Each function takes as a first argument the chosen representation of bits
 *
 * Examples of possible representation of bits :
 *   { zero = false ; unit = true } 
 *   { zero = 0     ; unit = 1    } 
 *   { zero = Z     ; unit = U    } 
 *)

(* USAGE:
 - Required:
 - Compile : ocamlc Generic_Bit_Vector.ml
*)

  
(* REPRESENTATION OF THE TWO BITS *)  
 
type 'bit zero_unit = { zero: 'bit ; unit: 'bit } 
    

      
module Generic =
  struct
 
    type 'bit vector = 'bit list

    let (pretty_bit: 'bit zero_unit -> 'bit -> string) = fun bit ->
	  fun b ->
		string_of_int (if b = bit.zero then 0 else 1)
	  
    let (pretty: 'bit zero_unit -> 'bit vector -> string) = fun bit ->
	  fun bits ->
		String.concat "" (List.map (pretty_bit bit) bits)

    let (print: 'bit zero_unit -> 'bit vector -> unit) = fun bit ->
	  fun bits ->
		print_string (pretty bit bits)
	
   (* CONVERSION int <-> bit vector (Little endian encoding: the less significant bit is on the left) *) 
      
    let rec (int_to_bits: 'bit zero_unit -> int -> 'bit vector) = fun bit ->
	  function
	    | 0 -> [ bit.zero ]
	    | 1 -> [ bit.unit ]
	    | i ->
		    let r = i mod 2
		    in (if r=0 then bit.zero else bit.unit) :: (int_to_bits bit ((i-r)/2))
							    
							    
    let (bits_to_int: 'bit zero_unit -> 'bit vector -> int option) = fun bit ->
	  fun bits ->
		let rec
		    (horner: int -> 'bit vector -> int) = fun int ->
			  function
			    | [] -> int
			    | b::bits ->
				    if b = bit.zero
				    then horner  (2 * int)    bits
				    else horner ((2 * int)+1) bits
	    in
		  match bits with
		  | [] -> None
		  | _  -> Some (horner 0 (List.rev bits))
			
    let (unsafe_bits_to_int: 'bit zero_unit -> 'bit vector -> int) = fun bit ->
	  fun bits ->
		match (bits_to_int bit bits) with
		| Some int -> int 
		| None -> assert (false)
		  

   (* OPERATIONS on bit vector *)

   (* The operation +1 *)
		  
    let rec (inc: 'bit zero_unit -> 'bit vector -> 'bit vector) = fun bit ->
	  fun bits ->
		match bits with
		| [b] -> if b=bit.zero then [bit.unit] else [bit.unit ; bit.zero]
		| b::bits ->
			if b = bit.zero
			then bit.unit :: bits
			else bit.zero :: (inc bit bits)
		| [] -> assert (false)
			  
   (* The operation *2 *)
				
    let (double: 'bit zero_unit -> 'bit vector -> 'bit vector) = fun bit ->
	  fun bits ->
		bit.zero :: bits

   (* The operation /2 *)
		 
    let (half: 'bit zero_unit -> 'bit vector -> 'bit vector) = fun bit ->
	  fun bits ->
		List.tl bits


   (* demo *)
  
    let (>>) x f = f x
	    
    let (demo: 'bit zero_unit -> unit -> unit) = fun bit ->
	  fun () ->
		[0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16]  
		  >> (List.map
			(fun int ->
			      [ int_to_bits bit (unsafe_bits_to_int bit (int_to_bits bit int)) ;
				inc bit (int_to_bits bit int) ;
				double bit (inc bit (int_to_bits bit int)) ;
				half bit (double bit (inc bit (int_to_bits bit int))) 
			      ]
				>> (List.map (pretty bit))
				>> (String.concat ";")
			))
		  >> (String.concat "\n")
		  >> print_string
  end
  

    
module type Two_Values_MT =
  sig
    type t 
    val zero:t
    val unit:t
  end

    
module Made_Of = functor ( Bit : Two_Values_MT) ->
 struct

   let zu = { zero = Bit.zero ; unit = Bit.unit }
   
   let pretty_bit = Generic.pretty_bit zu
   let pretty = Generic.pretty zu
   let print = Generic.print zu
   let int_to_bits = Generic.int_to_bits zu
   let bits_to_int = Generic.bits_to_int zu
   let unsafe_bits_to_int = Generic.unsafe_bits_to_int zu
       
 end


	
