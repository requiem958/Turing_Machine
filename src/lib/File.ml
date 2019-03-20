(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Mars 2017
 *
 * CONTENT 
 *
 * writing a string into a file
 *
 *)


type name_ext = string * string

let make: string -> string -> name_ext = fun name ext -> (name,ext)

let get_name: name_ext -> string = fst
let get_ext:  name_ext -> string = snd

let from_string: string -> name_ext = fun string ->
  let (txe, eman) = (MyString.string_to_list string) |> List.rev |> (MyList.split_on ".")
  in let ext  = (List.rev txe) |> MyString.list_to_string
     and name = (List.rev eman) |> MyString.list_to_string
     in (name,ext)

let to_string: name_ext -> string = fun (name,ext) ->
  if ext = "" then name else name ^ "." ^ ext


let i_output_in: name_ext -> string -> string = fun (name,ext) string ->
  let filename = to_string (name,ext)
  in let
      channel_out = open_out filename
    in
    begin
      print_string (".....data written in: " ^ filename ^ "\n");
      output_string channel_out string ;
      close_out channel_out ;
      filename
    end
