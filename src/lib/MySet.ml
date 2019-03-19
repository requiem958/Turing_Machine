(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes
 *
 * CONTENT 
 *
 *   Naive implementation of Set operations
 *
 * USAGE
 *
 *   Require    :  MyList.cmo MyString.cmo Pretty.cmo
 *   Compilation:  ocamlc MyList.cmo MyString.cmo Pretty.cmo MySet.ml 
 *   Interpreter:  ledit ocaml MyList.cmo MyString.cmo Pretty.cmo MySet.cmo
 *)


type 'a set = 'a list
type 'a t = 'a set

let empty = []

let singleton: 't -> 't set = fun t -> [t]

let add: 't -> 't set -> 't set = fun e s ->
  if List.mem e s then s else e::s
  
let from_list: 't list -> 't set = fun l ->
  List.fold_left (fun s e -> add e s) empty l
  
let union: 't set -> 't set -> 't set = fun s1 s2 ->
  List.sort Pervasives.compare (List.fold_right add s1 (List.fold_right add s2 []))
  
let big_union: ('t set) set -> 't set = fun sets ->
  List.fold_right (fun s acc -> union s acc) sets []
  
let inter: 't set -> 't set -> 't set = fun s1 s2 ->
  List.filter (fun e1 -> List.mem e1 s2) s1
  
let minus: 't set -> 't set -> 't set = fun s1 s2 ->
  List.filter (fun e1 -> not (List.mem e1 s2)) s1
  
let intersect: 't set -> 't set -> bool = fun s1 s2 ->
  MyList.exists_in s1 (fun e1 -> List.mem e1 s2)
  
let subseteq: 't set -> 't set -> bool = fun s1 s2 ->
  List.for_all (fun e1 -> List.mem e1 s2) s1
  
let normalize_with: ('t -> 't -> int) -> 't set -> 't set = fun compare set ->
  List.sort compare set
  
let normalize: 't set -> 't set = fun set ->
  normalize_with Pervasives.compare set
  
let pretty_set: ('t -> string) -> 't set -> string = fun pretty_elt set ->
  let format = Pretty.get_format () in
  match format with
  | Pretty.Latex -> MyList.prettys "\\{" "," "\\}" pretty_elt set
  | _ -> MyList.prettys "{" "," "}" pretty_elt set 
       
       
type 't indexed = ('t * int) set
                
let pretty_indexed_set: ('t -> string) -> 't indexed -> string = fun pretty_elt indexed_set ->
  MyList.prettys "" " " ""
    (fun (elt,index) -> String.concat "=" [ string_of_int index  ; pretty_elt elt ])
    indexed_set
  
let foreach_in: 'x set -> ('x -> 'y set) -> 'y set = fun setX f ->
  List.fold_right (fun x setY -> union (f x) setY) setX empty
  
let rec powerset: 't set -> ('t set) set = fun ts ->
  match ts with
  | [] -> [[]]
  | t::ts -> let ps = powerset ts in ps @ (List.map (fun s -> t::s) ps)
                                   
                                   
(* Fixpoint *)
                                   
let rec accumulating_fixpoint
          (targets_from: 'src set -> 'tgt set)
          (sources_from: 'tgt set -> 'src set)
          (deja_vu     : 'a set)
          (sources     : 'a set)
          (targets     : 'a set)
        : 'a set
  =
  if sources = [] 
  then targets
  else
    let deja_vu' = union deja_vu sources
    in let new_targets = targets_from sources
       in let new_sources = sources_from new_targets
	  in let sources' = minus new_sources deja_vu'
	     in let targets' = union new_targets targets
	        in accumulating_fixpoint
                     targets_from
                     sources_from
                     deja_vu'
                     sources'
                     targets'
                     
