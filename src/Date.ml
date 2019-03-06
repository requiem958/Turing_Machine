(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
 *
 * INSPIRED from 
 *
 *   PLEAC-Objective CAML
 *
 * CONTENT 
 *
 *   print Unix time
 *
 * USAGE
 *
 *   Require    :  unix.cma 
 *   Compilation:  ocamlc unix.cma Date.ml
 *   Interpreter:  ledit ocaml unix.cma Date.cmo
 *
 * DEMO
 *
 *   See the Turing Machine project, the Automata project
 *)


open Unix

let sleep_for ~(sec:float) =
  ignore (Unix.select [] [] [] sec)


let (pretty_time: unit -> string) = fun () ->
      let sec:float = Unix.gettimeofday() in
	let tm: Unix.tm = Unix.localtime sec in
	  let (ms:int) = Pervasives.truncate (1000. *. (sec -. (floor sec))) in
	    (Printf.sprintf "%04d-%02d-%02d-%02d:%02d:%02d.%03d"
	       (tm.tm_year + 1900)
	       tm.tm_mon
	       tm.tm_mday
	       tm.tm_hour
	       tm.tm_min
	       tm.tm_sec
	       ms
	    )
