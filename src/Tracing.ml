(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 *  Tracing/logging function calls
 *
 * - Usage:  
 *    see  Lambda_Calcul.ml  for a use case of the Trace module
 *    see  Turing Machine project for an illustration of the Log class
 * 
 * - Required modules: Unix.cma -> Date.ml
 *
 * - Compilation: ocamlc Unix.cma Date.ml Tracing.ml 
 *)


(* DELAYING A COMPUTATION
 *
 *   Consider the computation (1+2) of type: int 
 *  then  (fun _ -> (1+2))  is a delayed computation of type: unit -> int 
 *)
type 'a delayed_computation = unit -> 'a      
(*
 *  It can be triggered later by calling it with the value () of type unit.
 *  Indeed,  (fun _ -> (1+2)) ()  beta-reduces to   (1+2)
 *)

type filename = string


module Indent =
  (struct
    
    let _INDENTATION_ = ref 0
	
    let (incr: unit -> unit) = fun () -> _INDENTATION_ := (!_INDENTATION_) + 2
    let (decr: unit -> unit) = fun () -> _INDENTATION_ := (!_INDENTATION_) - 2	
	
    let (newline: string -> string) = fun string ->
	  String.concat "" ["\n" ; String.make (!_INDENTATION_) ' ' ; string ]
  end)
    




(* LOGGER as object to allow multiple active logs in different files *)

(* USAGE:
 *
 *  let log = new logger (String.concat "_" [ Date.pretty_time() ; "file_name.ext" ]
 *  in begin log#print "string1" ; log#print "string2" ; log#close end
 *)
   
class logger = fun (opt_name: string option) ->
      object(self)

	val opt_filename: string option =
	  match opt_name with
	  | None -> None
	  | Some name ->
		  let path = "_log"
		  in  Some (String.concat "/"  [ path ; name ]) 

	val mutable channel: out_channel = stdout

	initializer
	  channel <-
	    (match opt_filename with
	    | None -> stdout
	    | Some filename -> open_out filename
	    )

	method print: string -> unit = fun string ->
	      output_string channel string
		  
	method newline: string -> unit = fun string -> self#print (Indent.newline string)
		
	method print_msg: string -> unit = fun string ->
	      print_string string 

	method close: unit =
	  begin
	    (match opt_filename with
	    | None -> ()
	    | Some filename -> self#print_msg ("\n\n.... data logged in: " ^ filename ^ "\n")
	    ) ;
	    close_out channel
	  end

      end

	
class logger_too_complicated = fun (name:string) ->
      object(self)
	  
	val mutable logname: string = name
	val mutable channel: out_channel = stdout
	  
	val mutable trace_is_on: bool = true

	method print: string -> unit = fun string ->
	      if not (trace_is_on) then ()
	      else output_string channel string
		  
	method print_msg: string -> unit = fun string ->
	      if not (trace_is_on) then ()
	      else print_string string 

	method trace_on:  unit = trace_is_on <- true 
	method trace_off: unit = trace_is_on <- false

	method log_on: unit =
	  begin
	    (* A different file at each activation ? 
	     * logname <- String.concat "_" [ "log" ; Date.pretty_time() ; name ] ;
	     * this.channel <- open_out filename 
	     *)
	    (* Same file but append output to the end *)
	    channel <- open_out_gen [Open_wronly; Open_creat] 0o666 logname ;
	    seek_out channel (out_channel_length channel) ;
	    self#trace_on ;
	    self#print_msg ("\nstart logging in: " ^ logname ^ "\n") ;
	  end
	    
	method log_off: unit =
	  begin
	    self#print_msg ("\n\n.... data logged in: " ^ logname ^ "\n") ;
	    self#trace_off ;
	    close_out channel
	  end

	method log_in: string -> unit = fun name ->
	      begin
		logname <- String.concat "" [ name ; "_log_" ; Date.pretty_time() ; Pretty.get_extension() ] ;
     		self#log_on
	      end
	    

      
	method newline: string -> unit = fun string -> self#print (Indent.newline string)

      end


type loggers = logger list
      
    
(* TRACING *)	
      
module Trace =
 (struct 

   let default:logger = new logger None
       
   let _TRACE_ON_ = ref (false)
       
   let _OUTPUT_CHANNEL_ = ref (stdout) (* Default is the standard output channel *)

   let _LOG_NAME_ = ref ("stdout")
       
   let (print_msg: string -> unit) = fun string ->
	 if not (!_TRACE_ON_) then ()
	 else print_string string 

(*	     
   let (print: string -> unit) = fun string ->
	 if not (!_TRACE_ON_) then ()
	 else output_string (!_OUTPUT_CHANNEL_) string


   let (print: loggers -> string -> unit) = fun loggers string ->
	 if loggers=[] then default#print string
	 else
	   List.iter (fun logger -> logger#print string) loggers 
*)	   

   let (pretty_print_wrt: ('data -> string) -> loggers -> 'data -> 'data) = fun pretty loggers data ->
     if not (!_TRACE_ON_) then data
     else
       begin
	 (let string = pretty data in
	   if loggers=[]
	   then default#print string
	   else List.iter (fun logger -> logger#print string) loggers
	 ) ;
	 data
       end

   let print ?loggers:(loggers=[]) (string:string) : unit =
     if not (!_TRACE_ON_) then ()
     else
       if loggers=[] then default#print string
       else
	 List.iter (fun logger -> logger#print string) loggers 

   (* Usage of optional argument:
    * (print "the message")  thus loggers=[] in this case
    * (print "the message" ~loggers:[log1;log2])
    *)
	     
   let (trace_on:  unit -> unit) = fun () -> _TRACE_ON_ := true 
   let (trace_off: unit -> unit) = fun () -> _TRACE_ON_ := false
  
   let (trace: (unit -> 'a) -> 'a) = fun delayed_computation ->
	 begin
	   trace_on() ; print "\n" ;
	   let result = delayed_computation() in begin print "\n" ; trace_off() ; result end
	 end
	   
   let (untrace: (unit -> 'a) -> 'a) = fun delayed_computation ->
	 begin
	   trace_off() ; let result = delayed_computation() in begin trace_on() ; result end
	 end
	   

   let (log_open: filename -> unit) = fun filename ->
	 begin
	   _LOG_NAME_ := filename ; 
	   _OUTPUT_CHANNEL_ := open_out filename ;
	   print_msg ("\nstart logging in: " ^ filename ^ "\n") ;
	 end
	   
   let (log_close: unit -> unit) = fun () -> 
       begin
	 close_out (!_OUTPUT_CHANNEL_) ;
	 print_msg ("\n.... data logged in: " ^ (!_LOG_NAME_) ^ "\n") ;
       end
       
   let (log_in: filename -> (unit -> 'a) -> 'a) = fun filename delayed_computation ->
	 if not (!_TRACE_ON_)
	 then delayed_computation()
	 else
	   begin
	     log_open filename ;
	     let result = trace delayed_computation in
	       begin
		 log_close() ;
		 result
	       end
	   end

   let (newline: string -> unit) = fun string -> print (Indent.newline string)
	   
   let (call: string list -> ('a -> string) -> (unit -> 'a) -> 'a) = fun strings pretty delayed_computation ->
	 begin
	   newline (String.concat " " strings) ;
	   Indent.incr();
	   let result = delayed_computation() in
	     begin
	       Indent.decr();
	       newline (String.concat " " [ "~~~>  " ; pretty result ]) ;
	       result
	     end
	 end		  

 end)

    


    
