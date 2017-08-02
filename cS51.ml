(*
        General utilities for CS51
            Stuart M. Shieber
    Some utilities of general applicability for CS51.

    Note that some of these may not work on Windows systems, as they
    do not provide full support for Sys and Unix modules. Conditional
    execution could be added using Sys.os_type.  
 *)


(*----------------------------------------------------------------------
  Functional utilities
 *)

(* id x -- The identity function. Returns x *)

let id x = x ;;

(* const x -- Returns the constant function returning x. *)

let const x _ = x ;;
  
(* reduce f list -- Applies f to the elements of list left-to-right
   (as in List.fold_left) using first element as initial. This is a
   traditional higher-order function, standard in the literature, but
   an oversight in not appearing in the Pervasives or List modules. *)

let reduce (f : 'a -> 'a -> 'a) (list : 'a list) : 'a = 
  match list with
  | head::tail -> List.fold_left f head tail
  | [] -> failwith "can't reduce empty list"
;;


(*----------------------------------------------------------------------
  Assertions and debugging
 *)

(* verify assertion format_string ... -- Verifies that the boolean
   assertion evaluates to true, continuing silently if so; if the
   assertion fails (evaluates to false) it prints the format_string,
   as per Printf.printf, which can reference further arguments as
   well. Example of usage:
        # let n = 5 in
          verify (n mod 2 = 0) "n is %d, but should be even\n" n ;;
        n is 5, but should be even
        - : unit = ()
 *)

let verify (condition : bool)
           (fmt : ('a, out_channel, unit) format)
         : 'a =
  if condition then Printf.ifprintf stdout fmt
  else Printf.printf fmt
;;

  
(*----------------------------------------------------------------------
  Performance monitoring
 *)

(* call_timed f x -- Applies f to x returning a pair of the result and
   the time in seconds to execute the function call. *)
  
let call_timed (f : 'a -> 'b) (x : 'a) : 'b * float =
  let t0 = Unix.gettimeofday() in 
  let result = f x in 
  let t1 = Unix.gettimeofday() in
  (result, t1 -. t0)
;;

(* call_reporting_time f x -- Applies f to x returning the result,
   reporting timing information on stdout as a side effect. *)
  
let call_reporting_time (f : 'a -> 'b) (x : 'a) : 'b =
  let result, time = call_timed f x in
  Printf.printf "time (msecs): %f\n" (time *. 1000.);
  result
;;