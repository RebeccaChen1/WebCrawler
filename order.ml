(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

Definitions for an order datatype used to compare values.
*)

(* The type ordering is used for comparison operations *)
type ordering = Less | Equal | Greater ;;

let string_compare x y =
  let i = String.compare x y in
  if i = 0 then Equal else if i < 0 then Less else Greater
;;

let int_compare x y =
  let i = x - y in
  if i = 0 then Equal else if i < 0 then Less else Greater
;;
