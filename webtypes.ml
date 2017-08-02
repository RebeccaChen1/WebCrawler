(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

Type definitions and important modules used throughout this problem set. Defines

  -  a type "link" to represent an internet link, 
  -  a type "page" to represent a webpage, 
  -  a module LinkSet (created using the set functor you'll write in myset.ml)
     representing a set of links.

 *)

module M = Myset ;; 

(*----------------------------------------------------------------------
  Links
 *)
   
type link = { host : string ;  (* e.g., "www.eecs.harvard.edu" *)
              port : int ;     (* e.g., 80 *)
              path : string    (* e.g., "/~somebody/index.html" *)
            } ;;

(* string_of_link l -- Generates a string representation fo a link *)
let string_of_link (l : link) : string =
  if l.host = "" then "file://" ^ l.path
  else if l.port = 80 then l.host ^ ":" ^ l.path
  else l.host ^ ":" ^ (Bytes.create l.port) ^ l.path
;;

(* link_compare -- Compares two links in lexicographic order *)
let link_compare (x:link) (y:link) : Order.ordering =
  match Order.string_compare x.host y.host with
  | Equal ->
     (match Order.int_compare x.port y.port with
      | Equal -> 
          (match Order.string_compare x.path y.path with
           | Equal -> Equal
           | ans -> ans)
      | ans -> ans)
  | ans -> ans
;;
(*......................................................................
  LinkSet
 *)
  
module LinkSet = 
  M.Make (struct
    type t = link
    let compare = link_compare
    let string_of_t = string_of_link
    let gen () = { host = ""; port = 0; path = "" }
    let gen_lt _  = gen ()
    let gen_gt _  = gen ()
    let gen_random ()  = gen ()
    let gen_between _ _  = None
  end)

(*......................................................................
  LinkIndex

  Now we can create the index of links. The LinkIndex is actually a
  dictionary mapping words to LinkSets. The links appear in a words
  LinkSet are links to web-pages that reference that word 
 *)

module LinkIndex = 
  Dict.Make (struct
    type key = string
    type value = LinkSet.set
    let compare = Order.string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set
          
    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt _ = gen_key ()
    let gen_key_lt _ = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between _ _ = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(*----------------------------------------------------------------------
  Page

  A Page records information about a parsed page, including

  -  a url (of type elt, an element in a LinkSet),
  -  a LinkSet of links that appeared on the page, and
  -  a list of words that appeared on the page.
 *)

type page = { url : LinkSet.elt;
              links : LinkSet.set;
              words : string list
            }

(* string_of_page -- Converts a page to a string representation for
   debugging purposes and for use in PageRank *)
let string_of_page (p : page) : string =

  (* truncate a string to at most 100 characters *)
  let trunc s =
    if String.length s > 100 then
      (String.sub s 0 100) ^ "..."
    else s in

  (* concatenate the links and truncate them *)
  let ls = trunc (LinkSet.string_of_set p.links) in

  (* concatenate the words and truncate them *)
  let ws = trunc (String.concat ";" p.words) in

  (* return the concatenated information *)
  "page(" ^ (LinkSet.string_of_elt p.url) ^
    "; Links: " ^ ls ^ "; Words: " ^ ws ^ ")"
;;
