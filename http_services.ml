(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

A module for sending HTTP requests and responses.  
 *)

open Pagerank ;; 
open Webtypes ;; 
open Query ;;

(* The search engine home page for default responses to the client *)
let engine_home_page : string = "./askshiebs.html" ;;

(* Buffers for server responses *)
let buf_len = 4096 ;;
let buf = Bytes.create buf_len ;;  (* not thread safe *)

(*----------------------------------------------------------------------
  GENERATING A RESPONSE TO A CLIENT QUERY

This section provides utility for responding to a client query (what
the client is searching for) Under the hood it uses the Query module
to parse the query and find the relevant links with their PageRank
score. It provides functionality for formatting the information
returned from the Query module into HTTP/HTML format for returning to
the client.
 *)

(* std_response_header -- The standard HTTP response header *)
let std_response_header : string =
  "HTTP/1.1 200 OK\r\n"
    ^ "Server: AskShiebs/0.0\n"
    ^ "content-type: text/html; charset=utf-8\n"
    ^ "Content-Language: en-us\n"
    ^ "Connection: close\n\n"
;;

(* query_response_header -- The HTTP/HTML header for search responses
   to clients. Time is the amount of time it took to perform the
   query, num is the number of results the query found. *)
let query_response_header (time: float) (num : int) : string =
  std_response_header ^
    "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" 
    ^ "<html> <head> <title>AskShiebs search results </title></head>" 
    ^ "<body><h1>Search results:</h1>" 
    ^ "<p>" ^ string_of_int num ^ " results (" ^ (Printf.sprintf "%.5f" time) 
    ^ " seconds)</p><p><ul>"
;;

(* query_response_footer -- The HTML footer for search responses to
   clients. *)
let query_response_footer : string = "</ul><hr></body></html>" ;;

(* href_of_link -- Returns a string version of a link compatible with
   HTML. If the link is a file in the root_dir, return just the
   relative path. *)
let href_of_link (l : link) (root_dir: string) : string =
  let suffix str start =
    let len = String.length str in
    String.sub str start (len - start)
  in
  if l.host = "" then
    let root_regexp = Str.regexp_string root_dir in
    if Str.string_partial_match root_regexp l.path 0
    then suffix l.path (Str.match_end ())
    else "file://" ^ l.path
  else if l.port = 80 then
    l.host ^ ":" ^ l.path
  else
    l.host ^ ":" ^ (Bytes.create l.port) ^ l.path
;;

(* html_of_urllist -- Converts a set of url's to HTML to splice into
   the search response we send to clients. *)
let html_of_urllist (links: link list) (ranks : RankDict.dict) 
    (root_dir: string) : string =
  let deoptionalize opt default =
    match opt with
    | Some x -> x
    | None -> default in
  List.fold_left 
    (fun s link ->
      "<li>" 
        ^ (Printf.sprintf "%0.*f" 4
            (deoptionalize (RankDict.lookup ranks link) 0.0))
        ^ " <a href=\""
        ^ (href_of_link link root_dir) ^ "\">"
        ^ (string_of_link link)
        ^ "</a></li>" ^ s)
    "" links
;;
  
(* sort_by_rank -- Returns a list of links sorted by their ranks. Does
   not return ranks. *)
let sort_by_rank (links:LinkSet.set) (ranks : RankDict.dict) : link list =
  let compare_links a b =
    match (RankDict.lookup ranks a, RankDict.lookup ranks b) with
    | (Some x, Some y) -> compare x y
    (* No ranking is lower than any rank item *)
    | (Some _, None) -> 1
    | (None, Some _) -> -1
    | (None, None) -> 0
  in
  let links_list = LinkSet.fold (fun l x -> x :: l) [] links in
  List.sort compare_links links_list
;;
  
(* gen_q_HTML -- Parsea a user query_string, evaluates the query to
   retrieve the set of links, counts the number of links returned, sort
   the links by their rank, and package it all up in HTML format.
 *)
let gen_q_HTML (query_string : string) (index: LinkIndex.dict) 
               (ranks : RankDict.dict) (root_dir: string) : string =
  let start = Unix.gettimeofday () in
  let query = Q.parse_query query_string in
  let links = Q.eval_query index query in
  let num = LinkSet.fold (fun l _x -> 1 + l) 0 links in
  let sorted_links = sort_by_rank links ranks in
  let response_body = html_of_urllist sorted_links ranks root_dir in
  let finish = Unix.gettimeofday () in
  let time = finish -. start in
  (query_response_header time num) ^ response_body ^ query_response_footer
;;

(*----------------------------------------------------------------------
  RESPONDING TO ALL CLIENT REQUESTS 

We assume that clients request two things from the server: they are
always either querying the server for webpages (in which case we
generate the response above) or clicking a link to a webpage (in which
case, we read the webpage and return it to the client. This section
provides the functionality for responding to the possible client
requests. *)

(* read_page -- Reads the contents of a webpage; page is the filename
   of the page.  Used when a user clicks a link to a file page. *)
let read_page (page : string) : string =

  (* read in all the lines from a file and concatenate them into a big
     string *)
  let rec input_lines (inchan : in_channel) (lines: string list) : string list =
    try
      input_lines inchan ((input_line inchan) :: lines)
    with End_of_file -> List.rev lines in

  let _ = Printf.printf "Preparing '%s' for rendering...\n" page in
  let _ = flush_all () in
  let ch = open_in page in
  let lines = input_lines ch [] in
  let resp = std_response_header ^ (String.concat "" lines) in
  close_in ch;
  resp
;;

(* std_response -- Builds a message that has the default search engine
   home page to send to clients. *)
let std_response : string = read_page engine_home_page ;;

(* send_file -- Sends the file at fd back to the client using the
   buffer buf.  *)
let send_file (fd : Unix.file_descr) (buf : string): int =
  (* if the file is too big to send in one go, continue
     sending the remaining portions of the file *)
  let rec more st size =
    let res = Unix.send fd buf st size [] in
    if res < size then
      more (st + res) (size - res)
    else ()
  in
  let size = String.length buf in
  let _ = more 0 size in size
;;

(* process_request -- We're expecting a GET followed by a url or a
   query "?q=word+word".  If we find a query, then we feed it to the
   query parser to get query abstract syntax.  Then we evaluate the
   query, using the index we built earlier, to get a set of links.
   Then we put the result in an html document to send back to the
   client.

   If we find a url, we try to send back the correponding file.

   If we don't understand the request, then we send the default
   page. *)
let process_request (client_fd : Unix.file_descr)
                    (root_dir: string) 
                    (request : string)
                    (index : LinkIndex.dict)
                    (ranks: RankDict.dict)
                  : int =
  (*  let _ = Printf.printf "Request: %s\n----\n" request in
      let _ = flush_all() in *)
  let is_search (q : string) : bool = 
    let r = Str.regexp_string "?q=" in
    Str.string_match r q 0
  in
  let is_safe (q : string) : bool =
    (* At least check that the passed in path doesn't contain .. *)
    (* These checks *could* definitely be made more exhaustive *)
    let r = Str.regexp_string ".." in
    try
      let _ = Str.search_forward r q 0 in
      false
    with Not_found -> true
  in
  let http_get_re = 
    Str.regexp_case_fold "GET[ \t]+/\\([^ \t]*\\)[ \t]+HTTP/1\\.[0-9]"
  in
  try
    let _ = Str.search_forward http_get_re request 0 in
    let query_string = Str.matched_group 1 request in
    let response =
      match is_search query_string, is_safe query_string with 
      | true, _ -> gen_q_HTML query_string index ranks root_dir 
      | false, true -> read_page (Filename.concat root_dir query_string)
      | _ -> (Printf.printf "%s" "not safe!"; flush_all (); std_response)
    in send_file client_fd response
  with _ -> send_file client_fd std_response
;;
  
  
(*----------------------------------------------------------------------
  PROGRAMATICALLY GENERATING CLIENT REQUESTS

This section provides utility for programatically generating requests
on behalf of the client (which is what the crawler does!). *)
  
let rec receive_message (fd : Unix.file_descr) (contents: string list) : string =
  let len = Unix.recv fd buf 0 buf_len [] in
  if len = 0 then String.concat "" (List.rev contents)
  else receive_message fd ((String.sub buf 0 len) :: contents)
;;

(* The response from the web-server will have a bunch of headers on it
   separated from the actual data by two newlines (or two
   carriage-returns/line-feeds.)  This finds the demarcation and
   strips off all the headers. *)
let strip_headers (page_string : string) : string =
  (* helper function for finding the location of the newlines *)
  let rec find_two_newlines (i : int) : int option =
    if i + 2 < String.length page_string then
      match String.sub page_string i 2 with
      | "\n\n" -> Some (i + 2)
      | "\r\n" ->
         if i+4 < String.length page_string then
           (match String.sub page_string (i + 2) 2 with
            | "\r\n" -> Some (i + 4)
            | _ -> find_two_newlines (i + 1))
         else None
      | _ -> find_two_newlines (i + 1)
    else None
  in
  
  (* find the newlines and return the string of the page information *)
  match find_two_newlines 0 with
  | None -> page_string
  | Some i -> String.sub page_string i (String.length page_string - i)
;;
  
(* inet_fetch_url "www.seas.harvard.edu" 80 "/foo.html" -- Returns
  as a string the message response from the web server
  www.seas.harvard.edu:80 for path "/foo.html".  This is using the
  more expensive HTTP 1.0 style protocol where a socket is opened and
  closed for each request. *)
let inet_fetch_url (link : link) : string option =
  let host_addr =
    try Unix.inet_addr_of_string link.host
    with Failure _ ->
      (Unix.gethostbyname link.host).Unix.h_addr_list.(0) in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let _ = Unix.connect fd (Unix.ADDR_INET (host_addr,link.port)) in
  let msg = "GET " ^ link.path ^ " HTTP/1.1\r\nHost: " ^ link.host ^ ":" ^
    (Bytes.create link.port) ^ "Connection: close\r\n\r\n" in
  let i = Unix.send fd msg 0 (String.length msg) [] in
  if i = -1 then raise (Failure "Unix.send failed")
  else
    let result = receive_message fd [] in
    Unix.close fd;
    Some (strip_headers result)
;;

let file_fetch_url (link : link) : string option =
  let chan = open_in link.path in
  let rec receive_message chan contents =
    let len = input chan buf 0 buf_len in
    if len = 0 then String.concat "" (List.rev contents)
    else receive_message chan ((String.sub buf 0 len) :: contents) in
  let result = receive_message chan [] in
  close_in chan;
  Some result
;;

let fetch_url (link : link) (crawl_internet : bool) : string option =
  (* we return a blank page unless the link is a txt, text, htm, html,
     ml, or mli file *)
  let suffixes = [ ""; ".html"; ".html"; ".txt"; ".text"; ".ml"; ".mli"] in
  try
    if (List.exists (Filename.check_suffix link.path) suffixes) then
      if crawl_internet then inet_fetch_url link
      else file_fetch_url link
    else None
  with _ -> None
;;