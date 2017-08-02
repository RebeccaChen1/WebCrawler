(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

This file is the top-level code for a simple web crawler and search engine.
 *)

module AT = Askshiebs_tests ;;
module CS = Crawler_services ;;
module CR = Crawl ;;
module PR = Pagerank ;;
module WT = Webtypes ;;
module HS = Http_services ;;

(* server index ranks -- Runs a web server serving up a search engine
   with the provided index and ranks: opens a socket on the server
   port (specified on the command line), prepares it for listening,
   and then loops, accepting requests and sending responses. *)
let server (index : WT.LinkIndex.dict) (ranks : PR.RankDict.dict) =
  let _ = Printf.printf "Starting AskShiebs on port %d.\n" CS.server_port in
  let _ = Printf.printf "Press Ctrl-c to terminate AskShiebs.\n" in
  let _ = flush_all () in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_any, CS.server_port) in
  let _ = Unix.setsockopt fd Unix.SO_REUSEADDR true in
  let _ = Unix.bind fd sock_addr in
  let _ = Unix.listen fd 5 in  (* at most 5 queued requests *)
  let rec server_loop () =
    (* allow a client to connect *)
    let (client_fd, _) = Unix.accept fd in
    let buf = Bytes.create 4096 in
    let len = Unix.recv client_fd buf 0 (String.length buf) [] in
    let request = String.sub buf 0 len in
    let _ = HS.process_request client_fd CS.root_dir request index ranks in
      Unix.close client_fd ;
      server_loop() in
    server_loop() ;;

(* main -- Crawl a bit of the web, building an index and ranks for the
   pages, and then serve up a sarch engine for the crawled
   material. *)
let main () =
  (* Want different random numbers every time. *)
  let _ = Random.self_init () in
  let _ = flush_all () in
  let _ = Printf.printf "Indexing %d pages.\n" CS.num_pages_to_search in
  (* test the crawler -- prints whether tests succeeded *)
  let _ = AT.crawler_tests () in
  (* Construct the index to pass to the server *)
  let index =
    AT.time_crawler CR.crawler CS.num_pages_to_search CS.initial_link in
  let _ = Printf.printf "Index has been constructed.\n" in
  let _ = Printf.printf "Computing pageranks.\n" in
  let ranks = CS.compute_pagerank index in
  (* let _ = AT.debug_pageranks ranks in *)
  let _ = Printf.printf "Pageranks computed.\n" in
  server index ranks ;;

(* If called directly, run the server. *)
try
  let _ =
    Str.search_forward
      (Str.regexp "askshiebs\\.\\(byte\\|native\\)")
      (Sys.argv.(0)) 0 in
  main ()
with Not_found ->
  Printf.printf "Running from test; will not start server.\n" ;;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) each part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

(* First part: Implementing the crawler *)
let minutes_spent_on_crawl () : int = 360 ;;

(* Second part: Sets as dictionaries *)
let minutes_spent_on_sets () : int = 360 ;;

(* Third part: Testing the performance of the implementations *)
let minutes_spent_on_performance () : int = 420 ;;
