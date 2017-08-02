(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

Functions for testing and timing the crawler, as well as for
debugging the crawlers returned indexes and pageranks. All
functions to be defined here and called in askshiebs.ml
 *)

open Webtypes ;;
open Pagerank ;;
open Crawler_services ;;
open Crawl ;;
open CS51 ;;

(* tests for the crawler -- series of boolean tests,
   prints to stdout the results of the tests as
   either a failure or success. You should add
   to these tests *)
(* Pulled out link_present for other tests *)
let link_present index word link =
 match LinkIndex.lookup index word with
 | Some v -> LinkSet.member v {host = ""; port = 80; path = link}
 | _ -> false ;;

let crawler_tests () =
  (*
    link_present: a helper function that takes in an index
    returned by your crawler implementation, a string word,
    and a string "link" (path, in this case) and returns
    a boolean (true or false) of
    whether or said link's page contains the word
  *)

  (* test the crawler on the initial simple index link *)
  let initial_link_simple =
    {host = ""; port = 80; path = "./simple-html/index.html"} in
  let i = crawler num_pages_to_search initial_link_simple in
  (*
    take advantage of partial application & test if
    the word "girls" appears on certain web-pages
   *)
  if
    let test_gabbi = link_present i "girls" in
    let test_sam = link_present i "conceptual" in
    let test_fifty = link_present i "csfiftyone" in
    let test_functors = link_present i "modules" in
    let test_piazza = link_present i "what" in
    let test_shieber = link_present i "OCaml" in
    let test_queue = link_present i "FIFO" in

    (* indicate if the tests succeed *)
    test_gabbi "./simple-html/gabbi.html" &&
    not (test_gabbi "./simple-html/index.html") &&
    test_sam "./simple-html/sam.html" &&
    not (test_sam "./simple-html/functors.html") &&
    test_fifty "./simple-html/cs50.html" &&
    not (test_fifty "./simple-html/functors.html") &&
    test_functors "./simple-html/functors.html" &&
    not (test_functors "./simple-html/index.html") &&
    test_piazza "./simple-html/piazza.html" &&
    not (test_piazza "./simple-html/fifty.html") &&
    test_shieber "./simple-html/shieber.html" &&
    not (test_shieber "./simple-html/sam.html") &&
    test_queue "./simple-html/queue.html" &&
    not (test_queue "./simple-html/sam.html")
  then
    Printf.printf "SUCCESS: Crawler tests succeeded!\n"
  else
    Printf.printf "FAILURE: Crawler tests failed.\n"
;;

(* function that times the crawler and prints
   to stdout, uses the cs51 module, returns
   the index  *)
let time_crawler crawler num link =
  (* partially apply crawler to force it to fit
   * in to cs51 timing framework *)
  let partially_applied = crawler num in
  Printf.printf "Crawler timing: ";
  (* this prints out the time results and returns
   * the index *)
  call_reporting_time partially_applied link
;;

(* function that prints out the linkindex for debugging *)
let debug_index (index : LinkIndex.dict) : unit =
  Printf.printf "\nIndex:\n";
  print_string (LinkIndex.string_of_dict index);
  flush_all ()
;;

(* function for debugging pagerank -- karma problem *)
let debug_pageranks (ranks : RankDict.dict) : unit =
    Printf.printf "\nRanks:\n";
    print_string (RankDict.string_of_dict ranks);
    flush_all ()
;;

let html_tests () =
  let html_link = {host = ""; port = 80; path = "./html/index.html"} in
  let i = crawler 25 html_link in

  let test_manual001 = link_present i "Lexical" in
  let test_lex = link_present i "Comments" in
  let test_language = link_present i "denote" in
  let test_patterns = link_present i "bindings" in
  let test_types = link_present i "polymorphic" in

  assert (test_manual001 "./html/manual001.html");
  assert (not (test_manual001 "./html/index.html"));
  assert (test_lex "./html/lex.html");
  assert (not (test_lex "./html/expr.html"));
  assert (test_language "./html/language.html");
  assert (not (test_language "./html/previous_motif.gif"));
  assert (test_patterns "./html/patterns.html");
  assert (not (test_patterns "./html/libgraph.gif"));
  assert (test_types "./html/types.html");
  assert (not (test_types "./html/manual019.html")) ;;

let wiki_tests () =
 let wiki_link = {host = ""; port = 80;
                  path = "./wiki/Teenage_Mutant_Ninja_Turtles"} in
 let i = crawler 224 wiki_link in

 let test_turtles = link_present i "peak" in
 let test_pizza = link_present i "subsidiary" in
 let test_saint = link_present i "manga" in
 let test_toads = link_present i "NES" in
 let test_white = link_present i "degree" in

 assert (test_turtles "./wiki/Teenage_Mutant_Ninja_Turtles");
 assert (not (test_turtles "./simple-html/index.html"));
 assert (test_pizza "./wiki/Pizza_Hut");
 assert (not (test_pizza "./simple-html/piazza.html"));
 assert (test_saint "./wiki/Saint_Seiya");
 assert (not (test_saint "./wiki/Sean_Astin"));
 assert (test_toads "./wiki/Battletoads");
 assert (not (test_toads "./html/manual001"));
 assert (test_white "./wiki/White_people");
 assert (not (test_white "./wiki/Wu_Xing")) ;;
