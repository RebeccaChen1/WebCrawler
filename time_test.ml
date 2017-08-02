module AT = Askshiebs_tests ;;
open Crawl ;;

let rec timer (pages : int) (path : string) (n : int) =
  if n = 0 then 0.0
  else
  let start = Unix.gettimeofday () in
  ignore (crawler pages
          {host = ""; port = 80; path});
  let time = (Unix.gettimeofday () -. start) in
  time +. timer pages path (n - 1) ;;
AT.wiki_tests () ;;
AT.html_tests () ;;
(* Times to run for average *)
let num_times = 3 ;;
let f_num = float_of_int num_times in
let tot_simple = timer 8 "./simple-html/index.html" num_times in
let tot_html = timer 25 "./html/index.html" num_times in
let tot_wiki = timer 224 "./wiki/Teenage_Mutant_Ninja_Turtles" num_times in
let avg_simple = tot_simple /. f_num in
let avg_html = tot_html /. f_num in
let avg_wiki = tot_wiki /. f_num in
Printf.printf "Average Time of %d runs of Simple: %f\n"
  num_times avg_simple;
Printf.printf "Average Time of %d runs of HTML: %f\n"
  num_times avg_html;
Printf.printf "Average Time of %d runs of Wiki: %f\n"
  num_times avg_wiki ;;
