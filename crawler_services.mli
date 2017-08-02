(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

Interface and implementation of crawler services needed to build
the web index, including definitions of link and page datatypes, a
function for fetching a page given a link, and the values of the
command line arguments (the initial link, the number of pages to
search, and the server port).  
 *)

module PR = Pagerank ;;
module WT = Webtypes ;;
module HS = Http_services ;;
     
(* get_page -- Returns the page corresponding to the provided link. *)
val get_page : WT.link -> WT.page option

(* initial_link -- The initial link to be used by the crawler to start
   the crawl at. *)
val initial_link : WT.link

(* root_dir -- The root directory where the crawling will occur. The
   empty string if crawling the web, (dirname initial_link)
   otherwise. *)
val root_dir : string

(* num_pages_to_search -- The maximum number of (distinct) pages the
   crawler should process before deciding it's enough *)
val num_pages_to_search : int

(* server_port -- The port on which to listen for query requests *)
val server_port : int

(* compute_pagerank -- Computes the pagerank of a link index *)
val compute_pagerank : WT.LinkIndex.dict -> PR.RankDict.dict
