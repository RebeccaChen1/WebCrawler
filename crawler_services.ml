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

(* crawl_internet -- Set this flag to true to make the crawler really
   talk to the internet. Otherwise, it will treat the urls you give
   it as local file paths and ignore the host and port. *)
let crawl_internet = false ;;

(* Some useful patterns for parsing URLs and HTTP headers *)

(* http_re -- Matches a url, breaking it into a host, port number
   (default 80) and path (default "") using a regular expression. *)
let http_re =
  Str.regexp "http://\\([^/:]*\\)\\(:\\([0-9]+\\)\\)?\\(/\\([^#]*\\)\\)?"
;;

(* href_re -- Matches an href argument of an HTML anchor, extracting the link *)
let href_re = Str.regexp_case_fold "href *= *\"\\([^#\"]*\\)[^\"]*\"" ;;

(* absolute_path_re -- Matches an absolute path *)
let absolute_path_re = Str.regexp "/[.]*" ;;

(* javascript_re -- Matches a javascript url *)
let javascript_re = Str.regexp "javascript:[.]*" ;;

(* parent_path_re -- Matches a path that starts with .. *)
let parent_path_re = Str.regexp "\\.\\./.*" ;;

(* parse_url url -- Parses an url string into a link *)
let parse_url (url : string) : WT.link =
  if Str.string_match http_re url 0 then
    let host = Str.matched_group 1 url in
    let port =
      try int_of_string (Str.matched_group 3 url)
      with Not_found -> 80 in
    let init_path =
      try Str.matched_group 5 url
      with Not_found -> "" in
    let path = "/" ^ init_path in
    {host = host; port = port; path = path}
  else raise Not_found
;;

(* link_of_string source_link lik_string -- Converts an href from the
   page into a link. If the href starts with "http" then parses it to
   get the host, port, and path. Otherwise, assumes it's on this host.
   If there's no leading "/" on the href, we prepend the source_link's
   path. We filter out the javascript links. (Maybe others should be
   as well.) *)
let link_of_string (source_link : WT.link) (link_string : string) : WT.link option =
  try Some (parse_url link_string)
  with
    Not_found ->
      if Str.string_match absolute_path_re link_string 0 then
        Some {host=source_link.host;
              port=source_link.port; path=link_string}
      else if Str.string_match javascript_re link_string 0 then
        None
      else
        (* need to watch out for "../path" -- should probably also
          watch out for ./path. *)
        let rec glue_paths p s =
          if Str.string_match parent_path_re s 0 then
            glue_paths (Filename.dirname p)
              (String.sub s 3 (String.length s - 3))
          else p ^ "/" ^ s in
        let new_path =
          glue_paths (Filename.dirname source_link.path) link_string in
        Some {host=source_link.host; port=source_link.port;
              path=new_path}
;;

(* get_links source_link page -- Returns a list of URLs as links that
   occurred within href's on the page. *)
let get_links (source_link : WT.link) (page : string) : WT.LinkSet.set =
  let rec loop pos linkset =
    try
      let _ = Str.search_forward href_re page pos in
      let link_string = Str.matched_group 1 page in
      let newpos = Str.match_end() in
      match link_of_string source_link link_string with
      | None -> loop newpos linkset
      | Some link -> loop newpos (WT.LinkSet.insert linkset link)
    with
      Not_found -> linkset
  in
  loop 0 WT.LinkSet.empty
;;

(* get_words page -- Returns a list of words that occur on the page.
   For our purposes, words only contain alphabetic characters. If
   we try to use regexps here, we'll get a stack overflow, so this
   is coded by hand. *)
let get_words (page : string) : string list =
  let len = String.length page in
  let rec loop pos words =
    let rec find_end i =
      if i < len then
        let c = String.get page (pos + i) in
        if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then
          find_end (i + 1)
        else i
      else 0 in
    if pos < len then
      let e = find_end 0 in
      if e = 0 then loop (pos + 1) words else
        loop (pos + e) ((String.sub page pos e) :: words)
    else words
  in loop 0 []
;;

(* get_page link -- Attempts to get the page at the provided link. *)
let get_page (link : WT.link) : WT.page option =
  match HS.fetch_url link crawl_internet with
    None -> None
  | Some page ->
     let links = get_links link page in
     let words = get_words page in
     Some {url = link ; links = links ; words = words}
;;

(* Compute the pagerank of a link index *)
let compute_pagerank (index : WT.LinkIndex.dict) : PR.RankDict.dict =
  let links = WT.LinkIndex.fold (fun l _ v -> WT.LinkSet.union l v)
           WT.LinkSet.empty index in
  let pages = WT.LinkSet.fold
    (fun s link ->
     match get_page link with
     | None -> s
     | Some page -> PR.PageSet.insert s page) PR.PageSet.empty links in
  let link_graph = PR.graph_of_pages pages in
  PR.dict_of_ns (PR.EngineRanker.rank link_graph)
;;

(*......................................................................
Command-line processing and initializing the cralwler configuration 

Read the command line arguments and return the port number which the
server should use for serving, the number of pages to index, and the
root url. *)

let (server_port, num_pages_to_search, root_url) =
  let args = Sys.argv in
  try
    let port = int_of_string (Array.get args 1) in
    let num = int_of_string (Array.get args 2) in
    let root_url = Array.get args 3 in
    (port, num, root_url)
  with
    _ -> 
      Printf.printf "usage: %s <port> <num-pages> <root-url>\n"
        (Array.get args 0) ;
      Printf.printf ("running with defaults.\n");
      (* run with default arguments *)
      let port = 8080 in
      let num = 7 in
      let root_url = "simple-html/index.html" in
      (port, num, root_url)
;;

(* initial_link -- The link corresponding to the start URL, if
   available. *)
let initial_link : WT.link  =
  try parse_url root_url
  with
    Not_found ->
      match link_of_string {host=""; port=80; path=""} root_url with
      | None ->
          (Printf.printf
              "Please specify a url or path to a starting file\n" ;
            exit 1)
      | Some l -> l
;;

(* root_dir -- The root directory with respect to which the local
   crawl should happen. The empty string if crawling out in the
   web. *)
let root_dir : string =
  if initial_link.host = ""
  then Filename.dirname initial_link.path
  else ""
;;