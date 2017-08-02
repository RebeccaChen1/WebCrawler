(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

The crawler, which builds a dictionary from words to sets of
links.
 *)

(* Rename modules for convenience *)
module WT = Webtypes ;;
module CS = Crawler_services ;;

(* Only look at pagerank if you plan on implementing it! *)
module PR = Pagerank ;;

(*----------------------------------------------------------------------
  Section 1: CRAWLER
 *)

(* TODO: Replace the implementation of the crawl function (currently
   just a stub returning the empty dictionary) with a proper index of
   crawled pages. Build an index as follows:

   Remove a link from the frontier (the set of links that have yet to
   be visited), visit this link, add its outgoing links to the
   frontier, and update the index so that all words on this page are
   mapped to linksets containing this url.

   Keep crawling until we've reached the maximum number of links (n) or
   the frontier is empty.
 *)

let rec crawl (n : int)
          (frontier : WT.LinkSet.set)
          (visited : WT.LinkSet.set)
          (d : WT.LinkIndex.dict)
        : WT.LinkIndex.dict =
  (* Checks if # pages have been crawled and returns *)
  if n = 0 then d
  else
    (* Takes a link from the frontier *)
    match WT.LinkSet.choose frontier with
    | None -> d
    | Some (lnk, s) ->
      (* If the link has been visited already then remove it from the frontier
       * and call crawl again without decrementing the counter *)
      if WT.LinkSet.member visited lnk then crawl n s visited d
      else
      match CS.get_page lnk with
      | None -> crawl n s (WT.LinkSet.insert visited lnk) d
      | Some p ->
        (* Takes a string list of words on a page and a dictionary and
         * returns an updated dictionary that includes those words and the
         * links that word appears on *)
        let rec update_dict (lst : string list)
                  (d : WT.LinkIndex.dict)
                : WT.LinkIndex.dict =
          (match lst with
           | [] -> d
           | h :: t -> let lnk_set = WT.LinkSet.singleton lnk in
                       (* Checks if a key is in the dictionary already and
                        * then updates the value or adds the key value
                        * pair based on the result of lookup *)
                       match WT.LinkIndex.lookup d h with
                       | None -> update_dict t (WT.LinkIndex.insert d h lnk_set)
                       | Some v -> update_dict t (WT.LinkIndex.insert d h
                         (WT.LinkSet.insert v lnk)))
        in
        let d' = update_dict p.words d in
        crawl (n - 1) (WT.LinkSet.union p.links s)
          (WT.LinkSet.insert visited lnk) d' ;;

let crawler (num_pages_to_search : int) (initial_link : WT.link) =
  crawl num_pages_to_search
    (WT.LinkSet.singleton initial_link)
    WT.LinkSet.empty
    WT.LinkIndex.empty ;;
