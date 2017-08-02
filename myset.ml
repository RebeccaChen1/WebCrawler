(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

An interface and simple implementation of a set abstract datatype.
 *)

open Order ;;

(* Interface for set modules *)

module type SET =
  sig
    (* type of elements in the set *)
    type elt

    (* abstract type for the set *)
    type set

    val empty : set

    val is_empty : set -> bool

    val insert : set -> elt -> set

    val singleton : elt -> set

    val union : set -> set -> set

    val intersect : set -> set -> set

    (* remove an element from the set -- if the element isn't present,
      returns set unchanged *)
    val remove : set -> elt -> set

    (* returns true iff the element is in the set *)
    val member : set -> elt -> bool

    (* chooses some member from the set, removes it and returns that
       element plus the new set.  If the set is empty, returns
       None. *)
    val choose : set -> (elt * set) option

    (* fold a function across the elements of the set in some
       unspecified order, using the calling convention of fold_left,
       that is, if the set s contains s1, ..., sn, then
          fold f u s
       returns
          (f ... (f (f u s1) s2) ... sn)
     *)
    val fold : ('a -> elt -> 'a) -> 'a -> set -> 'a

    (* functions to convert values of these types to a string
       representation; useful for debugging. *)
    val string_of_set : set -> string
    val string_of_elt : elt -> string

    (* runs the tests. See TESTING EXPLANATION *)
    val run_tests : unit -> unit
  end

(* COMPARABLE signature -- A module that provides for elements that
   can be compared as to ordering and converted to a string
   representation. Includes functinos for generating values for
   testing purposes.
 *)

module type COMPARABLE =
  sig
    type t
    val compare : t -> t -> ordering
    val string_of_t : t -> string

    (* The functions below are used for testing. See TESTING EXPLANATION *)

    (* Generate a value of type t. The same t is always returned *)
    val gen : unit -> t

    (* Generate a random value of type t. *)
    val gen_random : unit -> t

    (* Generate a t greater than the argument. *)
    val gen_gt : t -> t

    (* Generate a t less than the argument. *)
    val gen_lt : t -> t

    (* Generate a t between the two arguments. Return None if no such
       t exists. *)
    val gen_between : t -> t -> t option
  end

(* An example implementation of the COMPARABLE signature. Use this
   struct for testing. *)

module IntComparable : (COMPARABLE with type t = int) =
  struct
    type t = int
    let compare x y =
      if x < y then Less
      else if x > y then Greater
      else Equal
    let string_of_t = string_of_int
    let gen () = 0
    let gen_random =
      let _ = Random.self_init () in
      (fun () -> Random.int 10000)
    let gen_gt x = x + 1
    let gen_lt x = x - 1
    let gen_between x y =
      let (lower, higher) = (min x y, max x y) in
      if higher - lower < 2 then None else Some (higher - 1)
  end

(*----------------------------------------------------------------------
  Implementation 1: List-based implementation of sets, represented as
  sorted lists with no duplicates.
 *)

module ListSet (C: COMPARABLE) : (SET with type elt = C.t) =
  struct
    type elt = C.t
    type set = elt list

    (* INVARIANT: sorted, no duplicates *)
    let empty = []

    let is_empty xs =
      match xs with
      | [] -> true
      | _ -> false

    let singleton x = [x]

    let rec insert xs x =
      match xs with
      | [] -> [x]
      | y :: ys ->
          match C.compare x y with
          | Greater -> y :: (insert ys x)
          | Equal -> xs
          | Less -> x :: xs

    let union xs ys = List.fold_left insert xs ys

    let rec remove xs y =
      match xs with
      | [] -> []
      | x :: xs1 ->
          match C.compare y x with
          | Equal -> xs1
          | Less -> xs
          | Greater -> x :: (remove xs1 y)

    let rec intersect xs ys =
      match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh :: xt, yh :: yt ->
          match C.compare xh yh with
          | Equal -> xh :: (intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt

    let rec member xs x =
      match xs with
      | [] -> false
      | y :: ys ->
          match C.compare x y with
          | Equal -> true
          | Greater -> member ys x
          | Less -> false

    let choose xs =
      match xs with
      | [] -> None
      | x :: rest -> Some (x, rest)

    let fold = List.fold_left

    let string_of_elt = C.string_of_t

    let string_of_set (s: set) : string =
      let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
      "set([" ^ (List.fold_left f "" s) ^ "])"


    (* Tests for the ListSet functor -- These are just examples of
    tests, your tests should be a lot more thorough than these. *)

    (* adds a list of (key,value) pairs in left-to-right order *)
    let insert_list (d: set) (lst: elt list) : set =
      List.fold_left (fun r k -> insert r k) d lst

    let rec generate_random_list (size: int) : elt list =
      if size <= 0 then []
      else (C.gen_random ()) :: (generate_random_list (size - 1))

    let test_insert () =
      let elts = generate_random_list 100 in
      let s1 = insert_list empty elts in
      List.iter (fun k -> assert(member s1 k)) elts;
      ()

    let test_remove () =
      let elts = generate_random_list 100 in
      let s1 = insert_list empty elts in
      let s2 = List.fold_right (fun k r -> remove r k) elts s1 in
      List.iter (fun k -> assert(not (member s2 k))) elts;
      ()

    let test_union () =
    let x = C.gen () in
    let y = C.gen_gt x in
    let s = singleton x in
    let s1 = insert s y in
    let s2 = singleton y in
    assert (union s s = s) ;
    assert (union s empty = s) ;
    assert (union s s1 = s1) ;
    assert (union s s2 = s1) ;
    assert (union empty empty = empty) ;
      ()

    let test_intersect () =
    let x = C.gen () in
    let y = C.gen_gt x in
    let s = singleton x in
    let s1 = insert s y in
    let s2 = singleton y in
    assert (intersect s s = s) ;
    assert (intersect s empty = empty) ;
    assert (intersect s s1 = s) ;
    assert (intersect s s2 = empty) ;
    assert (intersect empty empty = empty) ;
      ()

    let test_member () =
    let x = C.gen () in
    let y = C.gen_gt x in
    let s = singleton x in
    let s1 = insert s y in
    assert (member s x = true) ;
    assert (member empty x = false) ;
    assert (member s1 x = true) ;
    assert (member s y = false) ;
      ()

    let test_choose () =
    let x = C.gen () in
    let s = singleton x in
    assert (choose s = Some (x, empty)) ;
    assert (choose empty = None) ;
      ()

    let test_fold () =
    let x = C.gen () in
    let y = C.gen_gt x in
    let s = singleton x in
    let s1 = insert s y in
    assert (fold (fun a _b -> a + 1) 0 s = 1) ;
    assert (fold (fun a _b -> a + 1) 0 s1 = 2) ;
    assert (fold (fun a _b -> a + 1) 0 empty = 0) ;
      ()

    let test_is_empty () =
    let x = C.gen () in
    let s = singleton x in
    assert (is_empty s = false) ;
    assert (is_empty empty = true) ;
      ()

    let test_singleton () =
    let x = C.gen () in
    let s = insert empty x in
    assert (singleton x = s) ;
      ()

    let run_tests () =
      test_singleton () ;
      test_insert () ;
      test_remove () ;
      test_union () ;
      test_intersect () ;
      test_member () ;
      test_choose () ;
      test_fold () ;
      test_is_empty () ;
      ()

  end

(*----------------------------------------------------------------------
  Implementation 2: Sets as dictionaries
 *)
(*
  TODO: Use the skeleton code for the DictSet module below and
  complete the implementation, making sure that it conforms to the
  appropriate signature.

  Add appropriate tests for the functor and make sure that your
  implementation passes the tests. Once you have the DictSet functor
  working, you can use it instead of the ListSet implementation by
  updating the definition of the Make functor below.
*)
module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
    struct
    module D = Dict.Make(struct
        type key = C.t
        type value = unit
        let compare k k1 =
          C.compare k k1
        let string_of_key = C.string_of_t
        let string_of_value = fun _ -> "()"
        let gen_key = C.gen
        let gen_key_random = C.gen_random
        let gen_key_gt = C.gen_gt
        let gen_key_lt = C.gen_lt
        let gen_key_between = C.gen_between
        let gen_value = fun _ -> ()
        let gen_pair () = gen_key (), gen_value ()
      end)

    type elt = D.key

    type set = D.dict

    let empty = D.empty

    let is_empty = ( = ) empty

    let insert s elt = D.insert s elt ()

    let singleton = insert empty

    let union d d2 =
      if d = d2 then d
      else D.fold (D.insert) d d2

    let intersect d d2 =
      if d = d2 then d
      else
      let rec intersect_inner d d2 acc =
      match D.choose d2 with
      | None -> acc
      | Some (k,v,d') -> if D.member d k then
                           intersect_inner d d' (D.insert acc k v)
                         else intersect_inner d d' acc
    in
    intersect_inner d d2 D.empty

    let remove = D.remove

    let member = D.member

    let choose s =
      match D.choose s with
      | None -> None
      | Some (k, _v, d') -> Some (k, d')

    let rec fold f acc s =
      match D.choose s with
      | None -> acc
      | Some (k, _v, d') -> fold f (f acc k) d'

    (* implement the rest of the functions in the signature! *)

    let string_of_elt = D.string_of_key
    let string_of_set s = D.string_of_dict s

    (* Tests for the DictSet functor -- Use the tests from the ListSet
       functor to see how you should write tests. However, you must
       write a lot more comprehensive tests to test ALL your
       functions. *)

    (* Add your test functions to run_tests *)
    let run_tests () =
      let x = C.gen () in
      let x2 = C.gen_gt x in
      let x3 = C.gen_gt x2 in
      let x4 = C.gen_gt x3 in
      let s1 = insert empty x in
      let s2 = insert s1 x2 in
      let s3 = insert s2 x3 in
      let s4 = insert s3 x4 in
      let s5 = singleton x4 in
      let s6 = singleton (C.gen_lt x) in
      let test_empty () =
        assert(empty = empty);
        assert(not (empty = s1)) in
      let test_singleton () =
        assert(s1 = singleton x) in
      let test_union () =
        assert(union empty empty = empty);
        assert(union s1 s2 = s2);
        assert(union s1 empty = s1);
        assert(union empty (singleton x4) = s5) in
      let test_intersect () =
        assert(intersect empty s1 = empty);
        assert(intersect s1 s2 = s1);
        assert(intersect s4 s2 = s2);
        assert(intersect empty empty = empty);
        assert((intersect s6 s3) = empty) in
      let test_choose () =
        assert (choose empty = None);
        assert (choose s1 = (Some (x, empty)));
        assert (choose s2 = Some (x, (singleton x2)) ||
                choose s2 = Some (x2, s1)) in
      let test_member () =
        assert (member s1 x);
        assert (member s5 x4) in
      let test_remove () =
        assert (remove s1 x = empty);
        assert (remove s2 x2 = s1);
        assert (remove s6 x = s6);
        assert (remove empty x3 = empty) in
      let test_fold () =
        let length = fold (fun acc _ -> acc + 1) 0 in
        assert (length s1 = 1);
        assert (length empty = 0);
        assert (length s5 = 1);
        assert (length s4 = 4) in

      test_empty ();
      test_singleton ();
      test_member ();
      test_union ();
      test_intersect ();
      test_choose ();
      test_remove ();
      test_fold ();
end

(*----------------------------------------------------------------------
  Running the tests.
 *)

(* Create a module for sets of ints using the ListSet functor and test
   it. *)
module IntListSet = ListSet(IntComparable) ;;


(* Create a set of ints using the DictSet functor and test it.

   Uncomment out the lines below when you are ready to test your set
   implementation based on dictionaries. *)


module IntDictSet = DictSet(IntComparable) ;;

let _ = IntDictSet.run_tests() ;;

(*----------------------------------------------------------------------
  Make -- a functor that creates a set module by calling the ListSet
  or DictSet functors.

  This allows switching between th two implementations for all sets
  just by changing one place in the code.  *)

module Make (C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use the dictionary implementation of sets
     when you are finished. *)
   (*ListSet (C)*)
   DictSet(C)
