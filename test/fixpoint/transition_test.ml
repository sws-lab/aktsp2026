(** Transitiivse sulundi näide Vesali "The Sulund Design Pattern™" slaididelt. *)
open OUnit2
open Fixpoint

module Transition =
struct
  type t = int * string * int [@@deriving ord, show]
end

module TransitionSet =
struct
  include Set.Make (Transition) (* Taaskasutame standardset hulga moodulit. *)
  (* Aga lisame mõne funktsiooni juurde. *)

  (** Relatsioonide kompositsioon. *)
  let compose rel1 rel2 =
    fold (fun (s1, l1, t1) acc ->
        fold (fun (s2, l2, t2) acc ->
            if t1 = s2 then
              add (s1, l1 ^ l2, t2) acc
            else
              acc
          ) rel2 acc
      ) rel1 empty

  let show ts = [%show: Transition.t list] (elements ts)
end

(** Algne relatsioon. *)
let initial = TransitionSet.of_list [
  (0, "a", 1);
  (1, "b", 2);
  (1, "c", 3);
  (3, "d", 4);
]

(** Oodatav transitiivne sulund. *)
let expected = TransitionSet.of_list [
  (0, "a", 1);
  (1, "b", 2);
  (1, "c", 3);
  (3, "d", 4);
  (0, "ab", 2);
  (0, "ac", 3);
  (1, "cd", 4);
  (0, "acd", 4);
]

module TransitionSetFP = MakeSet (TransitionSet)

let assert_equal = assert_equal ~cmp:TransitionSet.equal ~printer:TransitionSet.show

(** Generaatorid omaduspõhiseks testimiseks. *)
let arbitrary_state = QCheck.int_range 0 6
let arbitrary_label = QCheck.(string_small_of (Gen.oneof_list ['a'; 'b'; 'c'; 'd'; 'e'; 'f']))
let arbitrary_transition = QCheck.triple arbitrary_state arbitrary_label arbitrary_state
let arbitrary_ts = QCheck.(map ~rev:TransitionSet.elements TransitionSet.of_list (list_small arbitrary_transition))


(** Püsipunkti funktsioon. *)
let f ts = TransitionSet.union initial (TransitionSet.compose ts initial)

(** Omaduspõhine test, mis kontrollib püsipunkti funktsiooni monotoonsust. *)
let test_fp_f_mono =
  QCheck.Test.make ~name:"f_mono"
    (QCheck.pair arbitrary_ts arbitrary_ts)
    (fun (ts1, ts2) ->
      QCheck.assume (TransitionSet.subset ts1 ts2);
      TransitionSet.subset (f ts1) (f ts2)
    )
  |> QCheck_ounit.to_ounit2_test

let test_fp _ =
  assert_equal expected (TransitionSetFP.fp f TransitionSet.empty)

let test_lfp _ =
  assert_equal expected (TransitionSetFP.lfp f)


(** Sulundi funktsioon. *)
let f ts = TransitionSet.compose ts initial

(** Omaduspõhine test, mis kontrollib sulundi funktsiooni monotoonsust. *)
let test_closure_f_mono =
  QCheck.Test.make ~name:"f_mono"
    (QCheck.pair arbitrary_ts arbitrary_ts)
    (fun (ts1, ts2) ->
      QCheck.assume (TransitionSet.subset ts1 ts2);
      TransitionSet.subset (f ts1) (f ts2)
    )
  |> QCheck_ounit.to_ounit2_test

let test_closure _ =
  assert_equal expected (TransitionSetFP.closure f initial)

(** Test, mis kontrollib sulundi funktsiooni agarust. *)
let test_closure_f_strict _ =
  assert_equal TransitionSet.empty (f TransitionSet.empty)

(** Omaduspõhine test, mis kontrollib sulundi funktsiooni distributiivsust. *)
let test_closure_f_distr =
  QCheck.Test.make ~name:"f_distr"
    (QCheck.pair arbitrary_ts arbitrary_ts)
    (fun (ts1, ts2) ->
      TransitionSet.equal (f (TransitionSet.union ts1 ts2)) (TransitionSet.union (f ts1) (f ts2))
    )
  |> QCheck_ounit.to_ounit2_test

let test_closure_strict_distr _ =
  assert_equal expected (TransitionSetFP.closure_strict_distr f initial)


let tests =
  "transition" >::: [
    "fp" >::: [
      test_fp_f_mono;
      "fp" >:: test_fp;
      "lfp" >:: test_lfp;
    ];
    "closure" >::: [
      test_closure_f_mono;
      "closure" >:: test_closure;
      "f_strict" >:: test_closure_f_strict;
      test_closure_f_distr;
      "closure_strict_distr" >:: test_closure_strict_distr;
    ];
  ]
