(** Epsilonsulundi näide "Introduction to Compiler Design" õpikust, peatükk 1.5.1. *)
open OUnit2
open Fixpoint

module IntSet =
struct
  include Set.Make (Int) (* Taaskasutame standardset hulga moodulit. *)
  (* Aga lisame mõne funktsiooni juurde. *)

  let show is = [%show: int list] (elements is)
end

(** Epsilonsammude funktsioon, joonis 1.5. *)
let nfa_eps = function
  | 1 -> [2; 5]
  | 5 -> [6; 7]
  | 8 -> [1]
  | _ -> []

(** Epsilonsammude funktsioon hulgal. *)
let nfa_eps_set states =
  (* IntSet.of_list (List.concat_map nfa_eps (IntSet.elements states)) *)
  IntSet.elements states
  |> List.concat_map nfa_eps
  |> IntSet.of_list

module IntSetFP = MakeSet (IntSet)

let assert_equal = assert_equal ~cmp:IntSet.equal ~printer:IntSet.show

(** Generaatorid omaduspõhiseks testimiseks. *)
let arbitrary_state = QCheck.int_range 1 10
let arbitrary_states = QCheck.(map ~rev:IntSet.elements IntSet.of_list (list_small arbitrary_state))


(** Omaduspõhine test, mis kontrollib püsipunkti funktsiooni monotoonsust. *)
let test_fp_f_mono =
  QCheck.Test.make ~name:"f_mono"
    (QCheck.triple arbitrary_states arbitrary_states arbitrary_states)
    (fun (initial, states1, states2) ->
      QCheck.assume (IntSet.subset states1 states2);
      let f x = IntSet.union initial (nfa_eps_set x) in
      IntSet.subset (f states1) (f states2)
    )
  |> QCheck_ounit.to_ounit2_test

let test_fp _ =
  (* Olekust 1. *)
  let f1 x = IntSet.union (IntSet.singleton 1) (nfa_eps_set x) in
  assert_equal (IntSet.of_list [1; 2; 5; 6; 7]) (IntSetFP.fp f1 IntSet.empty);
  (* Olekust 2. *)
  let f2 x = IntSet.union (IntSet.singleton 2) (nfa_eps_set x) in
  assert_equal (IntSet.of_list [2]) (IntSetFP.fp f2 IntSet.empty);
  (* Olekust 8. *)
  let f8 x = IntSet.union (IntSet.singleton 8) (nfa_eps_set x) in
  assert_equal (IntSet.of_list [1; 2; 5; 6; 7; 8]) (IntSetFP.fp f8 IntSet.empty)

let test_lfp _ =
  (* Olekust 1. *)
  let f1 x = IntSet.union (IntSet.singleton 1) (nfa_eps_set x) in
  assert_equal (IntSet.of_list [1; 2; 5; 6; 7]) (IntSetFP.lfp f1);
  (* Olekust 2. *)
  let f2 x = IntSet.union (IntSet.singleton 2) (nfa_eps_set x) in
  assert_equal (IntSet.of_list [2]) (IntSetFP.lfp f2);
  (* Olekust 8. *)
  let f8 x = IntSet.union (IntSet.singleton 8) (nfa_eps_set x) in
  assert_equal (IntSet.of_list [1; 2; 5; 6; 7; 8]) (IntSetFP.lfp f8)


(** Omaduspõhine test, mis kontrollib sulundi funktsiooni monotoonsust. *)
let test_closure_f_mono =
  QCheck.Test.make ~name:"f_mono"
    (QCheck.pair arbitrary_states arbitrary_states)
    (fun (states1, states2) ->
      QCheck.assume (IntSet.subset states1 states2);
      IntSet.subset (nfa_eps_set states1) (nfa_eps_set states2)
    )
  |> QCheck_ounit.to_ounit2_test

let test_closure _ =
  (* Olekust 1. *)
  assert_equal (IntSet.of_list [1; 2; 5; 6; 7]) (IntSetFP.closure nfa_eps_set (IntSet.singleton 1));
  (* Olekust 2. *)
  assert_equal (IntSet.of_list [2]) (IntSetFP.closure nfa_eps_set (IntSet.singleton 2));
  (* Olekust 8. *)
  assert_equal (IntSet.of_list [1; 2; 5; 6; 7; 8]) (IntSetFP.closure nfa_eps_set (IntSet.singleton 8))

(** Test, mis kontrollib sulundi funktsiooni agarust. *)
let test_closure_f_strict _ =
  assert_equal IntSet.empty (nfa_eps_set IntSet.empty)

(** Omaduspõhine test, mis kontrollib sulundi funktsiooni distributiivsust. *)
let test_closure_f_distr =
  QCheck.Test.make ~name:"f_distr"
    (QCheck.pair arbitrary_states arbitrary_states)
    (fun (states1, states2) ->
      IntSet.equal (nfa_eps_set (IntSet.union states1 states2)) (IntSet.union (nfa_eps_set states1) (nfa_eps_set states2))
    )
  |> QCheck_ounit.to_ounit2_test

let test_closure_strict_distr _ =
  (* Olekust 1. *)
  assert_equal (IntSet.of_list [1; 2; 5; 6; 7]) (IntSetFP.closure_strict_distr nfa_eps_set (IntSet.singleton 1));
  (* Olekust 2. *)
  assert_equal (IntSet.of_list [2]) (IntSetFP.closure_strict_distr nfa_eps_set (IntSet.singleton 2));
  (* Olekust 8. *)
  assert_equal (IntSet.of_list [1; 2; 5; 6; 7; 8]) (IntSetFP.closure_strict_distr nfa_eps_set (IntSet.singleton 8))

(** Omaduspõhine test, mis kontrollib kahe sulundi samaväärsust. *)
let test_closure_equivalent =
  QCheck.Test.make ~name:"equivalent"
    arbitrary_states
    (fun initial ->
      IntSet.equal (IntSetFP.closure nfa_eps_set initial) (IntSetFP.closure_strict_distr nfa_eps_set initial)
    )
  |> QCheck_ounit.to_ounit2_test


let tests =
  "nfa_eps" >::: [
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
      test_closure_equivalent;
    ];
  ]
