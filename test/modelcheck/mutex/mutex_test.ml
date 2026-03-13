open OUnit2
open Modelcheck_mutex


module NaiveMutexChecker = Modelcheck.Checker.MakeNaive (NaiveMutex)

let test_naive_correct _ =
  assert_equal ~printer:string_of_bool false (NaiveMutexChecker.is_correct ())

let test_naive_all_states _ =
  let all_states = NaiveMutexChecker.all_states () in
  let assert_exists expected f =
    assert_equal ~printer:string_of_bool expected (NaiveMutexChecker.StateSet.exists f all_states)
  in
  assert_exists true (fun s -> s.p1 = P1 && s.p2 = P1);
  assert_exists true (fun s -> s.p1 = P5 && s.p2 = P5);
  assert_exists true (fun s -> s.p1 = P3 && s.p2 = P3);
  assert_exists true (fun s -> s.p1 = P1 && s.p2 = P3);
  assert_exists true (fun s -> s.p1 = P3 && s.p2 = P1);
  assert_exists true (fun s -> s.p1 = P5 && s.p2 = P3);
  assert_exists true (fun s -> s.p1 = P3 && s.p2 = P5);
  assert_equal ~printer:string_of_int 29 (NaiveMutexChecker.StateSet.cardinal all_states)

let test_naive_error_states _ =
  let error_states = NaiveMutexChecker.error_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:(NaiveMutex.show state) expected (NaiveMutexChecker.StateSet.mem state error_states)
  in
  (* Kõik veaolekud. *)
  assert_mem true {p1 = P3; p2 = P3; flag = true};
  assert_equal ~printer:string_of_int 1 (NaiveMutexChecker.StateSet.cardinal error_states)


module PetersonAlgorithmChecker = Modelcheck.Checker.MakeNaive (PetersonAlgorithm)

let test_peterson_correct _ =
  assert_equal ~printer:string_of_bool true (PetersonAlgorithmChecker.is_correct ())

let test_peterson_all_states _ =
  let all_states = PetersonAlgorithmChecker.all_states () in
  let assert_exists expected f =
    assert_equal ~printer:string_of_bool expected (PetersonAlgorithmChecker.StateSet.exists f all_states)
  in
  assert_exists true (fun s -> s.p1 = P1 && s.p2 = P1);
  assert_exists true (fun s -> s.p1 = P6 && s.p2 = P6);
  assert_exists false (fun s -> s.p1 = P4 && s.p2 = P4);
  assert_exists true (fun s -> s.p1 = P1 && s.p2 = P4);
  assert_exists true (fun s -> s.p1 = P4 && s.p2 = P1);
  assert_exists true (fun s -> s.p1 = P6 && s.p2 = P4);
  assert_exists true (fun s -> s.p1 = P4 && s.p2 = P6);
  assert_equal ~printer:string_of_int 34 (PetersonAlgorithmChecker.StateSet.cardinal all_states)

let test_peterson_error_states _ =
  let error_states = PetersonAlgorithmChecker.error_states () in
  assert_equal ~printer:string_of_int 0 (PetersonAlgorithmChecker.StateSet.cardinal error_states);
  (* Kontrollime veaoleku funktsiooni hüpoteetilistel olekutel. *)
  assert_equal ~printer:string_of_bool true (PetersonAlgorithm.is_error {p1 = P4; p2 = P4; flag1 = true; flag2 = true; turn = T1});
  assert_equal ~printer:string_of_bool true (PetersonAlgorithm.is_error {p1 = P4; p2 = P4; flag1 = true; flag2 = true; turn = T2})


module DekkerAlgorithmChecker = Modelcheck.Checker.MakeNaive (DekkerAlgorithm)

let test_dekker_correct _ =
  assert_equal ~printer:string_of_bool true (DekkerAlgorithmChecker.is_correct ())

let test_dekker_all_states _ =
  let all_states = DekkerAlgorithmChecker.all_states () in
  let assert_exists expected f =
    assert_equal ~printer:string_of_bool expected (DekkerAlgorithmChecker.StateSet.exists f all_states)
  in
  assert_exists true (fun s -> s.p1 = P1 && s.p2 = P1);
  assert_exists true (fun s -> s.p1 = P12 && s.p2 = P12);
  assert_exists false (fun s -> s.p1 = P9 && s.p2 = P9);
  assert_exists true (fun s -> s.p1 = P1 && s.p2 = P9);
  assert_exists true (fun s -> s.p1 = P9 && s.p2 = P1);
  assert_exists true (fun s -> s.p1 = P12 && s.p2 = P9);
  assert_exists true (fun s -> s.p1 = P9 && s.p2 = P12);
  assert_equal ~printer:string_of_int 53 (DekkerAlgorithmChecker.StateSet.cardinal all_states)

let test_dekker_error_states _ =
  let error_states = DekkerAlgorithmChecker.error_states () in
  assert_equal ~printer:string_of_int 0 (DekkerAlgorithmChecker.StateSet.cardinal error_states);
  (* Kontrollime veaoleku funktsiooni hüpoteetilistel olekutel. *)
  assert_equal ~printer:string_of_bool true (DekkerAlgorithm.is_error {p1 = P9; p2 = P9; flag1 = true; flag2 = true; turn = T1});
  assert_equal ~printer:string_of_bool true (DekkerAlgorithm.is_error {p1 = P9; p2 = P9; flag1 = true; flag2 = true; turn = T2})


let tests =
  "modelcheck" >::: [
    "mutex" >::: [
      "naive" >::: [
        "correct" >:: test_naive_correct;
        "all_states" >:: test_naive_all_states;
        "error_states" >:: test_naive_error_states;
      ];
      "peterson" >::: [
        "correct" >:: test_peterson_correct;
        "all_states" >:: test_peterson_all_states;
        "error_states" >:: test_peterson_error_states;
      ];
      "dekker" >::: [
        "correct" >:: test_dekker_correct;
        "all_states" >:: test_dekker_all_states;
        "error_states" >:: test_dekker_error_states;
      ];
    ];
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
