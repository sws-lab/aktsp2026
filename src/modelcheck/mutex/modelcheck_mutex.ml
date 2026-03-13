(** Muteksite mudelkontroll. *)

(** Naiivne muteks:

    Algseisund:
       flag = false;

    Lõim 1:                       Lõim 2:
    1: while (flag) {}            1: while (flag) {}
    2: flag = true;               2: flag = true;
    3: // kriitiline sektsioon    3: // kriitiline sektsioon
    4: flag = false;              4: flag = false;
    5:                            5:

    *)
module NaiveMutex =
struct
  type point = P1 | P2 | P3 | P4 | P5 [@@deriving ord, show]
  type t = {
    p1: point;
    p2: point;
    flag: bool;
  } [@@deriving ord, show]

  let initial: t = {
    p1 = P1;
    p2 = P1;
    flag = false;
  }



  let step (s: t): t list =
    failwith "TODO"

  let is_error (s: t): bool =
    failwith "TODO"
end

(** Petersoni algoritm (https://en.wikipedia.org/wiki/Peterson%27s_algorithm):

    Algseisund:
       flag1 = flag2 = false;
       turn = 1; // pole tegelt oluline

    Lõim 1:                             Lõim 2:
    1: flag1 = true;                    1: flag2 = true;
    2: turn = 2;                        2: turn = 1;
    3: while (flag2 && turn == 2) {}    3: while (flag1 && turn == 1) {}
    4: // kriitiline sektsioon          4: // kriitiline sektsioon
    5: flag1 = false;                   5: flag2 = false;
    6:                                  6:

    *)
module PetersonAlgorithm =
struct
  type point = P1 | P2 | P3 | P4 | P5 | P6 [@@deriving ord, show]
  type thread = T1 | T2 [@@deriving ord, show]
  type t = {
    p1: point;
    p2: point;
    flag1: bool;
    flag2: bool;
    turn: thread;
  } [@@deriving ord, show]

  let initial: t = {
    p1 = P1;
    p2 = P1;
    flag1 = false;
    flag2 = false;
    turn = T1;
  }



  let step (s: t): t list =
    failwith "TODO"

  let is_error (s: t): bool =
    failwith "TODO"
end

(** Dekkeri algoritm (https://en.wikipedia.org/wiki/Dekker%27s_algorithm):

    Algseisund:
        flag1 = flag2 = false;
        turn = 1; // pole tegelt oluline

    Lõim 1:                         Lõim 2:
     1: flag1 = true;                1: flag2 = true;
     2: while (flag2) {              2: while (flag1) {
     3:   if (turn != 1) {           3:   if (turn != 2) {
     4:     flag1 = false;           4:     flag2 = false;
     5:     while (turn != 1) {}     5:     while (turn != 2) {}
     6:     flag1 = true;            6:     flag2 = true;
     7:   }                          7:   }
     8: }                            8: }
     9: // kriitiline sektsioon      9: // kriitiline sektsioon
    10: turn = 2;                   10: turn = 1;
    11: flag1 = false;              11: flag2 = false;
    12:                             12:

    *)
module DekkerAlgorithm =
struct
  type point = P1 | P2 | P3 | P4 | P5 | P6 | P9 | P10 | P11 | P12 [@@deriving ord, show]
  type thread = T1 | T2 [@@deriving ord, show]
  type t = {
    p1: point;
    p2: point;
    flag1: bool;
    flag2: bool;
    turn: thread;
  } [@@deriving ord, show]

  let initial: t = {
    p1 = P1;
    p2 = P1;
    flag1 = false;
    flag2 = false;
    turn = T1;
  }



  let step (s: t): t list =
    failwith "TODO"

  let is_error (s: t): bool =
    failwith "TODO"
end
