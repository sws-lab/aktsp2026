(** Mündiviskega aritmeetiliste avaldiste keel.
    Vt. https://courses.cs.ut.ee/t/akt/Main/Alusosa#rnd. *)

type t =
  | Num of int (** Konstant *)
  | Neg of t (** Unaarne - *)
  | Add of t * t (** + *)
  | Flip of t * t (** Mündivise: juhuslik valik alamavaldiste vahel *)

(** Münt on tõeväärtusi andev funktsioon. *)
type coin = unit -> bool


(** Väärtustab avaldise etteantud mündiga.
    NB! Väärtustamise järjekord on oluline. *)
let rec eval (coin: coin) (e: t): int =
  failwith "TODO"


(** Konstrueerib kahe listi otsekorrutise. *)
let cartesian_product (l1: 'a list) (l2: 'b list): ('a * 'b) list =
  List.concat_map (fun x -> List.map (fun y -> (x, y)) l2) l1

(** Rakendab funktsiooni kahe listi otsekorrutisele.
    Sellega saab vältida ajutisi otsekorrutise paare. *)
let cartesian_map (f: 'a -> 'b -> 'c) (l1: 'a list) (l2: 'b list): 'c list =
  List.concat_map (fun x -> List.map (fun y -> f x y) l2) l1

(** Leiab avaldise kõik võimalikud väärtused listina.
    NB! Tulemuste järjekord on oluline.
    Vihje: List.map.
    Vihje: cartesian_product või cartesian_map. *)
let rec eval_list (e: t): int list =
  failwith "TODO"


module IntSet =
struct
  include Set.Make (Int)

  (** Rakendab funktsiooni kahe täisarvude hulga otsekorrutisele.
      Sellega saab vältida ajutisi otsekorrutise paare. *)
  let cartesian_map (f: int -> int -> int) (s1: t) (s2: t): t =
    fold (fun x acc ->
        fold (fun y acc ->
            add (f x y) acc
          ) s2 acc
      ) s1 empty
end

(** Leiab avaldise kõik võimalikud väärtused hulgana.
    Mitte kasutada eval_list funktsiooni.
    Vihje: IntSet.map.
    Vihje: IntSet.cartesian_map. *)
let rec eval_set (e: t): IntSet.t =
  failwith "TODO"
