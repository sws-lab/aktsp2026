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
  match e with
  | Num i -> i
  | Neg e -> -(eval coin e)
  (* | Add (e1, e2) -> eval coin e2 + eval coin e1 *)
  | Add (e1, e2) ->
    let i1 = eval coin e1 in
    let i2 = eval coin e2 in
    i1 + i2
  (* | Flip (e1, e2) -> if coin () then eval coin e1 else eval coin e2 *)
  | Flip (e1, e2) -> eval coin (if coin () then e1 else e2)


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
  match e with
  | Num i -> [i]
  | Neg e -> List.map (fun x -> -x) (eval_list e)
  (* | Add (e1, e2) -> List.map (fun (x, y) -> x + y) (cartesian_product (eval_list e1) (eval_list e2)) *)
  (* | Add (e1, e2) -> cartesian_map (fun x y -> x + y) (eval_list e1) (eval_list e2) *)
  | Add (e1, e2) -> cartesian_map (+) (eval_list e1) (eval_list e2)
  | Flip (e1, e2) -> eval_list e1 @ eval_list e2


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
  (* IntSet.of_list (eval_list e) *)
  match e with
  | Num i -> IntSet.singleton i
  | Neg e -> IntSet.map (fun x -> -x) (eval_set e)
  | Add (e1, e2) -> IntSet.cartesian_map (+) (eval_set e1) (eval_set e2)
  | Flip (e1, e2) -> IntSet.union (eval_set e1) (eval_set e2)
