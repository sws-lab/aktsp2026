(** Püsipunktid.

    Vt. "Introduction to Compiler Design" õpikust, peatükk 1.5.1.
    Vt. Slaidid. *)

(** Püsipunktid üle suvalise võrreldava tüübi. *)
module Make (D: sig type t [@@deriving eq] end) =
struct
  (** Leiab funktsiooni püsipunkti alustades iteratsiooni antud väärtusest. *)
  let rec fp (f: D.t -> D.t) (x: D.t): D.t =
    let x' = f x in
    if D.equal x x' then
      x
    else
      fp f x'
end

(** Püsipunktid üle hulkade. *)
module MakeSet (D: Set.S) =
struct
  include Make (D)

  (** Leiab funktsiooni vähima püsipunkti.
      Kasutada fp funktsiooni. *)
  let lfp (f: D.t -> D.t): D.t =
    fp f D.empty

  (** Leiab funktsiooni sulundi, mis sisaldab antud väärtusi.
      Kasutada lfp funktsiooni. *)
  let closure (f: D.t -> D.t) (initial: D.t): D.t =
    lfp (fun x -> D.union initial (f x))

  (** Leiab agara distributiivse funktsiooni sulundi, mis sisaldab antud väärtusi.
      Pole vaja kasutada fp/lfp funktsiooni. *)
  let closure_strict_distr (f: D.t -> D.t) (initial: D.t): D.t =
    let rec helper closure frontier =
      (* if D.is_empty frontier then *)
      if D.subset frontier closure then
        closure
      else
        (* helper (D.union closure frontier) (f frontier) *)
        let closure' = D.union closure frontier in
        helper closure' (D.diff (f frontier) closure')
    in
    helper D.empty initial
end

(** Püsipunktid üle domeenide. *)
module MakeDomain (D: Domain.S) =
struct
  include Make (D)

  (** Leiab funktsiooni vähima püsipunkti. *)
  let lfp (f: D.t -> D.t): D.t =
    fp f D.bot

  (** Leiab funktsiooni sulundi, mis sisaldab antud domeeni elementi. *)
  let closure (f: D.t -> D.t) (initial: D.t): D.t =
    lfp (fun x -> D.join initial (f x))
end
