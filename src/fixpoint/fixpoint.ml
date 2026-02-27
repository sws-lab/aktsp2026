(** Püsipunktid.

    Vt. "Introduction to Compiler Design" õpikust, peatükk 1.5.1.
    Vt. Slaidid. *)

(** Püsipunktid üle suvalise võrreldava tüübi. *)
module Make (D: sig type t [@@deriving eq] end) =
struct
  (** Leiab funktsiooni püsipunkti alustades iteratsiooni antud väärtusest. *)
  let rec fp (f: D.t -> D.t) (x: D.t): D.t =
    failwith "TODO"
end

(** Püsipunktid üle hulkade. *)
module MakeSet (D: Set.S) =
struct
  include Make (D)

  (** Leiab funktsiooni vähima püsipunkti.
      Kasutada fp funktsiooni. *)
  let lfp (f: D.t -> D.t): D.t =
    failwith "TODO"

  (** Leiab funktsiooni sulundi, mis sisaldab antud väärtusi.
      Kasutada lfp funktsiooni. *)
  let closure (f: D.t -> D.t) (initial: D.t): D.t =
    failwith "TODO"

  (** Leiab agara distributiivse funktsiooni sulundi, mis sisaldab antud väärtusi.
      Pole vaja kasutada fp/lfp funktsiooni. *)
  let closure_strict_distr (f: D.t -> D.t) (initial: D.t): D.t =
    failwith "TODO"
end
