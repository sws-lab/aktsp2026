(** Abstraktne väärtustaja ehk abstraktne interpretaator. *)
open Ast

(** Abstraktseid väärtustajaid saab luua kasutades erinevaid täisarve abstraheerivaid domeene.
    Vt. IntDomain. *)
module Make (ID: IntDomain.S) =
struct
  (** Väärtuskeskkonna domeen kasutades antud täisarvude domeeni. *)
  module ED = EnvDomain.Make (ID)

  (** Väärtustab avaldise keskkonnas.
      Vihje: ID.of_int.
      Vihje: ID.of_interval.
      Vihje: ID.eval_binary. *)
  let rec eval_expr (env: ED.t) (expr: expr): ID.t =
    match expr with
    | Num i -> ID.of_int i
    | Var x -> ED.find x env
    | Rand (l, r) -> ID.of_interval (l, r)
    | Binary (l, b, r) ->
      ID.eval_binary (eval_expr env l) b (eval_expr env r)

  (** Väärtustab valvuri (avaldis ja selle oodatav tõeväärtus) keskkonnas.
      Kui valvur on keskkonnaga vastuolus, siis tagastab saavutamatu programmi oleku ED.bot.
      Kui valvuriga saab keskkonna muutujate väärtusi täpsemaks kitsendada, siis võib keskkonda muuta.
      Võib jätta keskkonna muutmata, kuid siis ei kasutata valvurist saadavat lisainfot. *)
  let eval_guard (env: ED.t) (expr: expr) (branch: bool): ED.t =
    let id = eval_expr env expr in
    if not branch && not (ID.leq (ID.of_int 0) id) then
      ED.bot
    (* else if branch && ID.equal (ID.of_int 0) id then *)
    else if branch && ID.leq id (ID.of_int 0) then
      ED.bot
    else
      match expr, branch with
      | Var x, false -> ED.add x (ID.of_int 0) env
      | Var x, true ->
        let prev = ED.find x env in
        let excl = ID.exclude 0 prev in
        ED.add x excl env
      | Binary (Var x, Eq, Num i), false
      | Binary (Var x, Ne, Num i), true ->
        let prev = ED.find x env in
        let excl = ID.exclude i prev in
        ED.add x excl env
      | Binary (Var x, Eq, Num i), true
      | Binary (Var x, Ne, Num i), false ->
        ED.add x (ID.of_int i) env
      | _ -> env

  module EDFP = Fixpoint.MakeDomain (ED)

  (** Väärtustab lause keskkonnas.
      Vihje: Vea jaoks kasuta failwith funktsiooni.
      Vihje: eval_guard.
      Vihje: While jaoks kasuta püsipunkti moodulit EDFP. *)
  let rec eval_stmt (env: ED.t) (stmt: stmt): ED.t =
    match stmt with
    | Nop -> env
    | Assign (x, e) -> ED.add x (eval_expr env e) env
    | Seq (a, b) -> eval_stmt (eval_stmt env a) b
    | If (c, t, f) ->
      let env_t = eval_stmt (eval_guard env c true) t in
      let env_f = eval_stmt (eval_guard env c false) f in
      ED.join env_t env_f
    | Error ->
      if ED.equal env ED.bot then
        ED.bot
      else
        failwith "eval_stmt: Error"
    | _ -> failwith "TODO"
end
