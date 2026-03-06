open Model

(** Mudelkontrollija, mis kasutab püsipunkti.
    Vt. Fixpoint moodul. *)
module MakeNaive (Model: Model) =
struct
  module StateSet = Set.Make (Model)
  module StateSetFP = Fixpoint.MakeSet (StateSet)

  let f states =
    StateSet.elements states
    |> List.concat_map Model.step
    |> StateSet.of_list

  (** Tagastab kõik saavutatavad olekud. *)
  let all_states (): StateSet.t =
    StateSetFP.closure_strict_distr f (StateSet.singleton Model.initial)

  (** Tagastab kõik saavutatavad veaolekud.
      Vihje: StateSet.filter. *)
  let error_states (): StateSet.t =
    StateSet.filter Model.is_error (all_states ())

  (** Kas mõni veaolek on saavutatav? *)
  let has_error (): bool =
    not (StateSet.is_empty (error_states ()))

  (** Kas veaolekud on mittesaavutatavad? *)
  let is_correct (): bool =
    not (has_error ())
end


