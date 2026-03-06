open Model

(** Mudelkontrollija, mis kasutab püsipunkti.
    Vt. Fixpoint moodul. *)
module MakeNaive (Model: Model) =
struct
  module StateSet = Set.Make (Model)
  module StateSetFP = Fixpoint.MakeSet (StateSet)



  (** Tagastab kõik saavutatavad olekud. *)
  let all_states (): StateSet.t =
    failwith "TODO"

  (** Tagastab kõik saavutatavad veaolekud.
      Vihje: StateSet.filter. *)
  let error_states (): StateSet.t =
    failwith "TODO"

  (** Kas mõni veaolek on saavutatav? *)
  let has_error (): bool =
    failwith "TODO"

  (** Kas veaolekud on mittesaavutatavad? *)
  let is_correct (): bool =
    failwith "TODO"
end


