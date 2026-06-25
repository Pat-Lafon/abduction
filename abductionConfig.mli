type t = {
  templates_file : string;
  templates : string list;
  name_to_avoid : string list;
  rlimit : int option;
}

(* [set]/[of_meta_config] are deliberately unexposed: [bootstrap] is the sole
   entry point so the zutils + typechecker sections always get set first. See
   abductionConfig.ml. *)
val bootstrap : Yojson.Safe.t -> unit

val get : unit -> t
