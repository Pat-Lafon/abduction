type t = {
  templates_file : string; (* .ml file of candidate let[@axiom] templates *)
  templates : string list; (* which candidate names to select from that file *) (* TODO: maybe unnneed give that we no longer use templates for recursive arguments?*)
  name_to_avoid : string list; [@default [ "inv"; "mx"; "lo"; "hi" ]]
  rlimit : int;
}
[@@deriving of_yojson { strict = true }]

include ConfigSection.Make (struct
  type nonrec t = t

  let name = "abduction"
  let of_yojson = of_yojson
end)
