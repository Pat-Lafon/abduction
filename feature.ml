open Zutils
open Prop
open Sugar
open Zdatatype

(* A feature is a [prop], not a [lit], so a guarded conjunction (a recognizer
   gating an accessor, e.g. [is_rbtnode v && color v == false]) can be one
   feature that [feature_vec_to_prop] negates as a *unit*. A [lit] cannot hold
   [&&] (litencoding has no boolean-connective case), and splitting the guard
   into a separate feature would let CEGIS form the unguarded cube
   [¬is_rbtnode v ∧ color v == false]. *)
type feature_tab = Nt.t prop list
type feature_vec = bool list
type feature_vec_id = int
type label = Pos | Neg | Unknown

let is_not_neg = function Neg -> false | _ -> true
let is_positive = function Pos -> true | _ -> false

let feature_vec_to_id vec =
  let rec aux = function
    | [] -> 0
    | true :: vec -> 1 + (2 * aux vec)
    | false :: vec -> 0 + (2 * aux vec)
  in
  aux vec

let feature_id_to_vec (num_features : int) id =
  let rec aux (n, res) id =
    if n == num_features then if id == 0 then res else _failatwith [%here] "die"
    else aux (n + 1, (id mod 2 == 1) :: res) (id / 2)
  in
  aux (0, []) id

let feature_vec_to_prop (ftab : feature_tab) vec =
  let props =
    List.map (fun (b, p) -> if b then p else Not p) @@ List.combine vec ftab
  in
  match props with [] -> mk_true | _ -> And props

let feature_id_to_prop (ftab : feature_tab) id =
  feature_vec_to_prop ftab @@ feature_id_to_vec (List.length ftab) id
