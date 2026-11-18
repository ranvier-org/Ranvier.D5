
type bool =
| True
| False

val negb : bool -> bool

type nat =
| O
| S of nat

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

val fst : ('a1, 'a2) prod -> 'a1

val snd : ('a1, 'a2) prod -> 'a2

type 'a list =
| Nil
| Cons of 'a * 'a list

val length : 'a1 list -> nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

val compOpp : comparison -> comparison

val add : nat -> nat -> nat

val mul : nat -> nat -> nat

val sub : nat -> nat -> nat

module Nat :
 sig
  val pred : nat -> nat

  val eqb : nat -> nat -> bool

  val leb : nat -> nat -> bool

  val ltb : nat -> nat -> bool

  val divmod : nat -> nat -> nat -> nat -> (nat, nat) prod

  val div : nat -> nat -> nat
 end

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val firstn : nat -> 'a1 list -> 'a1 list

val skipn : nat -> 'a1 list -> 'a1 list

val nth_error : 'a1 list -> nat -> 'a1 option

val rev : 'a1 list -> 'a1 list

val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list

type positive =
| XI of positive
| XO of positive
| XH

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val pred_double : positive -> positive

  val mul : positive -> positive -> positive

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val eqb : positive -> positive -> bool
 end

module Z :
 sig
  val double : z -> z

  val succ_double : z -> z

  val pred_double : z -> z

  val pos_sub : positive -> positive -> z

  val add : z -> z -> z

  val opp : z -> z

  val sub : z -> z -> z

  val mul : z -> z -> z

  val compare : z -> z -> comparison

  val leb : z -> z -> bool

  val ltb : z -> z -> bool

  val eqb : z -> z -> bool

  val max : z -> z -> z

  val min : z -> z -> z

  val pos_div_eucl : positive -> z -> (z, z) prod

  val div_eucl : z -> z -> (z, z) prod

  val div : z -> z -> z

  val geb : z -> z -> bool

  val gtb : z -> z -> bool

  val abs : z -> z
 end

val bisect_right_list : z list -> z -> nat

val last_nonempty : z -> z list -> z

val bin_index : z list -> z -> nat option

val mem_key : nat -> nat list -> bool

type join_model = { jm_enter : nat list; jm_update : nat list;
                    jm_exit : nat list }

val data_join_keys : nat list -> nat list -> join_model

val clamp_z : z -> z -> z -> z

val floor_step : z -> z -> z

val ceil_step : z -> z -> z

val nice_direct : z -> z -> z -> (z, z) prod

type pt2 = { px : z; py : z }

type aabb = { min_x : z; max_x : z; min_y : z; max_y : z }

val in_aabb : aabb -> pt2 -> bool

val dist_sq : pt2 -> pt2 -> z

type kdtree =
| KdEmpty
| KdNode of bool * pt2 * nat * kdtree * kdtree

val kd_within : aabb -> kdtree -> (pt2, nat) prod list

val bucket_pair : nat -> nat -> nat list

val emit_interior_buckets : (nat, nat) prod list -> nat -> nat list

val minmax_indices : nat -> (nat, nat) prod list -> nat list

val mem_id : nat -> nat list -> bool

val dedup_keep_first : nat list -> nat list -> nat list

type sel_state = { sel_available : nat list; sel_selected : nat list;
                   sel_focused : nat option; sel_hovered : nat option }

val filter_opt : nat list -> nat option -> nat option

val selection_reconcile : nat list -> nat list -> sel_state -> sel_state

val encode_length_prefixed : nat list list -> nat list

type sph_pt = { s_lon : z; s_lat : z }

val canonical_pole : sph_pt -> sph_pt

val seam_step : sph_pt -> sph_pt -> z

val canonical_seam_winding : sph_pt -> sph_pt list -> z

val sankey_clamp_move : z -> z -> z -> z

type lin_segment = { seg_from : z; seg_target : z; seg_started : z;
                     seg_duration : z }

val sample_step : z -> lin_segment -> z

val retarget_step : z -> z -> z -> lin_segment -> lin_segment

type g_event =
| GDown of nat * z * nat option
| GMove of nat * z
| GUp of nat * z
| GCancel
| GNodeExit of nat

type g_cmd =
| GCapture of nat
| GRelease of nat
| GSelectLasso of z list

type g_state = { gs_pointers : (nat, z) prod list; gs_target : nat option;
                 gs_lasso : z list }

val assoc_mem : nat -> (nat, z) prod list -> bool

val assoc_put : nat -> z -> (nat, z) prod list -> (nat, z) prod list

val assoc_remove : nat -> (nat, z) prod list -> (nat, z) prod list

val gesture_cancel : g_state -> (g_state, g_cmd list) prod

val lasso_append_fixed : z -> z -> z list -> z list

val gesture_step_lasso : z -> g_event -> g_state -> (g_state, g_cmd list) prod

type force_ctrl = { fc_revision : nat; fc_current : nat;
                    fc_frame_queued : bool }

val force_dispatch_sync : nat -> force_ctrl -> force_ctrl

val force_apply_worker_fenced : nat -> nat -> force_ctrl -> force_ctrl

val publish_frame_safe : bool -> force_ctrl -> (bool, force_ctrl) prod

type dom_tag =
| DomPlain of nat
| DomMark of nat * nat
| DomGroup of nat * nat

val dom_node_id : dom_tag -> nat

val walk_to_root_svg : nat -> dom_tag list -> nat list -> nat list option

val observe_bin_and_join :
  z list -> z list -> nat list -> nat list -> (nat option list, join_model)
  prod

val pow10_unit : nat -> z -> z -> z -> z

val nice_step_exact : z -> z -> z -> z

val linear_map_exact : z -> z -> z -> z -> bool -> z -> z

val observe_scale_and_nice :
  z -> z -> z -> z -> bool -> z list -> z -> z -> z -> (z list, (z, z) prod)
  prod

val insert_nat_sorted : nat -> nat list -> nat list

val sort_nats : nat list -> nat list

val entry_lt_x : (pt2, nat) prod -> (pt2, nat) prod -> bool

val entry_lt_y : (pt2, nat) prod -> (pt2, nat) prod -> bool

val insert_entry :
  bool -> (pt2, nat) prod -> (pt2, nat) prod list -> (pt2, nat) prod list

val sort_entries : bool -> (pt2, nat) prod list -> (pt2, nat) prod list

val build_kdtree : nat -> bool -> (pt2, nat) prod list -> kdtree

val index_pts : nat -> pt2 list -> (pt2, nat) prod list

val kd_nearest_acc :
  pt2 -> kdtree -> (z, nat) prod option -> (z, nat) prod option

val observe_spatial : pt2 list -> aabb -> pt2 -> (nat list, nat option) prod

val scan_bucket_extrema :
  nat -> nat -> z list -> nat -> nat -> z -> z -> (nat, nat) prod

val compute_minmax_buckets_fuel :
  nat -> nat -> nat -> nat -> z list -> (nat, nat) prod list

val seq_nats : nat -> nat -> nat list

val run_minmax_exact : nat -> z list -> nat list

val observe_sampling_selection :
  nat -> z list -> nat list -> nat list -> sel_state -> nat list list ->
  ((nat list, sel_state) prod, nat list) prod

val contour_2x2_shells : bool -> nat -> nat

val observe_contour_sankey_geo :
  bool -> nat -> z -> z -> z -> z -> z -> sph_pt -> sph_pt list -> ((nat, (z,
  z) prod) prod, z) prod

val run_gesture_events :
  z -> g_event list -> g_state -> (g_state, g_cmd list) prod

val collect_lassos : g_cmd list -> z list list

val count_releases : g_cmd list -> nat

val observe_transition_gesture_worker :
  lin_segment -> z -> z -> z -> z -> g_event list -> bool -> nat -> nat ->
  bool -> nat -> dom_tag list -> (((z, ((nat, nat) prod, z list list) prod)
  prod, (nat, bool) prod) prod, nat list option) prod
