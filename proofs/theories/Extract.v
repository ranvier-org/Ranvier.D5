From stdpp Require Import prelude.
From Stdlib Require Extraction.
From Stdlib Require Import Arith Lia List Bool ZArith.
From RanvierD5.Pure Require Import DataScale Spatial SamplingSelection GeometryGeo TransitionWorker.
Import ListNotations.

Local Arguments Nat.eqb : simpl never.
Open Scope Z_scope.

(** * Executable runners for conformance and differential testing *)

(** ** 1. `Data.bin` + `Data.join` runner *)

Definition observe_bin_and_join
    (edges values : list Z) (prev_keys next_keys : list nat)
    : list (option nat) * join_model :=
  (List.map (bin_index edges) values, data_join_keys prev_keys next_keys).

(** ** 2. `Scale.linear` (`Scale.clamp` + `Scale.map`) + `Scale.nice` runner *)

Fixpoint pow10_unit (fuel : nat) (span count cur : Z) : Z :=
  match fuel with
  | 0%nat => cur
  | S f =>
    if Z.leb (count * cur * 10) span then pow10_unit f span count (cur * 10)
    else cur
  end.

Definition nice_step_exact (lo hi count : Z) : Z :=
  let span := hi - lo in
  if Z.leb span 0 || Z.leb count 0 then 0
  else
    let unit := pow10_unit 6%nat span count 1 in
    let denom := count * unit in
    let lhs := span * span in
    let rhs := denom * denom in
    let factor :=
      if Z.geb lhs (50 * rhs) then 10
      else if Z.geb lhs (10 * rhs) then 5
      else if Z.geb lhs (2 * rhs) then 2
      else 1 in
    factor * unit.

Definition linear_map_exact (dom_lo dom_hi rng_lo rng_hi : Z) (clamped : bool) (v : Z) : Z :=
  if Z.eqb dom_lo dom_hi then (rng_lo + rng_hi) / 2
  else
    let raw := rng_lo + ((v - dom_lo) * (rng_hi - rng_lo)) / (dom_hi - dom_lo) in
    if clamped then clamp_z rng_lo rng_hi raw else raw.

Definition observe_scale_and_nice
    (dom_lo dom_hi rng_lo rng_hi : Z) (clamped : bool) (queries : list Z)
    (nice_lo nice_hi nice_count : Z)
    : list Z * (Z * Z) :=
  let mapped := List.map (linear_map_exact dom_lo dom_hi rng_lo rng_hi clamped) queries in
  let lo := Z.min nice_lo nice_hi in
  let hi := Z.max nice_lo nice_hi in
  let step := nice_step_exact lo hi nice_count in
  let '(nlo, nhi) := if Z.leb step 0 then (lo, hi) else nice_direct lo hi step in
  let nice_dom := if Z.leb nice_lo nice_hi then (nlo, nhi) else (nhi, nlo) in
  (mapped, nice_dom).

(** ** 3. `Spatial` & `Quadtree` `within` + `nearest` runner *)

Fixpoint insert_nat_sorted (x : nat) (xs : list nat) : list nat :=
  match xs with
  | [] => [x]
  | y :: rest =>
    if Nat.leb x y then x :: xs else y :: insert_nat_sorted x rest
  end.

Fixpoint sort_nats (xs : list nat) : list nat :=
  match xs with
  | [] => []
  | x :: rest => insert_nat_sorted x (sort_nats rest)
  end.

Definition entry_lt_x (a b : pt2 * nat) : bool :=
  let '(pa, oa) := a in
  let '(pb, ob) := b in
  Z.ltb (px pa) (px pb) || (Z.eqb (px pa) (px pb) && Nat.ltb oa ob).

Definition entry_lt_y (a b : pt2 * nat) : bool :=
  let '(pa, oa) := a in
  let '(pb, ob) := b in
  Z.ltb (py pa) (py pb) || (Z.eqb (py pa) (py pb) && Nat.ltb oa ob).

Fixpoint insert_entry (sx : bool) (x : pt2 * nat) (xs : list (pt2 * nat)) : list (pt2 * nat) :=
  match xs with
  | [] => [x]
  | y :: rest =>
    let le := if sx then negb (entry_lt_x y x) else negb (entry_lt_y y x) in
    if le then x :: xs else y :: insert_entry sx x rest
  end.

Fixpoint sort_entries (sx : bool) (xs : list (pt2 * nat)) : list (pt2 * nat) :=
  match xs with
  | [] => []
  | x :: rest => insert_entry sx x (sort_entries sx rest)
  end.

Fixpoint build_kdtree (fuel : nat) (sx : bool) (entries : list (pt2 * nat)) : kdtree :=
  match fuel, entries with
  | 0%nat, _ | _, [] => KdEmpty
  | S f, _ =>
    let sorted := sort_entries sx entries in
    let mid := Nat.div (length sorted) 2%nat in
    match nth_error sorted mid with
    | None => KdEmpty
    | Some (p, o) =>
      let left_es := firstn mid sorted in
      let right_es := skipn (S mid) sorted in
      KdNode sx p o (build_kdtree f (negb sx) left_es) (build_kdtree f (negb sx) right_es)
    end
  end.

Fixpoint index_pts (idx : nat) (pts : list pt2) : list (pt2 * nat) :=
  match pts with
  | [] => []
  | p :: rest => (p, idx) :: index_pts (S idx) rest
  end.

Fixpoint kd_nearest_acc (q : pt2) (t : kdtree) (best : option (Z * nat)) : option (Z * nat) :=
  match t with
  | KdEmpty => best
  | KdNode sx p o l r =>
    let d2 := dist_sq q p in
    let best1 :=
      match best with
      | None => Some (d2, o)
      | Some (bd2, bo) =>
        if Z.ltb d2 bd2 || (Z.eqb d2 bd2 && Nat.ltb o bo) then Some (d2, o) else best
      end in
    let delta := if sx then px q - px p else py q - py p in
    if Z.ltb delta 0 then
      let best2 := kd_nearest_acc q l best1 in
      let search_far :=
        match best2 with
        | None => true
        | Some (bd2, _) => Z.leb (delta * delta) bd2
        end in
      if search_far then kd_nearest_acc q r best2 else best2
    else
      let best2 := kd_nearest_acc q r best1 in
      let search_far :=
        match best2 with
        | None => true
        | Some (bd2, _) => Z.leb (delta * delta) bd2
        end in
      if search_far then kd_nearest_acc q l best2 else best2
  end.

Definition observe_spatial
    (pts : list pt2) (query_box : aabb) (query_pt : pt2)
    : list nat * option nat :=
  let tree := build_kdtree (S (length pts)) true (index_pts 0 pts) in
  let hits := sort_nats (List.map snd (kd_within query_box tree)) in
  let near_ord :=
    match kd_nearest_acc query_pt tree None with
    | None => None
    | Some (_, o) => Some o
    end in
  (hits, near_ord).

(** ** 4. `Sampling.minMax` + `Selection.reconcile` + `Recipe.key` runner *)

Fixpoint scan_bucket_extrema
    (idx stop : nat) (ys : list Z) (lo_i hi_i : nat) (lo_v hi_v : Z)
    : nat * nat :=
  match stop with
  | 0%nat => (lo_i, hi_i)
  | S rem =>
    match nth_error ys idx with
    | None => (lo_i, hi_i)
    | Some v =>
      let '(lo_i', lo_v') := if Z.ltb v lo_v then (idx, v) else (lo_i, lo_v) in
      let '(hi_i', hi_v') := if Z.gtb v hi_v then (idx, v) else (hi_i, hi_v) in
      scan_bucket_extrema (S idx) rem ys lo_i' hi_i' lo_v' hi_v'
    end
  end.

Fixpoint compute_minmax_buckets_fuel
    (rem b buckets interior : nat) (ys : list Z) : list (nat * nat) :=
  match rem with
  | 0%nat => []
  | S r =>
    let first := (1 + Nat.div (b * interior) buckets)%nat in
    let last_ := (1 + Nat.div ((S b) * interior) buckets)%nat in
    let v0 := match nth_error ys first with Some v => v | None => 0 end in
    let pair := scan_bucket_extrema (S first) (last_ - S first)%nat ys first first v0 v0 in
    pair :: compute_minmax_buckets_fuel r (S b) buckets interior ys
  end.

Fixpoint seq_nats (start count : nat) : list nat :=
  match count with
  | 0%nat => []
  | S c => start :: seq_nats (S start) c
  end.

Definition run_minmax_exact (target : nat) (ys : list Z) : list nat :=
  let n := length ys in
  if Nat.leb n target then seq_nats 0 n
  else
    let buckets := Nat.div (target - 2)%nat 2%nat in
    let interior := (n - 2)%nat in
    let pairs := compute_minmax_buckets_fuel buckets 0%nat buckets interior ys in
    minmax_indices n pairs.

Definition observe_sampling_selection
    (target : nat) (ys : list Z)
    (source_ids visible_ids : list nat) (st : sel_state)
    (key_parts : list (list nat))
    : list nat * sel_state * list nat :=
  (run_minmax_exact target ys,
   selection_reconcile source_ids visible_ids st,
   encode_length_prefixed key_parts).

(** ** 5. `Contour.compute` shell count + `Sankey.moveNode` + `GeoMeasure.bounds` polar-cap runner *)

(** On a `2 × 2` grid surrounded by unobserved cells, `Contour.compute` emits closed shells:
    0 shells for `code = 0`, 1 or 2 shells on saddle configurations `5`/`10` depending on
    `saddle_pos` (`(a - t)*(c - t) - (b - t)*(d - t) > 0`), and 1 shell for all other `code < 16`. *)
Definition contour_2x2_shells (saddle_pos : bool) (code : nat) : nat :=
  match code with
  | 0%nat => 0%nat
  | 5%nat => if saddle_pos then 1%nat else 2%nat
  | 10%nat => if saddle_pos then 2%nat else 1%nat
  | _ => if Nat.ltb code 16%nat then 1%nat else 0%nat
  end.

Definition observe_contour_sankey_geo
    (saddle_pos : bool) (code : nat)
    (lo_b hi_b y0 target_y link_y : Z)
    (ring_start : sph_pt) (ring_rest : list sph_pt)
    : nat * (Z * Z) * Z :=
  let shells := contour_2x2_shells saddle_pos code in
  let y' := sankey_clamp_move lo_b hi_b target_y in
  let link_y' := link_y + (y' - y0) in
  let winding := canonical_seam_winding ring_start (ring_rest ++ [ring_start]) in
  (shells, (y', link_y'), winding).

(** ** 6. `Transition` + `Gesture` + `ReactiveForce` + `InputAdapter` runner *)

Fixpoint run_gesture_events (thresh : Z) (evs : list g_event) (st : g_state)
    : g_state * list g_cmd :=
  match evs with
  | [] => (st, [])
  | e :: rest =>
    let '(st1, c1) := gesture_step_lasso thresh e st in
    let '(st2, c2) := run_gesture_events thresh rest st1 in
    (st2, c1 ++ c2)
  end.

Fixpoint collect_lassos (cmds : list g_cmd) : list (list Z) :=
  match cmds with
  | [] => []
  | GSelectLasso pts :: rest => pts :: collect_lassos rest
  | _ :: rest => collect_lassos rest
  end.

Fixpoint count_releases (cmds : list g_cmd) : nat :=
  match cmds with
  | [] => 0%nat
  | GRelease _ :: rest => S (count_releases rest)
  | _ :: rest => count_releases rest
  end.

Definition observe_transition_gesture_worker
    (s0 : lin_segment) (retarget_t next_val sample_t : Z)
    (lasso_thresh : Z) (evs : list g_event)
    (stale_worker : bool) (sync_val worker_val : nat) (req_ok : bool)
    (root_id : nat) (dom_chain : list dom_tag)
    : Z * (nat * nat * list (list Z)) * (nat * bool) * option (list nat) :=
  let s1 := retarget_step retarget_t next_val (seg_duration s0) s0 in
  let sampled := sample_step sample_t s1 in
  let '(gst, gcmds) :=
    run_gesture_events lasso_thresh evs
      {| gs_pointers := []; gs_target := None; gs_lasso := [] |} in
  let fc0 := {| fc_revision := 0%nat; fc_current := 0%nat; fc_frame_queued := false |} in
  let fc1 := force_dispatch_sync sync_val fc0 in
  let worker_rev := if stale_worker then 0%nat else 1%nat in
  let fc2 := force_apply_worker_fenced worker_rev worker_val fc1 in
  let '(_, fc3) := publish_frame_safe req_ok fc2 in
  let mark_path := walk_to_root_svg root_id dom_chain [] in
  (sampled,
   (length (gs_pointers gst), count_releases gcmds, collect_lassos gcmds),
   (fc_current fc3, fc_frame_queued fc3),
   mark_path).

(** ** OCaml Extraction *)

Extraction Language OCaml.
Set Extraction Output Directory ".".

Extraction "engine.ml"
  observe_bin_and_join
  observe_scale_and_nice
  observe_spatial
  observe_sampling_selection
  observe_contour_sankey_geo
  observe_transition_gesture_worker.
