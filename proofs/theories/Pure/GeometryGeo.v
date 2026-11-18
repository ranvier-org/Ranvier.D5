From stdpp Require Import prelude.
From Stdlib Require Import Arith Lia List Bool ZArith.
Import ListNotations.

Local Arguments Nat.eqb : simpl never.
Open Scope Z_scope.

(**md # Marching-squares topology (`Contour`), Cohen–Sutherland SIMD outcodes, spherical pole invariance (`GeoMeasure`), and `Sankey.moveNode`

Models the geometric, topological, and geographic invariants in `Ranvier.D5.Contour`, `Ranvier.D5.Sankey`, `Ranvier.D5.Geo.GeoRectangle`, and `Ranvier.D5.Geo.GeoMeasure`. *)

(** ** 1. Marching-squares cell boundary degree balance & homogeneous SIMD cell skip *)

(** The four sides of a marching-squares cell: 0=Top, 1=Right, 2=Bottom, 3=Left. *)
Inductive cell_side := SideTop | SideRight | SideBottom | SideLeft.

Definition side_eqb (a b : cell_side) : bool :=
  match a, b with
  | SideTop, SideTop | SideRight, SideRight | SideBottom, SideBottom | SideLeft, SideLeft => true
  | _, _ => false
  end.

Definition cell_segments (saddle_pos : bool) (code : nat) : list (cell_side * cell_side) :=
  match code with
  | 0%nat | 15%nat => []
  | 1%nat => [(SideTop, SideLeft)]
  | 2%nat => [(SideRight, SideTop)]
  | 3%nat => [(SideRight, SideLeft)]
  | 4%nat => [(SideBottom, SideRight)]
  | 5%nat =>
    if saddle_pos then [(SideTop, SideRight); (SideBottom, SideLeft)]
    else [(SideTop, SideLeft); (SideBottom, SideRight)]
  | 6%nat => [(SideBottom, SideTop)]
  | 7%nat => [(SideBottom, SideLeft)]
  | 8%nat => [(SideLeft, SideBottom)]
  | 9%nat => [(SideTop, SideBottom)]
  | 10%nat =>
    if saddle_pos then [(SideLeft, SideTop); (SideRight, SideBottom)]
    else [(SideRight, SideTop); (SideLeft, SideBottom)]
  | 11%nat => [(SideRight, SideBottom)]
  | 12%nat => [(SideLeft, SideRight)]
  | 13%nat => [(SideTop, SideRight)]
  | 14%nat => [(SideLeft, SideTop)]
  | _ => []
  end.

Fixpoint count_in_side (s : cell_side) (segs : list (cell_side * cell_side)) : nat :=
  match segs with
  | [] => 0%nat
  | (a, _) :: rest => (if side_eqb a s then 1%nat else 0%nat) + count_in_side s rest
  end.

Fixpoint count_out_side (s : cell_side) (segs : list (cell_side * cell_side)) : nat :=
  match segs with
  | [] => 0%nat
  | (_, b) :: rest => (if side_eqb b s then 1%nat else 0%nat) + count_out_side s rest
  end.

Fixpoint no_self_edge (segs : list (cell_side * cell_side)) : bool :=
  match segs with
  | [] => true
  | (a, b) :: rest => negb (side_eqb a b) && no_self_edge rest
  end.

(**md
**Every marching-squares cell configuration is loop-free, uses each side at most once, and skips homogeneous `0`/`15` cells.**

Across all 16 corner masks (`code < 16`) and both bilinear saddle decisions (`saddle_high`), `cell_segments` emits no self-edges, enters and exits each of the four sides at most once, and emits `[]` iff `code = 0` or `code = 15` (justifying SIMD 4-cell homogeneous skipping).

- [saddle_high:] bilinear center-value saddle decision (`center >= threshold`).
- [code:] 4-bit corner classification in `0 .. 15`.
- [s:] cell side tested for entry/exit multiplicity.
*)
Theorem marching_squares_cell_degree_balanced (saddle_high : bool) (code : nat) (s : cell_side) :
  (code < 16)%nat ->
  let segs := cell_segments saddle_high code in
  no_self_edge segs = true /\
  (count_in_side s segs <= 1)%nat /\
  (count_out_side s segs <= 1)%nat /\
  (segs = [] <-> code = 0%nat \/ code = 15%nat).
Proof.
  intros Hlt. cbv zeta.
  do 16 (destruct code as [|code]; [
    destruct saddle_high; destruct s; simpl;
    repeat split; try reflexivity; try lia;
    (intros Heq; try discriminate; left; reflexivity) ||
    (intros [H0 | H15]; try lia; reflexivity)
  |]).
  lia.
Qed.

(** ** 2. 4-bit Cohen–Sutherland SIMD outcode trivial accept and reject (`GeoRectangle.clipSegment`) *)

Record rect2 := {
  r_left : Z;
  r_top : Z;
  r_right : Z;
  r_bottom : Z
}.

Record outcode4 := {
  oc_left : bool;
  oc_right : bool;
  oc_top : bool;
  oc_bottom : bool
}.

Definition compute_outcode (r : rect2) (x y : Z) : outcode4 :=
  {| oc_left := Z.ltb x (r_left r);
     oc_right := Z.gtb x (r_right r);
     oc_top := Z.ltb y (r_top r);
     oc_bottom := Z.gtb y (r_bottom r) |}.

Definition outcode_or_zero (a b : outcode4) : bool :=
  negb (oc_left a || oc_left b || oc_right a || oc_right b ||
        oc_top a || oc_top b || oc_bottom a || oc_bottom b).

Definition outcode_and_nonzero (a b : outcode4) : bool :=
  (oc_left a && oc_left b) || (oc_right a && oc_right b) ||
  (oc_top a && oc_top b) || (oc_bottom a && oc_bottom b).

Definition pt_in_rect (r : rect2) (x y : Z) : Prop :=
  r_left r <= x /\ x <= r_right r /\ r_top r <= y /\ y <= r_bottom r.

(**md
**SIMD Cohen–Sutherland outcode masks soundly decide trivial accept and trivial half-plane reject.**

When `(code_a | code_b) = 0`, both segment endpoints lie inside `r` (`GeoRectangle` trivial accept); when `(code_a & code_b) <> 0`, every convex combination `p = a + t*(b - a)` (`0 <= t <= 1`, modeled via scaled integer `0 <= k <= denom`) lies strictly outside `r` (trivial reject).

- [r:] clip rectangle `[r_left, r_right] × [r_top, r_bottom]`.
- [ax:] start X coordinate.
- [ay:] start Y coordinate.
- [bx:] end X coordinate.
- [by_:] end Y coordinate.
- [k:] interpolation numerator (`0 <= k <= denom`).
- [denom:] positive interpolation denominator (`0 < denom`).
*)
Theorem cohen_sutherland_outcode_trivial_cases_sound
    (r : rect2) (ax ay bx by_ k denom : Z) :
  0 < denom ->
  0 <= k -> k <= denom ->
  let ca := compute_outcode r ax ay in
  let cb := compute_outcode r bx by_ in
  (outcode_or_zero ca cb = true -> pt_in_rect r ax ay /\ pt_in_rect r bx by_) /\
  (outcode_and_nonzero ca cb = true ->
    let px_scaled := (denom - k) * ax + k * bx in
    let py_scaled := (denom - k) * ay + k * by_ in
    px_scaled < denom * r_left r \/ px_scaled > denom * r_right r \/
    py_scaled < denom * r_top r \/ py_scaled > denom * r_bottom r).
Proof.
  intros Hden Hk0 Hkd. cbv zeta.
  unfold compute_outcode, outcode_or_zero, outcode_and_nonzero, pt_in_rect; simpl.
  rewrite Z.gtb_ltb, Z.gtb_ltb, Z.gtb_ltb, Z.gtb_ltb.
  split.
  - intros Hor.
    apply negb_true_iff in Hor.
    repeat (apply orb_false_elim in Hor as [Hor ?]).
    apply Z.ltb_ge in Hor, H, H0, H1, H2, H3, H4, H5.
    repeat split; lia.
  - intros Hand.
    apply orb_true_iff in Hand as [Hand | Hand];
    [apply orb_true_iff in Hand as [Hand | Hand];
      [apply orb_true_iff in Hand as [Hl | Hr] | ] | ].
    + apply andb_prop in Hl as [H1 H2]. apply Z.ltb_lt in H1, H2. left. nia.
    + apply andb_prop in Hr as [H1 H2]. apply Z.ltb_lt in H1, H2. right; left. nia.
    + apply andb_prop in Hand as [H1 H2]. apply Z.ltb_lt in H1, H2. right; right; left. nia.
    + apply andb_prop in Hand as [H1 H2]. apply Z.ltb_lt in H1, H2. right; right; right. nia.
Qed.

(** ** 3. Spherical pole canonicalization invariance (`GeoMeasure.bounds` Issue #66) *)

Record sph_pt := {
  s_lon : Z;
  s_lat : Z
}.

Definition canonical_pole (p : sph_pt) : sph_pt :=
  if Z.eqb (Z.abs (s_lat p)) 90 then {| s_lon := 0; s_lat := s_lat p |} else p.

Definition seam_step (a b : sph_pt) : Z :=
  let d := s_lon b - s_lon a in
  if Z.gtb d 180 then 1
  else if Z.ltb d (-180) then -1
  else 0.

Fixpoint raw_seam_winding (prev : sph_pt) (pts : list sph_pt) : Z :=
  match pts with
  | [] => 0
  | p :: rest => seam_step prev p + raw_seam_winding p rest
  end.

Fixpoint canonical_seam_winding (prev : sph_pt) (pts : list sph_pt) : Z :=
  let prev_c := canonical_pole prev in
  match pts with
  | [] => 0
  | p :: rest =>
    let pc := canonical_pole p in
    seam_step prev_c pc + canonical_seam_winding pc rest
  end.

Definition same_sphere_point (p q : sph_pt) : Prop :=
  s_lat p = s_lat q /\ (Z.abs (s_lat p) = 90 \/ s_lon p = s_lon q).

Lemma canonical_pole_respects_sphere (p q : sph_pt) :
  same_sphere_point p q ->
  canonical_pole p = canonical_pole q.
Proof.
  intros [Hlat Hlon]. unfold canonical_pole. rewrite Hlat.
  destruct Hlon as [Hpole | Heq].
  - rewrite Hlat in Hpole. rewrite ( proj2 (Z.eqb_eq _ _) Hpole ). reflexivity.
  - destruct p, q; simpl in *; subst; reflexivity.
Qed.

(**md
**Canonicalizing pole longitudes `(lon, ±90) ↦ (0, ±90)` makes `seamWinding` invariant across equivalent spherical rings.**

Any two vertex sequences `xs` and `ys` representing the exact same points on the unit sphere (`Forall2 same_sphere_point`) yield identical `canonical_seam_winding`, eliminating arbitrary dummy-longitude sensitivity at the poles.

- [p0:] starting ring vertex (`xs` representation).
- [q0:] starting ring vertex (`ys` representation).
- [xs:] remaining vertices with arbitrary dummy pole longitudes.
- [ys:] remaining vertices representing the same spherical points.
*)
Theorem pole_turn_seam_winding_invariant
    (p0 q0 : sph_pt) (xs ys : list sph_pt) :
  same_sphere_point p0 q0 ->
  Forall2 same_sphere_point xs ys ->
  canonical_seam_winding p0 xs = canonical_seam_winding q0 ys.
Proof.
  intros H0 Hrel. revert p0 q0 H0.
  induction Hrel as [|x y xs' ys' Hxy Hrest IH]; intros p0 q0 H0; simpl.
  - reflexivity.
  - rewrite (canonical_pole_respects_sphere p0 q0 H0).
    rewrite (canonical_pole_respects_sphere x y Hxy).
    f_equal.
    apply IH. split; [reflexivity | right; reflexivity].
Qed.

(**md
**Raw `seamWinding` without pole canonicalization produces `-1` instead of `0` on `[(10, 80); (-170, 90); (20, 80)]` (Issue #66).**

Replacing `(0, 90)` with the identical North Pole point `(-170, 90)` flips `raw_seam_winding` from `0` to `1` / `-1`, whereas `canonical_seam_winding` evaluates to `0` on both.

- [u:] unit witness triggering the counterexample.
*)
Theorem naive_pole_seam_winding_falsified (u : unit) :
  let _ := u in
  let a := {| s_lon := 20; s_lat := 80 |} in
  let b_canon := {| s_lon := 0; s_lat := 90 |} in
  let b_dummy := {| s_lon := -170; s_lat := 90 |} in
  let c := {| s_lon := 10; s_lat := 80 |} in
  same_sphere_point b_canon b_dummy /\
  raw_seam_winding a [b_canon; c; a] = 0 /\
  raw_seam_winding a [b_dummy; c; a] = -1 /\
  canonical_seam_winding a [b_dummy; c; a] = 0.
Proof.
  simpl. repeat split; try reflexivity; left; reflexivity.
Qed.

(** ** 4. `Sankey.moveNode` neighbor/viewport clamping & link endpoint shift *)

Definition sankey_clamp_move (lo_bound hi_bound target_y : Z) : Z :=
  Z.max lo_bound (Z.min hi_bound target_y).

(**md
**`Sankey.moveNode` preserves vertical non-overlap bounds and shifts incident link endpoints by exact `y' - y0`.**

When `lo_bound <= y0 <= hi_bound` (current node sits validly between its layer neighbors and viewport), clamping `target_y` to `[lo_bound, hi_bound]` keeps `lo_bound <= y' <= hi_bound` and shifts every incident link endpoint `link_y` by exact `delta = y' - y0`, preserving relative offset `link_y - y0`.

- [lo_bound:] upper neighbor bottom plus `NodePadding` (or viewport top).
- [hi_bound:] lower neighbor top minus `NodePadding + height` (or viewport bottom minus `height`).
- [y0:] node's original Y position.
- [target_y:] requested drag Y position.
- [link_y:] incident link endpoint Y before moving the node.
*)
Theorem sankey_move_node_clamped_in_bounds
    (lo_bound hi_bound y0 target_y link_y : Z) :
  lo_bound <= y0 -> y0 <= hi_bound ->
  let y' := sankey_clamp_move lo_bound hi_bound target_y in
  let delta := y' - y0 in
  let link_y' := link_y + delta in
  lo_bound <= y' /\ y' <= hi_bound /\ link_y' - y' = link_y - y0.
Proof.
  intros Hlo Hhi. cbv zeta. unfold sankey_clamp_move.
  repeat split; lia.
Qed.

(** ** 5. Batch D2 refinements: SIMD homogeneous `Contour` strip skip & order-preserving 64-bit `Edge` packing *)

Definition corner_code (threshold a b c d : Z) : nat :=
  (if Z.geb a threshold then 1%nat else 0%nat) +
  (if Z.geb b threshold then 2%nat else 0%nat) +
  (if Z.geb c threshold then 4%nat else 0%nat) +
  (if Z.geb d threshold then 8%nat else 0%nat).

Lemma forall_nth_z (P : Z -> Prop) (xs : list Z) (i : nat) :
  Forall P xs ->
  (i < length xs)%nat ->
  P (nth i xs 0).
Proof.
  intros Hall. revert i.
  induction Hall as [|x rest Hx Hrest IH]; intros [|i] Hlt; simpl in *; try lia.
  - exact Hx.
  - apply IH. lia.
Qed.

(**md
**SIMD homogeneous row-strip comparison soundly skips `W - 1` adjacent marching-squares cells (`Contour.compute`).**

Whenever a `W`-wide SIMD load on `r0` (row `y`) and `r1` (row `y + 1`) satisfies either all `< threshold` (`code = 0`) or all `>= threshold` (`code = 15`), every adjacent cell `j` (`S j < length r0`, `S j < length r1`) produces `cell_segments = []` regardless of saddle decision, allowing `Vector512` to skip 7 cells and `Vector256` to skip 3 cells per vector instruction.

- [threshold:] contour threshold level.
- [r0:] consecutive observed grid values along row `y`.
- [r1:] consecutive observed grid values along row `y + 1`.
- [saddle_high:] saddle decision at cell `j`.
- [j:] cell offset within the loaded strip (`S j < length r0`, `S j < length r1`).
*)
Theorem marching_squares_homogeneous_strip_skip_sound
    (threshold : Z) (r0 r1 : list Z) (saddle_high : bool) (j : nat) :
  ((Forall (fun v => v < threshold) r0 /\ Forall (fun v => v < threshold) r1) \/
   (Forall (fun v => threshold <= v) r0 /\ Forall (fun v => threshold <= v) r1)) ->
  (S j < length r0)%nat ->
  (S j < length r1)%nat ->
  cell_segments saddle_high
    (corner_code threshold (nth j r0 0) (nth (S j) r0 0) (nth (S j) r1 0) (nth j r1 0)) = [].
Proof.
  intros Hhom Hj0 Hj1.
  unfold corner_code.
  destruct Hhom as [[H0 H1] | [H0 H1]].
  - pose proof (forall_nth_z (fun v => v < threshold) r0 j H0 ltac:(lia)) as Ha.
    pose proof (forall_nth_z (fun v => v < threshold) r0 (S j) H0 Hj0) as Hb.
    pose proof (forall_nth_z (fun v => v < threshold) r1 (S j) H1 Hj1) as Hc.
    pose proof (forall_nth_z (fun v => v < threshold) r1 j H1 ltac:(lia)) as Hd.
    cbv beta in Ha, Hb, Hc, Hd.
    rewrite !Z.geb_leb.
    destruct (Z.leb threshold (nth j r0 0)) eqn:E1; [apply Z.leb_le in E1; lia |].
    destruct (Z.leb threshold (nth (S j) r0 0)) eqn:E2; [apply Z.leb_le in E2; lia |].
    destruct (Z.leb threshold (nth (S j) r1 0)) eqn:E3; [apply Z.leb_le in E3; lia |].
    destruct (Z.leb threshold (nth j r1 0)) eqn:E4; [apply Z.leb_le in E4; lia |].
    simpl. reflexivity.
  - pose proof (forall_nth_z (fun v => threshold <= v) r0 j H0 ltac:(lia)) as Ha.
    pose proof (forall_nth_z (fun v => threshold <= v) r0 (S j) H0 Hj0) as Hb.
    pose proof (forall_nth_z (fun v => threshold <= v) r1 (S j) H1 Hj1) as Hc.
    pose proof (forall_nth_z (fun v => threshold <= v) r1 j H1 ltac:(lia)) as Hd.
    cbv beta in Ha, Hb, Hc, Hd.
    rewrite !Z.geb_leb.
    destruct (Z.leb threshold (nth j r0 0)) eqn:E1; [| apply Z.leb_gt in E1; lia].
    destruct (Z.leb threshold (nth (S j) r0 0)) eqn:E2; [| apply Z.leb_gt in E2; lia].
    destruct (Z.leb threshold (nth (S j) r1 0)) eqn:E3; [| apply Z.leb_gt in E3; lia].
    destruct (Z.leb threshold (nth j r1 0)) eqn:E4; [| apply Z.leb_gt in E4; lia].
    simpl. reflexivity.
Qed.

Definition pack_contour_edge (x y : Z) (horiz : bool) : Z :=
  (x + 1) * 4294967296 + (y + 1) * 2 + (if horiz then 1 else 0).

Definition edge_lex_lt (x1 y1 : Z) (h1 : bool) (x2 y2 : Z) (h2 : bool) : Prop :=
  x1 < x2 \/ (x1 = x2 /\ (y1 < y2 \/ (y1 = y2 /\ h1 = false /\ h2 = true))).

(**md
**64-bit packed `Contour` edge key (`((x + 1) <<< 32) ||| ((y + 1) <<< 1) ||| horiz`) is injective and order-isomorphic to `struct(x, y, horiz)`.**

For all valid marching-squares edge coordinates (`-1 <= x, y < 2^31 - 1`), `pack_contour_edge` fits in nonnegative signed `int64` (`[0, 2^63)`), is injective, and preserves exact lexicographic `(x, y, horiz)` sort order under primitive `int64` sorting.

- [x1:] first edge column (`-1 <= x1 < 2^31 - 1`).
- [y1:] first edge row (`-1 <= y1 < 2^31 - 1`).
- [h1:] first edge orientation (`false` = vertical, `true` = horizontal).
- [x2:] second edge column (`-1 <= x2 < 2^31 - 1`).
- [y2:] second edge row (`-1 <= y2 < 2^31 - 1`).
- [h2:] second edge orientation.
*)
Theorem packed_contour_edge_order_isomorphic
    (x1 y1 : Z) (h1 : bool) (x2 y2 : Z) (h2 : bool) :
  -1 <= x1 -> x1 < 2147483647 ->
  -1 <= y1 -> y1 < 2147483647 ->
  -1 <= x2 -> x2 < 2147483647 ->
  -1 <= y2 -> y2 < 2147483647 ->
  0 <= pack_contour_edge x1 y1 h1 /\
  pack_contour_edge x1 y1 h1 < 9223372036854775808 /\
  (pack_contour_edge x1 y1 h1 = pack_contour_edge x2 y2 h2 <-> x1 = x2 /\ y1 = y2 /\ h1 = h2) /\
  (pack_contour_edge x1 y1 h1 < pack_contour_edge x2 y2 h2 <-> edge_lex_lt x1 y1 h1 x2 y2 h2).
Proof.
  intros Hx1lo Hx1hi Hy1lo Hy1hi Hx2lo Hx2hi Hy2lo Hy2hi.
  unfold pack_contour_edge, edge_lex_lt.
  destruct h1, h2.
  - split; [lia | split; [lia | split]].
    + split; [intros Heq; split; [lia | split; [lia | reflexivity]] | intros [? [? _]]; lia].
    + split; [intros Hlt; destruct (Z.lt_ge_cases x1 x2); [left; lia | right; split; [lia | left; lia]]
             | intros [? | [? [? | [? [Hf _]]]]]; try discriminate; lia].
  - split; [lia | split; [lia | split]].
    + split; [intros Heq; exfalso; lia | intros [_ [_ Hf]]; discriminate].
    + split; [intros Hlt; destruct (Z.lt_ge_cases x1 x2); [left; lia | right; split; [lia | left; lia]]
             | intros [? | [? [? | [? [Hf _]]]]]; try discriminate; lia].
  - split; [lia | split; [lia | split]].
    + split; [intros Heq; exfalso; lia | intros [_ [_ Hf]]; discriminate].
    + split; [intros Hlt; destruct (Z.lt_ge_cases x1 x2); [left; lia | right; split; [lia | destruct (Z.lt_ge_cases y1 y2); [left; lia | right; split; [lia | split; reflexivity]]]]
             | intros [? | [? [? | [? [_ _]]]]]; lia].
  - split; [lia | split; [lia | split]].
    + split; [intros Heq; split; [lia | split; [lia | reflexivity]] | intros [? [? _]]; lia].
    + split; [intros Hlt; destruct (Z.lt_ge_cases x1 x2); [left; lia | right; split; [lia | left; lia]]
             | intros [? | [? [? | [? [_ Ht]]]]]; try discriminate; lia].
Qed.
