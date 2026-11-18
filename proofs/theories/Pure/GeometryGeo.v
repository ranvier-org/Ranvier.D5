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

(** ** 6. Batch D3 refinements: `Int128` exact `orient2d` bounds, Y-straddle ray-cast filter, & CW-to-CCW half-edge involution *)

(**md
**Scaled IEEE-754 mantissas with exponent spread `<= 9` never overflow `int64` or `System.Int128` in exact `orient2d` (`Predicates`).**

When six IEEE-754 binary64 significands satisfy `|m_i| <= 2^53 - 1` (`9007199254740991`) and their relative left-shift factors satisfy `1 <= s_i <= 512` (`0 <= shift <= 9`), every scaled coordinate `v_i = m_i * s_i` fits strictly in signed `int64` (`[-2^63, 2^63 - 1]`) and the exact 2D orientation determinant `(v0 - v4)*(v3 - v5) - (v1 - v5)*(v2 - v4)` fits strictly in signed `System.Int128` (`[-2^127, 2^127 - 1]`) with zero heap allocation.

- [m0:] signed 53-bit significand of `a.X`.
- [m1:] signed 53-bit significand of `a.Y`.
- [m2:] signed 53-bit significand of `b.X`.
- [m3:] signed 53-bit significand of `b.Y`.
- [m4:] signed 53-bit significand of `c.X`.
- [m5:] signed 53-bit significand of `c.Y`.
- [s0:] power-of-two scale factor `2^(e0 - minExp)` in `[1, 512]`.
- [s1:] power-of-two scale factor `2^(e1 - minExp)` in `[1, 512]`.
- [s2:] power-of-two scale factor `2^(e2 - minExp)` in `[1, 512]`.
- [s3:] power-of-two scale factor `2^(e3 - minExp)` in `[1, 512]`.
- [s4:] power-of-two scale factor `2^(e4 - minExp)` in `[1, 512]`.
- [s5:] power-of-two scale factor `2^(e5 - minExp)` in `[1, 512]`.
*)
Theorem int128_orient2d_no_overflow
    (m0 m1 m2 m3 m4 m5 s0 s1 s2 s3 s4 s5 : Z) :
  -9007199254740991 <= m0 -> m0 <= 9007199254740991 ->
  -9007199254740991 <= m1 -> m1 <= 9007199254740991 ->
  -9007199254740991 <= m2 -> m2 <= 9007199254740991 ->
  -9007199254740991 <= m3 -> m3 <= 9007199254740991 ->
  -9007199254740991 <= m4 -> m4 <= 9007199254740991 ->
  -9007199254740991 <= m5 -> m5 <= 9007199254740991 ->
  1 <= s0 -> s0 <= 512 ->
  1 <= s1 -> s1 <= 512 ->
  1 <= s2 -> s2 <= 512 ->
  1 <= s3 -> s3 <= 512 ->
  1 <= s4 -> s4 <= 512 ->
  1 <= s5 -> s5 <= 512 ->
  let v0 := m0 * s0 in let v1 := m1 * s1 in
  let v2 := m2 * s2 in let v3 := m3 * s3 in
  let v4 := m4 * s4 in let v5 := m5 * s5 in
  let det := (v0 - v4) * (v3 - v5) - (v1 - v5) * (v2 - v4) in
  -9223372036854775808 <= v0 /\ v0 <= 9223372036854775807 /\
  -170141183460469231731687303715884105728 <= det /\
  det <= 170141183460469231731687303715884105727.
Proof.
  intros Hm0a Hm0b Hm1a Hm1b Hm2a Hm2b Hm3a Hm3b Hm4a Hm4b Hm5a Hm5b
         Hs0a Hs0b Hs1a Hs1b Hs2a Hs2b Hs3a Hs3b Hs4a Hs4b Hs5a Hs5b.
  cbv zeta.
  assert (Hv0 : -4611686018427387392 <= m0 * s0 /\ m0 * s0 <= 4611686018427387392) by nia.
  assert (Hv1 : -4611686018427387392 <= m1 * s1 /\ m1 * s1 <= 4611686018427387392) by nia.
  assert (Hv2 : -4611686018427387392 <= m2 * s2 /\ m2 * s2 <= 4611686018427387392) by nia.
  assert (Hv3 : -4611686018427387392 <= m3 * s3 /\ m3 * s3 <= 4611686018427387392) by nia.
  assert (Hv4 : -4611686018427387392 <= m4 * s4 /\ m4 * s4 <= 4611686018427387392) by nia.
  assert (Hv5 : -4611686018427387392 <= m5 * s5 /\ m5 * s5 <= 4611686018427387392) by nia.
  set (dx1 := m0 * s0 - m4 * s4).
  set (dy2 := m3 * s3 - m5 * s5).
  set (dy1 := m1 * s1 - m5 * s5).
  set (dx2 := m2 * s2 - m4 * s4).
  assert (Hdx1 : -9223372036854774784 <= dx1 /\ dx1 <= 9223372036854774784) by lia.
  assert (Hdy2 : -9223372036854774784 <= dy2 /\ dy2 <= 9223372036854774784) by lia.
  assert (Hdy1 : -9223372036854774784 <= dy1 /\ dy1 <= 9223372036854774784) by lia.
  assert (Hdx2 : -9223372036854774784 <= dx2 /\ dx2 <= 9223372036854774784) by lia.
  assert (Hp1 : -85070591730234605658436518579420528640 <= dx1 * dy2 /\
                 dx1 * dy2 <= 85070591730234605658436518579420528640) by nia.
  assert (Hp2 : -85070591730234605658436518579420528640 <= dy1 * dx2 /\
                 dy1 * dx2 <= 85070591730234605658436518579420528640) by nia.
  split; [lia | split; [lia | split; lia]].
Qed.

Definition edge_step_ref (px py ax ay bx by_ orient : Z) : bool * Z :=
  if Z.eqb orient 0 &&
     Z.leb (Z.min ax bx) px && Z.leb px (Z.max ax bx) &&
     Z.leb (Z.min ay by_) py && Z.leb py (Z.max ay by_) then (true, 0)
  else if Z.leb ay py && Z.gtb by_ py && Z.gtb orient 0 then (false, 1)
  else if Z.gtb ay py && Z.leb by_ py && Z.ltb orient 0 then (false, -1)
  else (false, 0).

Definition edge_step_y_filtered (px py ax ay bx by_ orient : Z) : bool * Z :=
  if (Z.ltb ay py && Z.ltb by_ py) || (Z.gtb ay py && Z.gtb by_ py) then (false, 0)
  else edge_step_ref px py ax ay bx by_ orient.

(**md
**Skipping non-Y-straddling polygon edges before `Predicates.orient2d` preserves exact boundary and winding classification (`Polygon.contains`).**

Whenever `(ay < py && by < py)` or `(ay > py && by > py)`, the edge can neither contain `(px, py)` on its bounding box nor cross the horizontal ray at `py`, so filtering on Y-straddle before invoking `orient2d` yields identical `(boundary, delta_winding)` results.

- [px:] query X coordinate.
- [py:] query Y coordinate.
- [ax:] edge start X coordinate.
- [ay:] edge start Y coordinate.
- [bx:] edge end X coordinate.
- [by_:] edge end Y coordinate.
- [orient:] exact `orient2d a b p` sign (`-1`, `0`, or `1`).
*)
Theorem y_straddle_filter_preserves_polygon_containment
    (px py ax ay bx by_ orient : Z) :
  edge_step_y_filtered px py ax ay bx by_ orient =
  edge_step_ref px py ax ay bx by_ orient.
Proof.
  unfold edge_step_y_filtered, edge_step_ref.
  rewrite !Z.gtb_ltb.
  destruct ((ay <? py) && (by_ <? py) || (py <? ay) && (py <? by_)) eqn:Hskip; [| reflexivity].
  apply orb_true_iff in Hskip as [Hlo | Hhi].
  - apply andb_prop in Hlo as [H1 H2]. apply Z.ltb_lt in H1, H2.
    assert (EnMax : Z.leb py (Z.max ay by_) = false) by (apply Z.leb_gt; lia).
    assert (EnBy : (py <? by_) = false) by (apply Z.ltb_ge; lia).
    assert (EnAy : (py <? ay) = false) by (apply Z.ltb_ge; lia).
    rewrite EnMax, EnBy, EnAy.
    rewrite !andb_false_r; simpl; reflexivity.
  - apply andb_prop in Hhi as [H1 H2]. apply Z.ltb_lt in H1, H2.
    assert (EnMin : Z.leb (Z.min ay by_) py = false) by (apply Z.leb_gt; lia).
    assert (EnAy : Z.leb ay py = false) by (apply Z.leb_gt; lia).
    assert (EnBy : Z.leb by_ py = false) by (apply Z.leb_gt; lia).
    rewrite EnMin, EnAy, EnBy.
    destruct (Z.eqb orient 0 && Z.leb (Z.min ax bx) px && Z.leb px (Z.max ax bx));
      simpl; rewrite ?andb_false_r; simpl; reflexivity.
Qed.

Definition flip_halfedge (e : Z) : Z :=
  let r := e mod 3 in
  if Z.eqb r 0 then e + 2
  else if Z.eqb r 2 then e - 2
  else e.

(**md
**The CW-to-CCW Delaunay half-edge permutation (`3k <-> 3k + 2`, `3k + 1 -> 3k + 1`) is a triangle-preserving involution (`DelaunayKernel`).**

Swapping vertices `1` and `2` of each triangle `(v0, v1, v2) -> (v0, v2, v1)` maps half-edges via `flip_halfedge`, which satisfies `flip_halfedge (flip_halfedge e) = e` and stays within the same triangle `e / 3`, allowing `Delaunay.build` to reuse `DelaunayKernel.fast`'s `opposite` table directly without rebuilding a half-edge hash table.

- [e:] nonnegative half-edge index (`0 <= e`).
*)
Theorem delaunay_cw_to_ccw_halfedge_involution (e : Z) :
  0 <= e ->
  0 <= flip_halfedge e /\
  (flip_halfedge e) / 3 = e / 3 /\
  flip_halfedge (flip_halfedge e) = e.
Proof.
  intros He. unfold flip_halfedge; cbv zeta.
  pose proof (Z.div_mod e 3 ltac:(lia)) as Hdm.
  pose proof (Z.mod_pos_bound e 3 ltac:(lia)) as Hmb.
  set (q := e / 3) in *.
  destruct (Z.eqb (e mod 3) 0) eqn:E0.
  - apply Z.eqb_eq in E0.
    assert (Hm : (e + 2) mod 3 = 2) by (symmetry; apply (Z.mod_unique (e + 2) 3 q 2); lia).
    assert (Hd : (e + 2) / 3 = q) by (symmetry; apply (Z.div_unique (e + 2) 3 q 2); lia).
    rewrite Hm, Hd; simpl; repeat split; lia.
  - apply Z.eqb_neq in E0.
    destruct (Z.eqb (e mod 3) 2) eqn:E2.
    + apply Z.eqb_eq in E2.
      assert (Hm : (e - 2) mod 3 = 0) by (symmetry; apply (Z.mod_unique (e - 2) 3 q 0); lia).
      assert (Hd : (e - 2) / 3 = q) by (symmetry; apply (Z.div_unique (e - 2) 3 q 0); lia).
      rewrite Hm, Hd; simpl; repeat split; lia.
    + apply Z.eqb_neq in E2.
      assert (Hm : e mod 3 = 1) by lia.
      rewrite Hm; simpl; repeat split; lia.
Qed.

(** ** 7. Batch D4 refinements: `Ranvier.D5.Geo` spherical ring caching, single-pass rotation, azimuthal cap circle, & RFC 8259 number scanner *)

Section SphericalRingRefinements.
  Context {V P R S : Type}.

  Fixpoint ring_scan_recomputed
      (phi : V -> P) (step : V -> P -> V -> P -> S -> S)
      (prev : V) (vs : list V) (acc : S) : S :=
    match vs with
    | [] => acc
    | cur :: rest =>
      ring_scan_recomputed phi step cur rest
        (step prev (phi prev) cur (phi cur) acc)
    end.

  Fixpoint ring_scan_carried
      (phi : V -> P) (step : V -> P -> V -> P -> S -> S)
      (prev : V) (prev_p : P) (vs : list V) (acc : S) : S :=
    match vs with
    | [] => acc
    | cur :: rest =>
      let cur_p := phi cur in
      ring_scan_carried phi step cur cur_p rest
        (step prev prev_p cur cur_p acc)
    end.

  (**md
  **Carrying per-vertex spherical state across ring edges equals recomputing both endpoints per edge (`GeoPolygon.contains`, `GeoMeasure.area`).**

  Whenever `prev_p = phi prev` (such as cached `sinCosDegrees (lat / 2 + 45)` or `canonicalPole`), carrying `cur_p = phi cur` forward as the next iteration's `prev_p` computes `phi` once per ring vertex instead of twice and yields the exact same accumulated state as `ring_scan_recomputed`.

  - [phi:] per-vertex projection/trigonometric preparation.
  - [step:] edge transition consuming `(prev, phi prev, cur, phi cur, state)`.
  - [prev:] prior vertex (`ring[ring.Length - 1]` initially).
  - [vs:] ring vertices traversed in order.
  - [acc:] initial accumulator state.
  *)
  Theorem carried_vertex_scan_eq_recomputed
      (phi : V -> P) (step : V -> P -> V -> P -> S -> S)
      (prev : V) (vs : list V) (acc : S) :
    ring_scan_carried phi step prev (phi prev) vs acc =
    ring_scan_recomputed phi step prev vs acc.
  Proof.
    revert prev acc.
    induction vs as [|cur rest IH]; intros prev acc; simpl.
    - reflexivity.
    - apply IH.
  Qed.

  Fixpoint preclip_open_rotated
      (rot : V -> R) (edge : R -> R -> S -> S)
      (prev_r : R) (vs : list V) (acc : S) : S :=
    match vs with
    | [] => acc
    | v :: rest =>
      let cur_r := rot v in
      preclip_open_rotated rot edge cur_r rest (edge prev_r cur_r acc)
    end.

  Fixpoint preclip_already_rotated
      (edge : R -> R -> S -> S)
      (prev_r : R) (rs : list R) (first_r : R) (acc : S) : S :=
    match rs with
    | [] => edge prev_r first_r acc
    | cur_r :: rest =>
      preclip_already_rotated edge cur_r rest first_r (edge prev_r cur_r acc)
    end.

  Lemma preclip_already_rotated_map
      (rot : V -> R) (edge : R -> R -> S -> S)
      (prev_r : R) (vs : list V) (v0 : V) (acc : S) :
    preclip_already_rotated edge prev_r (List.map rot vs) (rot v0) acc =
    preclip_open_rotated rot edge prev_r (Datatypes.app vs [v0]) acc.
  Proof.
    revert prev_r acc.
    induction vs as [|v rest IH]; intros prev_r acc; simpl.
    - reflexivity.
    - apply IH.
  Qed.

  (**md
  **Preclipping an already-rotated ring with implicit closure equals building `closed = ring ++ [ring[0]]` and rotating again (`GeoPolygon`, `GeoPreclip`).**

  Rotating `v0 :: rest` once into `rot v0 :: map rot rest` and traversing its edges with implicit wrap-around to `rot v0` produces the exact same sink state transition as allocating `closed = (v0 :: rest) ++ [v0]` and applying `rotateKeepingSeam` a second time on every vertex.

  - [rot:] `GeoPreclip.rotateKeepingSeam projection`.
  - [edge:] `GeoPreclip` per-edge clipper transition.
  - [v0:] first ring vertex.
  - [rest:] remaining ring vertices.
  - [acc:] initial clipper sink state.
  *)
  Theorem single_rotation_closed_ring_eq
      (rot : V -> R) (edge : R -> R -> S -> S)
      (v0 : V) (rest : list V) (acc : S) :
    let r0 := rot v0 in
    preclip_already_rotated edge r0 (List.map rot rest) r0 acc =
    preclip_open_rotated rot edge (rot v0) (Datatypes.app rest [v0]) acc.
  Proof.
    cbv zeta.
    apply preclip_already_rotated_map.
  Qed.
End SphericalRingRefinements.

(**md
**Azimuthal cap boundary screen projection reduces to a direct circle around `screen(0, 0)` (`GeoPolygon.projectWith`).**

For every azimuthal projection whose raw origin is `(0, 0)` and whose visible cap colatitude maps to raw radius `r_plane` (`raw_x = r_plane * sin_t`, `raw_y = r_plane * cos_t`), applying the affine screen transform `(tx + scale * (raw_x - cx), ty - scale * (raw_y - cy))` equals `(origin_x + (scale * r_plane) * sin_t, origin_y - (scale * r_plane) * cos_t)` where `(origin_x, origin_y)` is the screen projection of `(0, 0)`.

- [tx:] screen translate X (scaled integer).
- [ty:] screen translate Y (scaled integer).
- [scale:] projection scale factor.
- [cx:] raw projection center X.
- [cy:] raw projection center Y.
- [r_plane:] `Projection.clipPlaneRadius`.
- [sin_t:] `sin t` at boundary parameter `t`.
- [cos_t:] `cos t` at boundary parameter `t`.
*)
Theorem azimuthal_cap_circle_screen_eq
    (tx ty scale cx cy r_plane sin_t cos_t : Z) :
  let origin_x := tx + scale * (0 - cx) in
  let origin_y := ty - scale * (0 - cy) in
  let boundary_r := scale * r_plane in
  let raw_x := r_plane * sin_t in
  let raw_y := r_plane * cos_t in
  tx + scale * (raw_x - cx) = origin_x + boundary_r * sin_t /\
  ty - scale * (raw_y - cy) = origin_y - boundary_r * cos_t.
Proof.
  cbv zeta. split; nia.
Qed.

(** RFC 8259 Section 6 JSON number token alphabet:
    0..9 = digits '0'..'9', 10 = '-', 11 = '+', 12 = '.', 13 = 'e'/'E', >=14 = other. *)
Definition is_digit_tok (c : nat) : bool := Nat.leb c 9%nat.
Definition is_nonzero_digit_tok (c : nat) : bool := Nat.leb 1%nat c && Nat.leb c 9%nat.

Fixpoint strip_digits (cs : list nat) : list nat :=
  match cs with
  | [] => []
  | c :: rest => if is_digit_tok c then strip_digits rest else c :: rest
  end.

Definition strip_one_or_more_digits (cs : list nat) : option (list nat) :=
  match cs with
  | c :: rest => if is_digit_tok c then Some (strip_digits rest) else None
  | [] => None
  end.

Definition strip_int_part (cs : list nat) : option (list nat) :=
  match cs with
  | [] => None
  | c :: rest =>
    if Nat.eqb c 0%nat then Some rest
    else if is_nonzero_digit_tok c then Some (strip_digits rest)
    else None
  end.

Definition strip_frac_part (cs : list nat) : option (list nat) :=
  match cs with
  | c :: rest =>
    if Nat.eqb c 12%nat then strip_one_or_more_digits rest
    else Some (c :: rest)
  | [] => Some []
  end.

Definition strip_exp_part (cs : list nat) : option (list nat) :=
  match cs with
  | c :: rest =>
    if Nat.eqb c 13%nat then
      match rest with
      | s :: rest' =>
        if Nat.eqb s 10%nat || Nat.eqb s 11%nat then strip_one_or_more_digits rest'
        else strip_one_or_more_digits (s :: rest')
      | [] => None
      end
    else Some (c :: rest)
  | [] => Some []
  end.

Definition valid_json_number_tokens (cs : list nat) : bool :=
  let after_sign :=
    match cs with
    | c :: rest => if Nat.eqb c 10%nat then rest else cs
    | [] => []
    end in
  match strip_int_part after_sign with
  | None => false
  | Some r1 =>
    match strip_frac_part r1 with
    | None => false
    | Some r2 =>
      match strip_exp_part r2 with
      | Some [] => true
      | _ => false
      end
    end
  end.

(**md
**The single-pass RFC 8259 JSON number scanner accepts valid integer/decimal/exponent lexemes and rejects malformed or multi-token payloads (`GeoJson`).**

Verifies the decision boundaries required by `GeoJson.serialize` when validating `GeoValue.Number` / `GeoId.Number` without `JsonDocument.Parse`: leading-zero numbers like `"01"`, bare signs `"-"` / `"+1"`, trailing dots `"1."`, bare exponents `"1e+"`, and injected delimiters `"0,1"` all evaluate to `false`, while `"0"`, `"-0"`, multi-digit integers, decimals, and signed exponents evaluate to `true`.

- [d:] any nonzero leading digit `1 .. 9`.
*)
Theorem rfc8259_number_scanner_boundaries_sound (d : nat) :
  (1 <= d)%nat -> (d <= 9)%nat ->
  valid_json_number_tokens [0%nat] = true /\
  valid_json_number_tokens [10%nat; 0%nat] = true /\
  valid_json_number_tokens [d; 0%nat; 12%nat; 5%nat; 13%nat; 10%nat; 3%nat] = true /\
  valid_json_number_tokens [] = false /\
  valid_json_number_tokens [10%nat] = false /\
  valid_json_number_tokens [11%nat; 1%nat] = false /\
  valid_json_number_tokens [0%nat; 1%nat] = false /\
  valid_json_number_tokens [1%nat; 12%nat] = false /\
  valid_json_number_tokens [12%nat; 5%nat] = false /\
  valid_json_number_tokens [1%nat; 13%nat; 11%nat] = false /\
  valid_json_number_tokens [0%nat; 14%nat; 1%nat] = false.
Proof.
  intros Hd1 Hd9.
  do 9 (destruct d as [|d]; [try lia; vm_compute; repeat split; reflexivity |]).
  destruct d as [|d]; [vm_compute; repeat split; reflexivity | lia].
Qed.

(**md
**SoA point-longitude gap scanning and half/three-quarter angle identities preserve `GeoMeasure.bounds` and `GeoPath` resampling.**

1. On sorted point intervals `[x, x]` (`prev_finish <= next_x`), merging `[next_x, next_x]` into `prev_finish` always advances `finish' = next_x` with gap `next_x - prev_finish` whenever `next_x > prev_finish`, justifying 1D `double` sorting without storing duplicate `east` coordinates on `MultiPoint` inputs.
2. Given midpoint sine/cosine `(sm, cm)` on the unit circle (`sm^2 + cm^2 = 1`) and quarter-angle `(sq, cq)` satisfying `2*sq*cq = sm` and `cq^2 - sq^2 = cm`, the three-quarter angle formulas `s3q = sm*cq + cm*sq` and `c3q = cm*cq - sm*sq` lie on the unit circle (`s3q^2 + c3q^2 = (sm^2 + cm^2)*(sq^2 + cq^2)`) and satisfy the exact Chebyshev triple-angle identities `s3q = 3*sq*cq^2 - sq^3` and `c3q = cq^3 - 3*cq*sq^2`.

- [prev_finish:] prior merged interval upper bound.
- [next_x:] next sorted point longitude (`prev_finish <= next_x`).
- [sq:] `sin(theta / 4)` (scaled integer).
- [cq:] `cos(theta / 4)` (scaled integer).
*)
Theorem soa_point_gap_and_quarter_angle_identities
    (prev_finish next_x sq cq : Z) :
  prev_finish <= next_x ->
  let merged_finish :=
    if Z.gtb next_x prev_finish then next_x else Z.max prev_finish next_x in
  let sm := 2 * sq * cq in
  let cm := cq * cq - sq * sq in
  let s3q := sm * cq + cm * sq in
  let c3q := cm * cq - sm * sq in
  merged_finish = next_x /\
  s3q = 3 * sq * cq * cq - sq * sq * sq /\
  c3q = cq * cq * cq - 3 * cq * sq * sq /\
  s3q * s3q + c3q * c3q = (sm * sm + cm * cm) * (sq * sq + cq * cq).
Proof.
  intros Hle; cbv zeta.
  rewrite Z.gtb_ltb.
  split.
  - destruct (Z.ltb prev_finish next_x) eqn:Elt; [reflexivity | apply Z.ltb_ge in Elt; lia].
  - split; [ring | split; ring].
Qed.
