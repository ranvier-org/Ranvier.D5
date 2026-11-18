From stdpp Require Import prelude.
From Stdlib Require Import Arith Lia List Bool ZArith.
Import ListNotations.

Local Arguments Nat.eqb : simpl never.
Open Scope Z_scope.

(**md # Balanced k-d tree (`Spatial`) and `Quadtree` range & nearest-neighbor pruning

Models the spatial indexing structures and branch-and-bound pruning invariants in `Ranvier.D5.Spatial` and `Ranvier.D5.Quadtree`. *)

(** ** 1. Squared Euclidean geometry and AABB closest-point lower bounds *)

Record pt2 := {
  px : Z;
  py : Z
}.

Record aabb := {
  min_x : Z;
  max_x : Z;
  min_y : Z;
  max_y : Z
}.

Definition in_aabb (b : aabb) (p : pt2) : bool :=
  Z.leb (min_x b) (px p) && Z.leb (px p) (max_x b) &&
  Z.leb (min_y b) (py p) && Z.leb (py p) (max_y b).

Definition dist_sq (a b : pt2) : Z :=
  let dx := px a - px b in
  let dy := py a - py b in
  dx * dx + dy * dy.

Definition clamp_1d (lo hi q : Z) : Z :=
  Z.max lo (Z.min hi q).

Definition closest_on_aabb (b : aabb) (q : pt2) : pt2 :=
  {| px := clamp_1d (min_x b) (max_x b) (px q);
     py := clamp_1d (min_y b) (max_y b) (py q) |}.

Lemma clamp_1d_sq_le (lo hi q p : Z) :
  lo <= p -> p <= hi ->
  (q - clamp_1d lo hi q) * (q - clamp_1d lo hi q) <= (q - p) * (q - p).
Proof.
  intros Hlo Hhi. unfold clamp_1d.
  assert (Hcases : q < lo \/ (lo <= q /\ q <= hi) \/ hi < q) by lia.
  destruct Hcases as [Hlt | [[H1 H2] | Hgt]].
  - replace (Z.max lo (Z.min hi q)) with lo by lia.
    assert (Hd1 : p - q = (p - lo) + (lo - q)) by lia.
    assert (Hp1 : 0 <= p - lo) by lia.
    assert (Hp2 : 0 <= lo - q) by lia.
    replace ((q - lo) * (q - lo)) with ((lo - q) * (lo - q)) by ring.
    replace ((q - p) * (q - p)) with ((p - q) * (p - q)) by ring.
    rewrite Hd1. nia.
  - replace (Z.max lo (Z.min hi q)) with q by lia.
    replace ((q - q) * (q - q)) with 0 by ring.
    apply Z.square_nonneg.
  - replace (Z.max lo (Z.min hi q)) with hi by lia.
    assert (Hd2 : q - p = (q - hi) + (hi - p)) by lia.
    assert (Hp1 : 0 <= q - hi) by lia.
    assert (Hp2 : 0 <= hi - p) by lia.
    rewrite Hd2. nia.
Qed.

(**md
**The clamped AABB point lower-bounds squared distance to every point inside the `Quadtree` cell.**

For any point `p` inside `cell` (`in_aabb cell p = true`), `dist_sq q (closest_on_aabb cell q) <= dist_sq q p`, guaranteeing that `Quadtree.nearest` can safely prune any cell whose closest AABB point exceeds the current best distance (or search radius) without missing a closer or tie-breaking point.

- [cell:] the axis-aligned bounding box of a `Quadtree` node.
- [q:] the query position.
- [p:] any point stored inside `cell`.
*)
Theorem quadtree_aabb_lower_bound_sound (cell : aabb) (q p : pt2) :
  in_aabb cell p = true ->
  dist_sq q (closest_on_aabb cell q) <= dist_sq q p.
Proof.
  intros Hin. unfold in_aabb in Hin.
  repeat (apply andb_prop in Hin as [Hin ?]).
  apply Z.leb_le in Hin. apply Z.leb_le in H. apply Z.leb_le in H0. apply Z.leb_le in H1.
  unfold dist_sq, closest_on_aabb; simpl.
  pose proof (clamp_1d_sq_le (min_x cell) (max_x cell) (px q) (px p) Hin H1).
  pose proof (clamp_1d_sq_le (min_y cell) (max_y cell) (py q) (py p) H0 H).
  lia.
Qed.

(** ** 2. `Quadtree` leaf-count conservation *)

Inductive qtree :=
  | QLeaf (items : list pt2)
  | QInternal (nw ne sw se : qtree).

Fixpoint qtree_stored_count (t : qtree) : nat :=
  match t with
  | QLeaf items => length items
  | QInternal nw ne sw se =>
    (qtree_stored_count nw + qtree_stored_count ne +
     qtree_stored_count sw + qtree_stored_count se)%nat
  end.

Definition quadrant_bucket (mx my : Z) (quad : nat) (p : pt2) : bool :=
  let east := Z.geb (px p) mx in
  let south := Z.geb (py p) my in
  match quad with
  | 0%nat => negb east && negb south
  | 1%nat => east && negb south
  | 2%nat => negb east && south
  | _ => east && south
  end.

Lemma partition_four_counts (mx my : Z) (pts : list pt2) :
  (length (List.filter (quadrant_bucket mx my 0) pts) +
   length (List.filter (quadrant_bucket mx my 1) pts) +
   length (List.filter (quadrant_bucket mx my 2) pts) +
   length (List.filter (quadrant_bucket mx my 3) pts) = length pts)%nat.
Proof.
  induction pts as [|p rest IH]; [reflexivity |].
  simpl.
  set (f0 := List.filter (quadrant_bucket mx my 0) rest) in *.
  set (f1 := List.filter (quadrant_bucket mx my 1) rest) in *.
  set (f2 := List.filter (quadrant_bucket mx my 2) rest) in *.
  set (f3 := List.filter (quadrant_bucket mx my 3) rest) in *.
  unfold quadrant_bucket.
  destruct (Z.geb (px p) mx), (Z.geb (py p) my); simpl; lia.
Qed.

(**md
**Splitting a `Quadtree` cell at `(mx, my)` into NW, NE, SW, and SE quadrants conserves all points.**

Every point falls into exactly one of the four quadrant buckets (`east = px >= mx`, `south = py >= my`), so recursive subdivision preserves total item count with zero drops or duplicates.

- [mx:] cell horizontal midpoint.
- [my:] cell vertical midpoint.
- [pts:] points partitioned across the four child quadrants.
*)
Theorem quadtree_leaf_count_conservation (mx my : Z) (pts : list pt2) :
  let nw := QLeaf (List.filter (quadrant_bucket mx my 0) pts) in
  let ne := QLeaf (List.filter (quadrant_bucket mx my 1) pts) in
  let sw := QLeaf (List.filter (quadrant_bucket mx my 2) pts) in
  let se := QLeaf (List.filter (quadrant_bucket mx my 3) pts) in
  qtree_stored_count (QInternal nw ne sw se) = length pts.
Proof.
  cbv zeta; simpl.
  apply partition_four_counts.
Qed.

(** ** 3. Balanced k-d tree (`Spatial`) range and `nearest` split-plane pruning *)

Inductive kdtree :=
  | KdEmpty
  | KdNode (split_x : bool) (pt : pt2) (ord : nat) (left right : kdtree).

Fixpoint kd_all (P : pt2 -> Prop) (t : kdtree) : Prop :=
  match t with
  | KdEmpty => True
  | KdNode _ p _ l r => P p /\ kd_all P l /\ kd_all P r
  end.

Fixpoint kd_ordered (t : kdtree) : Prop :=
  match t with
  | KdEmpty => True
  | KdNode sx p _ l r =>
    let coord := fun q => if sx then px q else py q in
    kd_all (fun q => coord q <= coord p) l /\
    kd_all (fun q => coord p <= coord q) r /\
    kd_ordered l /\
    kd_ordered r
  end.

Fixpoint kd_in (p : pt2) (ord : nat) (t : kdtree) : Prop :=
  match t with
  | KdEmpty => False
  | KdNode _ q o l r => (p = q /\ ord = o) \/ kd_in p ord l \/ kd_in p ord r
  end.

Lemma kd_all_in (P : pt2 -> Prop) (t : kdtree) (p : pt2) (ord : nat) :
  kd_all P t -> kd_in p ord t -> P p.
Proof.
  induction t as [|sx q o l IHl r IHr]; simpl; intros Hall Hin; [contradiction |].
  destruct Hall as [Hq [Hl Hr]].
  destruct Hin as [[-> _] | [Hinl | Hinr]]; [exact Hq | exact (IHl Hl Hinl) | exact (IHr Hr Hinr)].
Qed.

Fixpoint kd_within (b : aabb) (t : kdtree) : list (pt2 * nat) :=
  match t with
  | KdEmpty => []
  | KdNode sx p o l r =>
    let c := if sx then px p else py p in
    let lo := if sx then min_x b else min_y b in
    let hi := if sx then max_x b else max_y b in
    let go_l := if Z.leb lo c then kd_within b l else [] in
    let here := if in_aabb b p then [(p, o)] else [] in
    let go_r := if Z.leb c hi then kd_within b r else [] in
    go_l ++ here ++ go_r
  end.

(**md
**`Spatial.within` is sound and complete over every k-d tree satisfying `kd_ordered`.**

Branch-and-bound subtree skipping on `lo <= split_coord` and `split_coord <= hi` returns `(p, ord)` if and only if `(p, ord)` is in the tree and `in_aabb b p = true`.

- [b:] inclusive query rectangle `[min_x, max_x] × [min_y, max_y]`.
- [t:] balanced k-d tree satisfying `kd_ordered`.
- [p:] candidate point.
- [ord:] original input index of `p`.
*)
Theorem spatial_within_sound_and_complete (b : aabb) (t : kdtree) (p : pt2) (ord : nat) :
  kd_ordered t ->
  (In (p, ord) (kd_within b t) <-> kd_in p ord t /\ in_aabb b p = true).
Proof.
  induction t as [|sx q o l IHl r IHr]; simpl; intros Hord.
  - split; [contradiction | intros [Hf _]; contradiction].
  - destruct Hord as [Hle_l [Hge_r [Hord_l Hord_r]]].
    rewrite !in_app_iff.
    split.
    + intros [Hl | [Hhere | Hr]].
      * destruct (Z.leb (if sx then min_x b else min_y b) (if sx then px q else py q)); [| contradiction].
        apply (IHl Hord_l) in Hl as [Hinl Hinb].
        split; [right; left; exact Hinl | exact Hinb].
      * destruct (in_aabb b q) eqn:Hbq; simpl in Hhere; [| contradiction].
        destruct Hhere as [Heq | Hf]; [| contradiction].
        injection Heq as <- <-.
        split; [left; split; reflexivity | exact Hbq].
      * destruct (Z.leb (if sx then px q else py q) (if sx then max_x b else max_y b)); [| contradiction].
        apply (IHr Hord_r) in Hr as [Hinr Hinb].
        split; [right; right; exact Hinr | exact Hinb].
    + intros [[Heq | [Hinl | Hinr]] Hinb].
      * destruct Heq as [-> ->]. rewrite Hinb. right; left; simpl; left; reflexivity.
      * left.
        pose proof (kd_all_in _ l p ord Hle_l Hinl) as Hle.
        unfold in_aabb in Hinb.
        pose proof Hinb as Hinb_copy.
        repeat (apply andb_prop in Hinb_copy as [Hinb_copy ?]).
        apply Z.leb_le in Hinb_copy. apply Z.leb_le in H. apply Z.leb_le in H0. apply Z.leb_le in H1.
        assert (Hgo : Z.leb (if sx then min_x b else min_y b) (if sx then px q else py q) = true).
        { destruct sx; simpl in *; apply Z.leb_le; lia. }
        rewrite Hgo. apply (IHl Hord_l). split; assumption.
      * right; right.
        pose proof (kd_all_in _ r p ord Hge_r Hinr) as Hge.
        unfold in_aabb in Hinb.
        pose proof Hinb as Hinb_copy.
        repeat (apply andb_prop in Hinb_copy as [Hinb_copy ?]).
        apply Z.leb_le in Hinb_copy. apply Z.leb_le in H. apply Z.leb_le in H0. apply Z.leb_le in H1.
        assert (Hgo : Z.leb (if sx then px q else py q) (if sx then max_x b else max_y b) = true).
        { destruct sx; simpl in *; apply Z.leb_le; lia. }
        rewrite Hgo. apply (IHr Hord_r). split; assumption.
Qed.

(**md
**`Spatial.nearest` strict plane-distance pruning (`axis_sq > best_d2`) never prunes a closer or tie-breaking point.**

When `q` lies on the near side of the splitting plane at `split_pt` and `(q_c - split_c)^2 > best_d2` (strict `>`), every point `p` in the far subtree satisfies `dist_sq q p > best_d2`, so `p` can neither beat `best_d2` nor tie `best_d2` with an earlier `order`.

- [sx:] split axis (`true` for X, `false` for Y).
- [q:] query point.
- [split_pt:] pivot point at the current k-d node.
- [p:] any point in the far subtree.
- [best_d2:] current best squared distance.
*)
Theorem spatial_nearest_pruning_sound (sx : bool) (q split_pt p : pt2) (best_d2 : Z) :
  let qc := if sx then px q else py q in
  let sc := if sx then px split_pt else py split_pt in
  let pc := if sx then px p else py p in
  ((qc <= sc /\ sc <= pc) \/ (pc <= sc /\ sc <= qc)) ->
  (qc - sc) * (qc - sc) > best_d2 ->
  dist_sq q p > best_d2.
Proof.
  cbv zeta. intros Hside Hprune. unfold dist_sq; cbv zeta.
  assert (Haxis : ( (if sx then px q else py q) - (if sx then px p else py p) ) *
                  ( (if sx then px q else py q) - (if sx then px p else py p) ) >=
                  ( (if sx then px q else py q) - (if sx then px split_pt else py split_pt) ) *
                  ( (if sx then px q else py q) - (if sx then px split_pt else py split_pt) )).
  { destruct Hside as [[H1 H2] | [H1 H2]];
    [ assert (Hd : (if sx then px p else py p) - (if sx then px q else py q) =
                   ((if sx then px p else py p) - (if sx then px split_pt else py split_pt)) +
                   ((if sx then px split_pt else py split_pt) - (if sx then px q else py q))) by lia
    | assert (Hd : (if sx then px q else py q) - (if sx then px p else py p) =
                   ((if sx then px q else py q) - (if sx then px split_pt else py split_pt)) +
                   ((if sx then px split_pt else py split_pt) - (if sx then px p else py p))) by lia ];
    nia. }
  destruct sx; simpl in Haxis, Hprune.
  - assert (Hnn : 0 <= (py q - py p) * (py q - py p)) by apply Z.square_nonneg; lia.
  - assert (Hnn : 0 <= (px q - px p) * (px q - px p)) by apply Z.square_nonneg; lia.
Qed.

(** ** 4. Batch D3 refinements: flat struct-array k-d tree layout & `sqrt`-free SIMD N-body softening *)

Record flat_kd_node := {
  fk_split_x : bool;
  fk_pt : pt2;
  fk_ord : nat;
  fk_left : option nat;
  fk_right : option nat
}.

Fixpoint flatten_kdtree (base : nat) (t : kdtree) : option nat * list flat_kd_node :=
  match t with
  | KdEmpty => (None, [])
  | KdNode sx p o l r =>
    let '(l_idx, l_arr) := flatten_kdtree (S base) l in
    let r_base := (S base + length l_arr)%nat in
    let '(r_idx, r_arr) := flatten_kdtree r_base r in
    let root_node := {| fk_split_x := sx; fk_pt := p; fk_ord := o; fk_left := l_idx; fk_right := r_idx |} in
    (Some base, root_node :: l_arr ++ r_arr)
  end.

Fixpoint flat_kd_within (fuel : nat) (arr : list flat_kd_node) (b : aabb) (idx_opt : option nat) : list (pt2 * nat) :=
  match idx_opt with
  | None => []
  | Some idx =>
    match fuel with
    | 0%nat => []
    | S f =>
      match nth_error arr idx with
      | None => []
      | Some nd =>
        let sx := fk_split_x nd in
        let p := fk_pt nd in
        let o := fk_ord nd in
        let c := if sx then px p else py p in
        let lo := if sx then min_x b else min_y b in
        let hi := if sx then max_x b else max_y b in
        let go_l := if Z.leb lo c then flat_kd_within f arr b (fk_left nd) else [] in
        let here := if in_aabb b p then [(p, o)] else [] in
        let go_r := if Z.leb c hi then flat_kd_within f arr b (fk_right nd) else [] in
        go_l ++ here ++ go_r
      end
    end
  end.

Fixpoint kd_depth (t : kdtree) : nat :=
  match t with
  | KdEmpty => 0%nat
  | KdNode _ _ _ l r => S (Nat.max (kd_depth l) (kd_depth r))
  end.

Lemma nth_error_prefix_head {A : Type} (pre tl : list A) (x : A) :
  nth_error (pre ++ x :: tl) (length pre) = Some x.
Proof.
  induction pre as [|h rest IH]; simpl; [reflexivity | exact IH].
Qed.

Lemma cons_app_reassoc {A : Type} (pre a b suf : list A) (x : A) :
  pre ++ (x :: a ++ b) ++ suf = pre ++ x :: a ++ b ++ suf /\
  pre ++ x :: a ++ b ++ suf = (pre ++ [x]) ++ a ++ (b ++ suf) /\
  pre ++ x :: a ++ b ++ suf = (pre ++ [x] ++ a) ++ b ++ suf.
Proof.
  split.
  - induction pre as [|h rest IH]; simpl.
    + f_equal. clear; induction a as [|ha ra IHa]; simpl; [reflexivity | f_equal; exact IHa].
    + f_equal; exact IH.
  - split.
    + induction pre as [|h rest IH]; simpl.
      * clear; induction a as [|ha ra IHa]; simpl; [reflexivity | f_equal; exact IHa].
      * f_equal; exact IH.
    + induction pre as [|h rest IH]; simpl.
      * clear; induction a as [|ha ra IHa]; simpl; [reflexivity | f_equal; exact IHa].
      * f_equal; exact IH.
Qed.

Lemma flatten_kdtree_embedded (t : kdtree) :
  forall (prefix suffix : list flat_kd_node) (fuel : nat) (b : aabb),
    (kd_depth t <= fuel)%nat ->
    let '(idx_opt, mid) := flatten_kdtree (length prefix) t in
    flat_kd_within fuel (prefix ++ mid ++ suffix) b idx_opt = kd_within b t.
Proof.
  induction t as [|sx p o l IHl r IHr]; intros prefix suffix fuel b Hfuel; cbn [flatten_kdtree kd_depth kd_within] in *.
  - destruct fuel; reflexivity.
  - destruct fuel as [|f]; [lia |].
    destruct (flatten_kdtree (S (length prefix)) l) as [l_idx l_arr] eqn:Hl.
    destruct (flatten_kdtree (S (length prefix) + length l_arr)%nat r) as [r_idx r_arr] eqn:Hr.
    set (root_nd := {| fk_split_x := sx; fk_pt := p; fk_ord := o; fk_left := l_idx; fk_right := r_idx |}).
    pose proof (cons_app_reassoc prefix l_arr r_arr suffix root_nd) as [Hre [Harr_eq_l Harr_eq_r]].
    cbn [flat_kd_within].
    rewrite Hre.
    rewrite nth_error_prefix_head.
    cbn [fk_split_x fk_pt fk_ord fk_left fk_right].
    assert (Hlen_l : length (prefix ++ [root_nd]) = S (length prefix)).
    { rewrite length_app; simpl; lia. }
    assert (Hlen_r : length (prefix ++ [root_nd] ++ l_arr) = (S (length prefix) + length l_arr)%nat).
    { rewrite !length_app; simpl; lia. }
    pose proof (IHl (prefix ++ [root_nd]) (r_arr ++ suffix) f b ltac:(lia)) as Hl_eq.
    rewrite Hlen_l, Hl in Hl_eq.
    pose proof (IHr (prefix ++ [root_nd] ++ l_arr) suffix f b ltac:(lia)) as Hr_eq.
    rewrite Hlen_r, Hr in Hr_eq.
    rewrite Harr_eq_l at 1.
    rewrite Harr_eq_r at 1.
    change (fk_split_x root_nd) with sx.
    change (fk_pt root_nd) with p.
    change (fk_ord root_nd) with o.
    change (fk_left root_nd) with l_idx.
    change (fk_right root_nd) with r_idx.
    destruct (Z.leb (if sx then min_x b else min_y b) (if sx then px p else py p));
      destruct (Z.leb (if sx then px p else py p) (if sx then max_x b else max_y b));
      rewrite ?Hl_eq, ?Hr_eq; reflexivity.
Qed.

(**md
**Flattening a k-d tree into a contiguous struct array preserves exact range-search semantics (`Spatial`).**

Replacing heap-allocated `SpatialNode` discriminated-union pointers with integer child indices into a flat `FlatSpatialNode<'T>[]` array yields the exact same `kd_within` traversal results as the inductive pointer tree.

- [t:] inductive k-d tree.
- [b:] query bounding box.
*)
Theorem flat_kdtree_isomorphic (t : kdtree) (b : aabb) :
  let '(root_opt, arr) := flatten_kdtree 0%nat t in
  flat_kd_within (kd_depth t) arr b root_opt = kd_within b t.
Proof.
  pose proof (flatten_kdtree_embedded t [] [] (kd_depth t) b ltac:(lia)) as H.
  simpl in H.
  destruct (flatten_kdtree 0%nat t) as [root_opt arr].
  rewrite app_nil_r in H.
  exact H.
Qed.

(**md
**`sqrt`-free SIMD N-body softening (`max(d^2, min_d^2)`) is algebraically equivalent to `Double.Hypot` softening (`ForceKernels.manyBody`).**

For all nonnegative distances `d >= 0` and `min_d >= 0`, `(max d min_d)^2 = max (d^2) (min_d^2)` and `(dx * num * d)` cancels `d` identically against `(softened^2)`, proving that `dx * (S * alpha * m / max(dx^2 + dy^2, minDist^2))` eliminates `sqrt` in both softened (`0 < d < minDist`) and unsoftened (`d >= minDist`) regimes.

- [d:] Euclidean pair distance (`d >= 0`).
- [min_d:] minimum softening distance (`min_d >= 0`).
- [dx:] coordinate separation along X or Y.
- [num:] scaled force numerator (`Strength * alpha * mass`).
*)
Theorem many_body_rational_form_cancels_sqrt (d min_d dx num : Z) :
  0 <= d ->
  0 <= min_d ->
  let softened := Z.max d min_d in
  softened * softened = Z.max (d * d) (min_d * min_d) /\
  (dx * num * d) * (softened * softened) = (dx * num) * (d * Z.max (d * d) (min_d * min_d)).
Proof.
  intros Hd Hmin. cbv zeta.
  assert (Hsq : Z.max d min_d * Z.max d min_d = Z.max (d * d) (min_d * min_d)).
  { destruct (Z.le_gt_cases d min_d) as [Hle | Hgt].
    - rewrite (Z.max_r d min_d Hle).
      assert (Hle2 : d * d <= min_d * min_d) by nia.
      symmetry; apply Z.max_r; exact Hle2.
    - assert (Hge : min_d <= d) by lia.
      rewrite (Z.max_l d min_d Hge).
      assert (Hge2 : min_d * min_d <= d * d) by nia.
      symmetry; apply Z.max_l; exact Hge2. }
  split; [exact Hsq | rewrite Hsq; ring].
Qed.

(** ** 5. Batch D6 refinements: 3D `Octree`, `sqrt`-free `ForceAtlas2` Barnes–Hut, and `(3+1)D` Minkowski `H3`/`Walrus` identities *)

Record pt3 := {
  px3 : Z;
  py3 : Z;
  pz3 : Z
}.

Record aabb3 := {
  min_x3 : Z;
  max_x3 : Z;
  min_y3 : Z;
  max_y3 : Z;
  min_z3 : Z;
  max_z3 : Z
}.

Definition in_aabb3 (b : aabb3) (p : pt3) : bool :=
  Z.leb (min_x3 b) (px3 p) && Z.leb (px3 p) (max_x3 b) &&
  Z.leb (min_y3 b) (py3 p) && Z.leb (py3 p) (max_y3 b) &&
  Z.leb (min_z3 b) (pz3 p) && Z.leb (pz3 p) (max_z3 b).

Definition dist_sq3 (a b : pt3) : Z :=
  let dx := px3 a - px3 b in
  let dy := py3 a - py3 b in
  let dz := pz3 a - pz3 b in
  dx * dx + dy * dy + dz * dz.

Definition closest_on_aabb3 (b : aabb3) (q : pt3) : pt3 :=
  {| px3 := clamp_1d (min_x3 b) (max_x3 b) (px3 q);
     py3 := clamp_1d (min_y3 b) (max_y3 b) (py3 q);
     pz3 := clamp_1d (min_z3 b) (max_z3 b) (pz3 q) |}.

Definition octant_bucket (mx my mz : Z) (oct : nat) (p : pt3) : bool :=
  let gx := Z.geb (px3 p) mx in
  let gy := Z.geb (py3 p) my in
  let gz := Z.geb (pz3 p) mz in
  match oct with
  | 0%nat => negb gx && negb gy && negb gz
  | 1%nat => gx && negb gy && negb gz
  | 2%nat => negb gx && gy && negb gz
  | 3%nat => gx && gy && negb gz
  | 4%nat => negb gx && negb gy && gz
  | 5%nat => gx && negb gy && gz
  | 6%nat => negb gx && gy && gz
  | _ => gx && gy && gz
  end.

Lemma partition_eight_counts (mx my mz : Z) (pts : list pt3) :
  (length (List.filter (octant_bucket mx my mz 0) pts) +
   length (List.filter (octant_bucket mx my mz 1) pts) +
   length (List.filter (octant_bucket mx my mz 2) pts) +
   length (List.filter (octant_bucket mx my mz 3) pts) +
   length (List.filter (octant_bucket mx my mz 4) pts) +
   length (List.filter (octant_bucket mx my mz 5) pts) +
   length (List.filter (octant_bucket mx my mz 6) pts) +
   length (List.filter (octant_bucket mx my mz 7) pts) = length pts)%nat.
Proof.
  induction pts as [|p rest IH]; [reflexivity |].
  simpl.
  set (f0 := List.filter (octant_bucket mx my mz 0) rest) in *.
  set (f1 := List.filter (octant_bucket mx my mz 1) rest) in *.
  set (f2 := List.filter (octant_bucket mx my mz 2) rest) in *.
  set (f3 := List.filter (octant_bucket mx my mz 3) rest) in *.
  set (f4 := List.filter (octant_bucket mx my mz 4) rest) in *.
  set (f5 := List.filter (octant_bucket mx my mz 5) rest) in *.
  set (f6 := List.filter (octant_bucket mx my mz 6) rest) in *.
  set (f7 := List.filter (octant_bucket mx my mz 7) rest) in *.
  unfold octant_bucket.
  destruct (Z.geb (px3 p) mx), (Z.geb (py3 p) my), (Z.geb (pz3 p) mz); simpl; lia.
Qed.

(**md
**3D `Octree` clamped cuboid closest-point squared distance lower-bounds cell members and 8-way octant partitioning conserves all points.**

Justifies squared-distance priority and child pre-filtering in `Octree.nearest` as well as in-place 8-way counting-partition slice construction in `Octree.buildWithCancellation`.

- [cell:] 3D axis-aligned bounding cuboid of an `Octree` node.
- [q:] 3D query point.
- [p:] any 3D point inside `cell`.
- [mx:] X split midpoint.
- [my:] Y split midpoint.
- [mz:] Z split midpoint.
- [pts:] points partitioned across the eight child octants.
*)
Theorem octree_aabb3_lower_bound_and_leaf_conservation
    (cell : aabb3) (q p : pt3) (mx my mz : Z) (pts : list pt3) :
  (in_aabb3 cell p = true -> dist_sq3 q (closest_on_aabb3 cell q) <= dist_sq3 q p) /\
  (length (List.filter (octant_bucket mx my mz 0) pts) +
   length (List.filter (octant_bucket mx my mz 1) pts) +
   length (List.filter (octant_bucket mx my mz 2) pts) +
   length (List.filter (octant_bucket mx my mz 3) pts) +
   length (List.filter (octant_bucket mx my mz 4) pts) +
   length (List.filter (octant_bucket mx my mz 5) pts) +
   length (List.filter (octant_bucket mx my mz 6) pts) +
   length (List.filter (octant_bucket mx my mz 7) pts) = length pts)%nat.
Proof.
  split.
  - intros Hin. unfold in_aabb3 in Hin.
    repeat (apply andb_prop in Hin as [Hin ?]).
    apply Z.leb_le in Hin. apply Z.leb_le in H. apply Z.leb_le in H0.
    apply Z.leb_le in H1. apply Z.leb_le in H2. apply Z.leb_le in H3.
    unfold dist_sq3, closest_on_aabb3; simpl.
    pose proof (clamp_1d_sq_le (min_x3 cell) (max_x3 cell) (px3 q) (px3 p) Hin H3).
    pose proof (clamp_1d_sq_le (min_y3 cell) (max_y3 cell) (py3 q) (py3 p) H2 H1).
    pose proof (clamp_1d_sq_le (min_z3 cell) (max_z3 cell) (pz3 q) (pz3 p) H0 H).
    lia.
  - apply partition_eight_counts.
Qed.

(**md
**`ForceAtlas2` / `LinLog` Barnes–Hut opening test (`sz^2 < theta^2 * d^2`), `1/d` repulsion, and linear attraction/gravity eliminate `sqrt`.**

For nonnegative `sz, theta, d >= 0`: (1) `sz < theta * d <-> sz^2 < theta^2 * d^2`, avoiding `sqrt` on every visited Barnes–Hut cell; (2) `(dx / d) * (kr_m / d) = (dx * kr_m) / d^2` (cross-multiplied); and (3) `(dx / d) * (w * d) = dx * w` for `ForceAtlas2` attraction and central gravity.

- [sz:] Barnes–Hut cell extent width.
- [theta:] Barnes–Hut opening angle parameter (`theta >= 0`).
- [d:] Euclidean distance (`d >= 0`).
- [dx:] coordinate separation along X, Y, or Z.
- [kr_m:] repulsion numerator (`kr * m_i * m_cell`).
- [w:] linear spring or central-gravity weight.
*)
Theorem forceatlas2_rational_repulsion_and_linear_forces_cancel_sqrt
    (sz theta d dx kr_m w : Z) :
  0 <= sz ->
  0 <= theta ->
  0 <= d ->
  (sz < theta * d <-> sz * sz < (theta * theta) * (d * d)) /\
  (dx * kr_m) * (d * d) = (dx * d) * (kr_m * d) /\
  dx * (w * d) = (dx * w) * d.
Proof.
  intros Hsz Hth Hd.
  split.
  - split; intros Hlt.
    + assert (Hnn : 0 <= theta * d) by nia.
      replace ((theta * theta) * (d * d)) with ((theta * d) * (theta * d)) by ring.
      nia.
    + replace ((theta * theta) * (d * d)) with ((theta * d) * (theta * d)) in Hlt by ring.
      assert (Hcases : sz < theta * d \/ theta * d <= sz) by lia.
      destruct Hcases as [Hok | Hge]; [exact Hok |].
      assert (Hnn : 0 <= theta * d) by nia.
      nia.
  - split; ring.
Qed.

(**md
**Minkowski `(3+1)D` upper-sheet points project strictly inside the open Klein & Poincaré unit balls (`H3` & `Walrus`).**

Whenever `x^2 + y^2 + z^2 - w^2 = -1` and `1 <= w`, `1 + x^2 + y^2 + z^2 = w^2`, `x^2 + y^2 + z^2 < w^2` (so Klein `||(x/w, y/w, z/w)|| < 1`), and `(1 + w)^2 - (x^2 + y^2 + z^2) = 2 * (1 + w)` (so Poincaré `1 - ||u||^2 = 2 / (1 + w)`), justifying fused SIMD `projectBallSoA` and direct endpoint reuse for Klein straight-chord geodesics.

- [x:] spacelike X coordinate.
- [y:] spacelike Y coordinate.
- [z:] spacelike Z coordinate.
- [w:] timelike W coordinate (`w >= 1`).
*)
Theorem minkowski_sheet_ball_and_klein_chord_identities (x y z w : Z) :
  x * x + y * y + z * z - w * w = -1 ->
  1 <= w ->
  1 + x * x + y * y + z * z = w * w /\
  x * x + y * y + z * z < w * w /\
  (1 + w) * (1 + w) - (x * x + y * y + z * z) = 2 * (1 + w).
Proof.
  intros Hsheet Hw.
  repeat split; lia.
Qed.

Definition bip_crosses (pos : Z -> Z) (u1 v1 u2 v2 : Z) : Prop :=
  (u1 < u2 /\ pos v2 < pos v1) \/ (u2 < u1 /\ pos v1 < pos v2).

Definition swap_adj_pos (pos : Z -> Z) (a b k v : Z) : Z :=
  if Z.eqb v a then k + 1
  else if Z.eqb v b then k
  else pos v.

(**md
**`Sugiyama` adjacent-swap crossing delta depends exclusively on edges incident to `(a, b)`.**

When `pos a = k`, `pos b = k + 1`, and every other vertex `c` has `pos c <> k` and `pos c <> k + 1`, swapping `a` and `b` leaves `bip_crosses` unchanged on every edge pair except `(u1, a)` vs `(u2, b)` (and symmetrically `(u1, b)` vs `(u2, a)`), where `(u1, a)` and `(u2, b)` cross before iff `u2 < u1` and after iff `u1 < u2`.

- [pos:] current 0-based horizontal layer position map.
- [a:] left adjacent vertex at `pos a = k`.
- [b:] right adjacent vertex at `pos b = k + 1` (`a <> b`).
- [k:] adjacent slot index.
- [u1:] upper-layer neighbor of `v1`.
- [v1:] lower-layer endpoint 1.
- [u2:] upper-layer neighbor of `v2`.
- [v2:] lower-layer endpoint 2.
*)
Theorem sugiyama_adjacent_swap_crossing_delta
    (pos : Z -> Z) (a b k u1 v1 u2 v2 : Z) :
  a <> b ->
  pos a = k ->
  pos b = k + 1 ->
  (forall c, c <> a -> c <> b -> pos c <> k /\ pos c <> k + 1) ->
  let pos' := swap_adj_pos pos a b k in
  ((~(v1 = a /\ v2 = b) /\ ~(v1 = b /\ v2 = a)) ->
    (bip_crosses pos' u1 v1 u2 v2 <-> bip_crosses pos u1 v1 u2 v2)) /\
  (bip_crosses pos u1 a u2 b <-> u2 < u1) /\
  (bip_crosses pos' u1 a u2 b <-> u1 < u2).
Proof.
  intros Hab Ha Hb Hother. cbv zeta.
  split.
  - intros [Hnot_ab Hnot_ba].
    unfold bip_crosses, swap_adj_pos.
    destruct (Z.eqb_spec v1 a) as [Hv1a | Hv1a];
    destruct (Z.eqb_spec v1 b) as [Hv1b | Hv1b];
    destruct (Z.eqb_spec v2 a) as [Hv2a | Hv2a];
    destruct (Z.eqb_spec v2 b) as [Hv2b | Hv2b];
    try (exfalso; congruence);
    try (exfalso; apply Hnot_ab; split; assumption);
    try (exfalso; apply Hnot_ba; split; assumption);
    try tauto;
    try (destruct (Hother v2 Hv2a Hv2b); rewrite ?Hv1a, ?Hv1b in *; split; intros [[? ?] | [? ?]]; lia);
    try (destruct (Hother v1 Hv1a Hv1b); rewrite ?Hv2a, ?Hv2b in *; split; intros [[? ?] | [? ?]]; lia);
    (rewrite ?Hv1a, ?Hv1b, ?Hv2a, ?Hv2b in *; split; intros [[? ?] | [? ?]]; lia).
  - unfold bip_crosses, swap_adj_pos.
    assert (Haa : Z.eqb a a = true) by (apply Z.eqb_eq; reflexivity).
    assert (Hbb : Z.eqb b b = true) by (apply Z.eqb_eq; reflexivity).
    assert (Hba : Z.eqb b a = false) by (apply Z.eqb_neq; congruence).
    rewrite Haa, Hba, Hbb, Ha, Hb.
    split; (split; [intros [[? ?] | [? ?]]; lia | intros ?; lia]).
Qed.

Definition accept_sparse_pivot_pair (is_piv : Z -> bool) (lp lj d : Z) : bool :=
  Z.ltb 1 d && (negb (is_piv lj) || Z.ltb lp lj).

(**md
**`HashSet`-free sparse `Stress2D` / `Stress3D` edge + pivot pair emission visits each unordered pair `{u, v}` at most once.**

Emitting undirected edges (`d = 1`) first and then emitting pivot pairs `(lp, lj)` with `d > 1` guarded by `negb (is_piv lj) || lp < lj` covers every edge and pivot pair while guaranteeing that `(lp, lj)` neither collides with any `d = 1` edge nor gets emitted in reverse as `(lj, lp)`.

- [is_piv:] boolean membership flag for the `k`-center landmark pivot set.
- [lp:] current landmark pivot index (`is_piv lp = true`).
- [lj:] target vertex index (`lp <> lj`).
- [d:] shortest-path hop distance `dist(lp, lj)`.
*)
Theorem sparse_stress_pivot_edge_dedup_exact
    (is_piv : Z -> bool) (lp lj d : Z) :
  is_piv lp = true ->
  lp <> lj ->
  (accept_sparse_pivot_pair is_piv lp lj 1 = false) /\
  (is_piv lj = true -> 1 < d ->
    accept_sparse_pivot_pair is_piv lp lj d = negb (accept_sparse_pivot_pair is_piv lj lp d)).
Proof.
  intros Hpiv_lp Hneq.
  unfold accept_sparse_pivot_pair.
  split.
  - reflexivity.
  - intros Hpiv_lj Hd.
    assert (Hd_true : Z.ltb 1 d = true) by (apply Z.ltb_lt; exact Hd).
    rewrite Hd_true, Hpiv_lp, Hpiv_lj; simpl.
    destruct (Z.ltb lp lj) eqn:Hlt1; destruct (Z.ltb lj lp) eqn:Hlt2; auto.
    + apply Z.ltb_lt in Hlt1. apply Z.ltb_lt in Hlt2. lia.
    + apply Z.ltb_ge in Hlt1. apply Z.ltb_ge in Hlt2. lia.
Qed.

(**md
**Contiguous arena reservation (`firstChild .. firstChild + childCount - 1`) indexes every child cell without pointer arrays (`FlatOctree` & `FlatQuadtree2D`).**

When `children` are appended at `first_child = length arena` before any descendant allocations (`arena ++ children ++ descendants`), looking up `nth_error` at `first_child + k` for any `k < length children` retrieves the exact `k`-th child `nth_error children k`, eliminating per-node child-array and leaf-list heap allocations in 2D/3D Barnes–Hut trees.

- [arena:] prefix of already-reserved flat nodes (`first_child = length arena`).
- [children:] contiguous block of child nodes reserved for the current internal cell.
- [descendants:] nodes subsequently appended by recursive child subdivisions.
- [k:] child offset (`0 <= k < length children`).
*)
Theorem flat_cell_children_contiguous_slice_exact
    {N : Type} (arena children descendants : list N) (k : nat) :
  (k < length children)%nat ->
  nth_error (arena ++ children ++ descendants) (length arena + k)%nat =
  nth_error children k.
Proof.
  intros Hk.
  rewrite nth_error_app2 by lia.
  replace (length arena + k - length arena)%nat with k by lia.
  rewrite nth_error_app1 by exact Hk.
  reflexivity.
Qed.
