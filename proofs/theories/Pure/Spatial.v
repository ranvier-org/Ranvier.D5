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
