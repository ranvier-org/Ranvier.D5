From stdpp Require Import prelude.
From Stdlib Require Import Arith Lia List Bool ZArith.
Import ListNotations.

Local Arguments Nat.eqb : simpl never.
Open Scope Z_scope.

(**md # Data summaries, binning, keyed joins, continuous scales, and SIMD reductions

Models the discrete and algebraic contracts of `Ranvier.D5.Data`, `Ranvier.D5.Scale`, `Ranvier.D5.Random.Distribution`, and SIMD lane-chunked numeric reductions. *)

(** ** 1. Bisect-right and half-open / inclusive-final-edge binning *)

Fixpoint bisect_right_list (edges : list Z) (x : Z) : nat :=
  match edges with
  | [] => 0%nat
  | e :: rest =>
    if Z.leb e x then S (bisect_right_list rest x) else 0%nat
  end.

Inductive sorted_strict : list Z -> Prop :=
  | SortedNil : sorted_strict []
  | SortedOne e : sorted_strict [e]
  | SortedCons a b rest :
    a < b -> sorted_strict (b :: rest) -> sorted_strict (a :: b :: rest).

Lemma sorted_strict_head_lt a rest y :
  sorted_strict (a :: rest) -> In y rest -> a < y.
Proof.
  intros Hsort Hin.
  remember (a :: rest) as xs eqn:Heq.
  revert a rest Heq Hin.
  induction Hsort as [| e | a0 b0 tl Hab Hsub IH]; intros a rest Heq Hin; try discriminate.
  - injection Heq as <- <-. inversion Hin.
  - injection Heq as <- <-. simpl in Hin.
    destruct Hin as [-> | Hin]; [lia |].
    pose proof (IH b0 tl eq_refl Hin). lia.
Qed.

(**md
**`bisect_right_list` partitions sorted edges into `<= x` on the left and `> x` on the right.**

On strictly increasing bin edges, `bisect_right_list edges x` returns `idx <= length edges` such that every edge before `idx` satisfies `e <= x` and every edge from `idx` onward satisfies `x < e`.

- [edges:] strictly increasing list of bin edges.
- [x:] the finite observation being located.
*)
Theorem bisect_right_partition (edges : list Z) (x : Z) :
  sorted_strict edges ->
  (bisect_right_list edges x <= length edges)%nat /\
  (forall j e, (j < bisect_right_list edges x)%nat -> nth_error edges j = Some e -> e <= x) /\
  (forall j e, (bisect_right_list edges x <= j)%nat -> nth_error edges j = Some e -> x < e).
Proof.
  intros Hsort.
  induction Hsort as [| e | a b rest Hab Hsub [IHlen [IHlo IHhi]]]; simpl.
  - repeat split; [lia | intros j e Hj; lia | intros [|j] e _ Hn; discriminate].
  - destruct (Z.leb e x) eqn:He; simpl.
    + apply Z.leb_le in He. repeat split; [lia | |].
      * intros [|j] v Hj Hn; [injection Hn as <-; lia | lia].
      * intros [|[|j]] v Hj Hn; simpl in Hn; try lia; discriminate.
    + apply Z.leb_gt in He. repeat split; [lia | intros j v Hj; lia |].
      intros [|[|j]] v _ Hn; simpl in Hn; [injection Hn as <-; lia | discriminate | discriminate].
  - destruct (Z.leb a x) eqn:Ha; simpl in *.
    + apply Z.leb_le in Ha.
      destruct (Z.leb b x) eqn:Hb; simpl in *.
      * repeat split; [lia | |].
        -- intros [|j] v Hj Hn; [injection Hn as <-; lia | apply (IHlo j v ltac:(lia) Hn)].
        -- intros [|j] v Hj Hn; [lia | apply (IHhi j v ltac:(lia) Hn)].
      * repeat split; [lia | |].
        -- intros [|j] v Hj Hn; [injection Hn as <-; lia | lia].
        -- intros [|[|j]] v Hj Hn; simpl in Hn.
           ++ lia.
           ++ apply Z.leb_gt in Hb. injection Hn as <-; lia.
           ++ apply Z.leb_gt in Hb.
              assert (Hin : In v rest) by (eapply nth_error_In; exact Hn).
              pose proof (sorted_strict_head_lt b rest v Hsub Hin). lia.
    + apply Z.leb_gt in Ha. repeat split; [lia | intros j v Hj; lia |].
      intros [|[|j]] v _ Hn; simpl in Hn.
      * injection Hn as <-; lia.
      * injection Hn as <-; lia.
      * assert (Hin : In v rest) by (eapply nth_error_In; exact Hn).
        pose proof (sorted_strict_head_lt b rest v Hsub Hin). lia.
Qed.

Fixpoint last_nonempty (x : Z) (rest : list Z) : Z :=
  match rest with
  | [] => x
  | y :: tl => last_nonempty y tl
  end.

Definition bin_index (edges : list Z) (x : Z) : option nat :=
  match edges with
  | [] | [_] => None
  | lo :: b :: rest =>
    let hi := last_nonempty b rest in
    if Z.ltb x lo || Z.ltb hi x then None
    else
      let k := S (length rest) in
      let raw := Nat.pred (bisect_right_list edges x) in
      Some (if Nat.ltb (Nat.pred k) raw then Nat.pred k else raw)
  end.

Definition in_bin_bounds (edges : list Z) (idx : nat) (x : Z) : Prop :=
  exists lo hi,
    nth_error edges idx = Some lo /\
    nth_error edges (S idx) = Some hi /\
    lo <= x /\
    (if Nat.eqb (S (S idx)) (length edges) then x <= hi else x < hi).

Lemma nth_error_last_nonempty (a b : Z) (rest : list Z) :
  nth_error (a :: b :: rest) (S (length rest)) = Some (last_nonempty b rest).
Proof.
  revert a b. induction rest as [|c tl IH]; intros a b; cbn; [reflexivity | apply (IH b c)].
Qed.

(**md
**`Data.bin` assigns every in-range observation to a unique valid bin (`[lo, hi)` or final `[lo, hi]`).**

Given strictly increasing edges with at least two boundaries (`k >= 1` bins), `bin_index` returns `Some idx` with `idx < k` satisfying `lo <= x < hi` on interior bins and `lo <= x <= hi` on the final bin, and `None` outside `[edges[0], edges[k]]`.

- [a:] first (lower) bin boundary.
- [b:] second bin boundary.
- [rest:] remaining upper bin boundaries.
- [x:] the observation to classify.
*)
Theorem data_bin_unique_membership (a b : Z) (rest : list Z) (x : Z) :
  sorted_strict (a :: b :: rest) ->
  let edges := a :: b :: rest in
  let hi_max := last_nonempty b rest in
  if Z.ltb x a || Z.ltb hi_max x then
    bin_index edges x = None
  else
    exists idx,
      bin_index edges x = Some idx /\
      (idx < S (length rest))%nat /\
      in_bin_bounds edges idx x.
Proof.
  intros Hsort. cbv zeta.
  unfold bin_index.
  destruct (Z.ltb x a || Z.ltb (last_nonempty b rest) x) eqn:Hout; [reflexivity |].
  apply orb_false_elim in Hout as [Hge Hle].
  apply Z.ltb_ge in Hge. apply Z.ltb_ge in Hle.
  pose proof (bisect_right_partition (a :: b :: rest) x Hsort) as [Hblen [Hblo Hbhi]].
  cbn [length] in Hblen.
  assert (Hbpos : (1 <= bisect_right_list (a :: b :: rest) x)%nat).
  { simpl. destruct (Z.leb a x) eqn:Ha; [lia | apply Z.leb_gt in Ha; lia]. }
  set (k := S (length rest)).
  set (raw := Nat.pred (bisect_right_list (a :: b :: rest) x)).
  set (idx := if Nat.ltb (Nat.pred k) raw then Nat.pred k else raw).
  assert (Hidx_le_raw : (idx <= raw)%nat).
  { unfold idx. destruct (Nat.ltb (Nat.pred k) raw) eqn:Hcmp; [apply Nat.ltb_lt in Hcmp; lia | lia]. }
  assert (Hidx_lt : (idx < k)%nat).
  { unfold idx, k. destruct (Nat.ltb (Nat.pred (S (length rest))) raw) eqn:Hcmp;
      [lia | apply Nat.ltb_ge in Hcmp; lia]. }
  exists idx. split; [reflexivity |].
  split; [exact Hidx_lt |].
  unfold in_bin_bounds.
  assert (Hlen_edges : length (a :: b :: rest) = S k) by (unfold k; cbn; reflexivity).
  destruct (nth_error (a :: b :: rest) idx) as [lo |] eqn:Hlo_nth;
    [| apply nth_error_None in Hlo_nth; lia].
  destruct (nth_error (a :: b :: rest) (S idx)) as [hi |] eqn:Hhi_nth;
    [| apply nth_error_None in Hhi_nth; lia].
  exists lo, hi. split; [reflexivity | split; [reflexivity | split]].
  - apply (Hblo idx lo ltac:(unfold raw in Hidx_le_raw; lia) Hlo_nth).
  - destruct (Nat.eqb (S (S idx)) (length (a :: b :: rest))) eqn:Hlast.
    + apply Nat.eqb_eq in Hlast.
      assert (HSidx : S idx = S (length rest)) by (simpl in Hlast; lia).
      pose proof (nth_error_last_nonempty a b rest) as Hlast_nth.
      rewrite <- HSidx in Hlast_nth.
      rewrite Hhi_nth in Hlast_nth. injection Hlast_nth as ->. lia.
    + apply Nat.eqb_neq in Hlast.
      assert (Hlt_bisect : (bisect_right_list (a :: b :: rest) x <= S idx)%nat).
      { subst idx raw k.
        destruct (Nat.ltb (Nat.pred (S (length rest))) (Nat.pred (bisect_right_list (a :: b :: rest) x))) eqn:Hcmp.
        - exfalso; apply Hlast; simpl; reflexivity.
        - apply Nat.ltb_ge in Hcmp. lia. }
      apply (Hbhi (S idx) hi Hlt_bisect Hhi_nth).
Qed.

(** ** 2. Keyed `Data.join` partition and order preservation *)

Fixpoint mem_key (k : nat) (xs : list nat) : bool :=
  match xs with
  | [] => false
  | y :: rest => Nat.eqb k y || mem_key k rest
  end.

Record join_model := {
  jm_enter : list nat;
  jm_update : list nat;
  jm_exit : list nat
}.

Definition data_join_keys (prev next : list nat) : join_model :=
  {| jm_enter := List.filter (fun k => negb (mem_key k prev)) next;
     jm_update := List.filter (fun k => mem_key k prev) next;
     jm_exit := List.filter (fun k => negb (mem_key k next)) prev |}.

Lemma filter_compl_length {A : Type} (p : A -> bool) (xs : list A) :
  (length (List.filter p xs) + length (List.filter (fun x => negb (p x)) xs) = length xs)%nat.
Proof.
  induction xs as [|x rest IH]; simpl; [reflexivity |].
  destruct (p x); simpl; lia.
Qed.

(**md
**`Data.join` partitions `next` into `Enter ⊔ Update` and `previous` into `Exit ⊔ Update` without losing keys.**

Every key in `next` appears in `jm_enter` or `jm_update` (and never both), preserving exact `next` count, while every key in `prev` absent from `next` appears in `jm_exit`.

- [prev:] ordered keys from the previous dataset.
- [next:] ordered keys from the incoming dataset.
- [k:] any candidate key tested for membership.
*)
Theorem data_join_partition_and_order (prev next : list nat) (k : nat) :
  let j := data_join_keys prev next in
  (length (jm_enter j) + length (jm_update j) = length next)%nat /\
  (In k (jm_enter j) <-> In k next /\ mem_key k prev = false) /\
  (In k (jm_update j) <-> In k next /\ mem_key k prev = true) /\
  (In k (jm_exit j) <-> In k prev /\ mem_key k next = false).
Proof.
  cbv zeta. unfold data_join_keys; simpl.
  pose proof (filter_compl_length (fun x => mem_key x prev) next) as Hlen.
  split; [lia | split; [split | split; [split | split]]].
  - intros Hin. apply filter_In in Hin as [H1 H2]. apply negb_true_iff in H2. split; assumption.
  - intros [H1 H2]. apply filter_In. split; [exact H1 | apply negb_true_iff; exact H2].
  - intros Hin. apply filter_In in Hin; exact Hin.
  - intros Hboth. apply filter_In; exact Hboth.
  - intros Hin. apply filter_In in Hin as [H1 H2]. apply negb_true_iff in H2. split; assumption.
  - intros [H1 H2]. apply filter_In. split; [exact H1 | apply negb_true_iff; exact H2].
Qed.

(** ** 3. Continuous scale clamping, `Scale.nice` enclosure, and `Distribution.integer` bounds *)

Definition clamp_z (lo hi v : Z) : Z :=
  let mn := Z.min lo hi in
  let mx := Z.max lo hi in
  Z.max mn (Z.min mx v).

(**md
**Clamping a mapped scale output confines every result to `[min c d, max c d]` under normal or reversed ranges.**

Whether the scale range `(c, d)` is ascending (`c <= d`) or reversed (`d < c`), `clamp_z c d v` always lies within `[Z.min c d, Z.max c d]`.

- [c:] range start coordinate.
- [d:] range end coordinate.
- [v:] un-clamped mapped coordinate.
*)
Theorem scale_clamp_within_range (c d v : Z) :
  Z.min c d <= clamp_z c d v /\ clamp_z c d v <= Z.max c d.
Proof.
  unfold clamp_z. lia.
Qed.

Definition floor_step (x step : Z) : Z := (x / step) * step.
Definition ceil_step (x step : Z) : Z := - ((- x) / step) * step.

Definition nice_direct (lo hi step : Z) : Z * Z :=
  (floor_step lo step, ceil_step hi step).

Definition interior_ticks_two (lo hi step : Z) : bool :=
  let first := - ((- lo) / step) in
  let last := hi / step in
  Z.leb (first + 1) last.

Definition nice_via_interior_ticks (lo hi step : Z) : Z * Z :=
  if interior_ticks_two lo hi step then nice_direct lo hi step else (lo, hi).

(**md
**Direct `Scale.nice` step rounding encloses `[lo, hi]` as multiples of `step` regardless of interior tick count.**

Computing `floor_step lo step` and `ceil_step hi step` directly from `step > 0` guarantees `lo' <= lo <= hi <= hi'` and exact divisibility by `step`, even when zero or one tick falls strictly inside `[lo, hi]`.

- [lo:] lower domain endpoint.
- [hi:] upper domain endpoint (`lo <= hi`).
- [step:] positive 1/2/5 tick step (`0 < step`).
*)
Theorem scale_nice_encloses_domain (lo hi step : Z) :
  0 < step ->
  lo <= hi ->
  let '(lo', hi') := nice_direct lo hi step in
  lo' <= lo /\ hi <= hi' /\ lo' mod step = 0 /\ hi' mod step = 0.
Proof.
  intros Hstep Hle. unfold nice_direct, floor_step, ceil_step.
  pose proof (Z.div_mod lo step ltac:(lia)) as Hdm1.
  pose proof (Z.mod_pos_bound lo step Hstep) as Hmb1.
  pose proof (Z.div_mod (- hi) step ltac:(lia)) as Hdm2.
  pose proof (Z.mod_pos_bound (- hi) step Hstep) as Hmb2.
  repeat split; try lia.
  - apply Z_mod_mult.
  - replace (- (- hi / step) * step) with ((- (- hi / step)) * step) by lia.
    apply Z_mod_mult.
Qed.

(**md
**Naive `Scale.nice` delegating to `ticks` fails to round `(101, 199)` at `step = 100` (Issue #64).**

When `[lo, hi] = [101, 199]` and `step = 100`, `interior_ticks_two` is `false` (zero multiples of `100` lie inside `[101, 199]`), leaving `(101, 199)` unrounded instead of `(100, 200)`.

- [u:] unit witness triggering the counterexample.
*)
Theorem naive_nice_from_interior_ticks_falsified (u : unit) :
  let _ := u in
  nice_via_interior_ticks 101 199 100 = (101, 199) /\
  nice_direct 101 199 100 = (100, 200).
Proof.
  vm_compute. split; reflexivity.
Qed.

Definition integer_draw (floor_lo floor_hi offset : Z) : Z :=
  Z.min (floor_hi - 1) (floor_lo + offset).

(**md
**Requiring `floor_lo < floor_hi` keeps every `Distribution.integer` sample inside `[floor_lo, floor_hi)`.**

Whenever `floor_lo < floor_hi` and `0 <= offset < floor_hi - floor_lo`, `integer_draw` returns an integer strictly within `[floor_lo, floor_hi)`.

- [floor_lo:] floored lower endpoint (`floor low`).
- [floor_hi:] floored upper endpoint (`floor high`).
- [offset:] uniform integer step in `[0, floor_hi - floor_lo)`.
*)
Theorem integer_dist_floor_bounds_safe (floor_lo floor_hi offset : Z) :
  floor_lo < floor_hi ->
  0 <= offset ->
  offset < floor_hi - floor_lo ->
  floor_lo <= integer_draw floor_lo floor_hi offset /\
  integer_draw floor_lo floor_hi offset < floor_hi.
Proof.
  intros Hlt H0 Hoff. unfold integer_draw. lia.
Qed.

(**md
**Accepting `low < high` without `floor low < floor high` draws `-1` on `[0.2, 0.9)` (Issue #63).**

When `floor_lo = 0` and `floor_hi = 0` (from `low = 0.2 < high = 0.9`), `integer_draw 0 0 0` evaluates to `-1 < floor_lo`.

- [u:] unit witness triggering the counterexample.
*)
Theorem naive_integer_dist_bounds_falsified (u : unit) :
  let _ := u in
  integer_draw 0 0 0 = -1 /\ integer_draw 0 0 0 < 0.
Proof.
  change (integer_draw 0 0 0) with (-1). split; [reflexivity | lia].
Qed.

(** ** 4. Algebraic equivalence of SIMD `W`-lane chunked reduction + scalar tail *)

Fixpoint sum_z (xs : list Z) : Z :=
  match xs with
  | [] => 0
  | x :: rest => x + sum_z rest
  end.

Fixpoint simd_chunked_sum (fuel width : nat) (xs : list Z) : Z :=
  match fuel with
  | 0%nat => sum_z xs
  | S f =>
    if Nat.ltb (length xs) width || Nat.eqb width 0%nat then sum_z xs
    else sum_z (firstn width xs) + simd_chunked_sum f width (skipn width xs)
  end.

Lemma sum_z_app (a b : list Z) :
  sum_z (a ++ b) = sum_z a + sum_z b.
Proof.
  induction a as [|x rest IH]; simpl; [lia | rewrite IH; lia].
Qed.

(**md
**SIMD `W`-lane vector accumulation plus scalar remainder equals exact sequential `sum_z`.**

For any vector lane width (`width` = 2, 4, or 8 for `Vector128`/`Vector256`/`Vector512<double>`) and loop fuel, summing `firstn width` chunks followed by the scalar tail produces the exact sum of `xs`.

- [fuel:] maximum number of SIMD vector iterations.
- [width:] SIMD register lane count (`Vector128` = 2, `Vector256` = 4, `Vector512` = 8).
- [xs:] contiguous numeric slice to reduce.
*)
Theorem simd_chunked_sum_eq_fold (fuel width : nat) (xs : list Z) :
  simd_chunked_sum fuel width xs = sum_z xs.
Proof.
  revert xs. induction fuel as [|f IH]; intros xs; simpl; [reflexivity |].
  destruct (Nat.ltb (length xs) width || Nat.eqb width 0%nat); [reflexivity |].
  rewrite IH.
  rewrite <- sum_z_app.
  rewrite firstn_skipn.
  reflexivity.
Qed.
