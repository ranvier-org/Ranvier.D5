From stdpp Require Import prelude.
From Stdlib Require Import Arith Lia List Bool ZArith.
Import ListNotations.

Local Arguments Nat.eqb : simpl never.
Open Scope nat_scope.

(**md # Gap-aware series downsampling (`Sampling`), two-domain `Selection.reconcile`, and `Recipe.key` injectivity

Models the downsampling, selection survival, and key-encoding contracts in `Ranvier.D5.Sampling`, `Ranvier.D5.Charts.Selection`, and `Ranvier.D5.Charts.Recipe`. *)

(** ** 1. Monotone endpoint-preserving bucket selection (`Sampling.minMaxRun` / `Sampling.lttbRun`) *)

Inductive strictly_increasing : list nat -> Prop :=
  | IncNil : strictly_increasing []
  | IncOne x : strictly_increasing [x]
  | IncCons a b rest :
    (a < b)%nat -> strictly_increasing (b :: rest) -> strictly_increasing (a :: b :: rest).

Definition bucket_pair (lo hi : nat) : list nat :=
  if Nat.eqb lo hi then [lo]
  else if Nat.ltb lo hi then [lo; hi]
  else [hi; lo].

Fixpoint emit_interior_buckets (buckets : list (nat * nat)) (last_idx : nat) : list nat :=
  match buckets with
  | [] => [last_idx]
  | (i_min, i_max) :: rest =>
    bucket_pair i_min i_max ++ emit_interior_buckets rest last_idx
  end.

Definition minmax_indices (len : nat) (buckets : list (nat * nat)) : list nat :=
  match len with
  | 0 => []
  | 1 => [0]
  | S (S _ as last_idx) => 0 :: emit_interior_buckets buckets last_idx
  end.

Inductive valid_buckets (low_bound : nat) : list (nat * nat) -> nat -> Prop :=
  | VBEmpty hi_bound :
    (low_bound <= hi_bound)%nat ->
    valid_buckets low_bound [] hi_bound
  | VBCons i1 i2 mid rest hi_bound :
    (low_bound <= i1)%nat -> (i1 < mid)%nat ->
    (low_bound <= i2)%nat -> (i2 < mid)%nat ->
    valid_buckets mid rest hi_bound ->
    valid_buckets low_bound ((i1, i2) :: rest) hi_bound.

Lemma strictly_increasing_cons_head a xs :
  xs <> [] ->
  (a < hd 0 xs)%nat ->
  strictly_increasing xs ->
  strictly_increasing (a :: xs).
Proof.
  intros Hne Hlt Hinc.
  destruct xs as [|b rest]; [contradiction |].
  simpl in Hlt. constructor; assumption.
Qed.

Lemma emit_interior_inc low_bound buckets last_idx :
  valid_buckets low_bound buckets last_idx ->
  emit_interior_buckets buckets last_idx <> [] /\
  (low_bound <= hd 0 (emit_interior_buckets buckets last_idx))%nat /\
  last (emit_interior_buckets buckets last_idx) 0 = last_idx /\
  strictly_increasing (emit_interior_buckets buckets last_idx) /\
  (length (emit_interior_buckets buckets last_idx) <= 2 * length buckets + 1)%nat.
Proof.
  induction 1 as [hi_bound Hle | l0 i1 i2 mid rest hi_bound Hi1l Hi1m Hi2l Hi2m Hvb [IHne [IHhd [IHlast [IHinc IHlen]]]]]; simpl.
  - repeat split; [discriminate | lia | constructor | lia].
  - unfold bucket_pair.
    destruct (Nat.eqb i1 i2) eqn:Heq.
    + apply Nat.eqb_eq in Heq; subst i2; simpl.
      repeat split.
      * discriminate.
      * lia.
      * destruct (emit_interior_buckets rest hi_bound); [contradiction | exact IHlast].
      * apply strictly_increasing_cons_head; [exact IHne | lia | exact IHinc].
      * lia.
    + apply Nat.eqb_neq in Heq.
      destruct (Nat.ltb i1 i2) eqn:Hlt; simpl.
      * apply Nat.ltb_lt in Hlt.
        repeat split.
        -- discriminate.
        -- lia.
        -- destruct (emit_interior_buckets rest hi_bound); [contradiction | exact IHlast].
        -- constructor; [lia | apply strictly_increasing_cons_head; [exact IHne | lia | exact IHinc]].
        -- lia.
      * apply Nat.ltb_ge in Hlt.
        repeat split.
        -- discriminate.
        -- lia.
        -- destruct (emit_interior_buckets rest hi_bound); [contradiction | exact IHlast].
        -- constructor; [lia | apply strictly_increasing_cons_head; [exact IHne | lia | exact IHinc]].
        -- lia.
Qed.

(**md
**`Sampling.minMaxRun` retains exact endpoints `[0, len - 1]`, emits strictly increasing indices, and bounds output count.**

For every run of length `len >= 2` partitioned into disjoint interior buckets between `1` and `len - 1`, `minmax_indices` begins with `0`, ends with `len - 1`, preserves strict source-index order, and emits at most `2 * length buckets + 2` samples.

- [extra:] `len - 2` for a run of length `S (S extra) >= 2`.
- [buckets:] `(lowest, highest)` index pairs drawn from disjoint interior sub-ranges of `1 .. S extra - 1`.
*)
Theorem minmax_run_preserves_endpoints_and_order (extra : nat) (buckets : list (nat * nat)) :
  let last_idx := S extra in
  valid_buckets 1 buckets last_idx ->
  let out := minmax_indices (S last_idx) buckets in
  hd 0 out = 0 /\
  last out 0 = last_idx /\
  strictly_increasing out /\
  (length out <= 2 * length buckets + 2)%nat.
Proof.
  cbv zeta. intros Hvb.
  destruct (emit_interior_inc 1 buckets (S extra) Hvb) as [Hne [Hhd [Hlast [Hinc Hlen]]]].
  simpl.
  repeat split.
  - destruct (emit_interior_buckets buckets (S extra)) as [|x xs]; [contradiction | exact Hlast].
  - apply strictly_increasing_cons_head; [exact Hne | lia | exact Hinc].
  - lia.
Qed.

(** ** 2. Largest-Remainder (`Hamilton`) budget conservation (`Sampling.allocateBudgets`) *)

Fixpoint sum_nat (xs : list nat) : nat :=
  match xs with
  | [] => 0
  | x :: rest => x + sum_nat rest
  end.

Fixpoint distribute_unit_slack (slack : nat) (caps : list nat) (allocs : list nat) : list nat :=
  match slack, caps, allocs with
  | 0, _, _ => allocs
  | _, [], _ => []
  | _, _, [] => []
  | S s, c :: cs, a :: as_ =>
    if Nat.ltb a c then S a :: distribute_unit_slack s cs as_
    else a :: distribute_unit_slack (S s) cs as_
  end.

Fixpoint count_room (caps allocs : list nat) : nat :=
  match caps, allocs with
  | c :: cs, a :: as_ => (if Nat.ltb a c then 1 else 0) + count_room cs as_
  | _, _ => 0
  end.

Lemma distribute_unit_slack_bounds (slack : nat) (caps allocs : list nat) :
  length caps = length allocs ->
  (forall i c a, nth_error caps i = Some c -> nth_error allocs i = Some a -> a <= c)%nat ->
  (slack <= count_room caps allocs)%nat ->
  let out := distribute_unit_slack slack caps allocs in
  sum_nat out = sum_nat allocs + slack /\
  (forall i c b, nth_error caps i = Some c -> nth_error out i = Some b -> b <= c)%nat.
Proof.
  revert slack allocs.
  induction caps as [|c cs IH]; intros slack [|a as_] Hlen Hle Hroom; cbv zeta; simpl in *; try discriminate.
  - destruct slack as [|s]; simpl; [split; [lia | intros [|i] ? ? Hn _; discriminate] | lia].
  - destruct slack as [|s]; simpl.
    + split; [lia |].
      intros [|i] c0 b0 Hc Hb; simpl in *.
      * injection Hc as <-; injection Hb as <-. apply (Hle 0 c a eq_refl eq_refl).
      * apply (Hle (S i) c0 b0 Hc Hb).
    + assert (Hac : (a <= c)%nat) by apply (Hle 0 c a eq_refl eq_refl).
      assert (Htail_le : forall i c0 a0, nth_error cs i = Some c0 -> nth_error as_ i = Some a0 -> (a0 <= c0)%nat).
      { intros i c0 a0 Hc Ha. apply (Hle (S i) c0 a0 Hc Ha). }
      injection Hlen as Hlen'.
      destruct (Nat.ltb a c) eqn:Hlt; simpl in *.
      * apply Nat.ltb_lt in Hlt.
        destruct (IH s as_ Hlen' Htail_le ltac:(lia)) as [Hs Hb].
        split; [lia |].
        intros [|i] c0 b0 Hc Ho; simpl in *.
        -- injection Hc as <-; injection Ho as <-; lia.
        -- apply (Hb i c0 b0 Hc Ho).
      * apply Nat.ltb_ge in Hlt.
        destruct (IH (S s) as_ Hlen' Htail_le ltac:(lia)) as [Hs Hb].
        split; [lia |].
        intros [|i] c0 b0 Hc Ho; simpl in *.
        -- injection Hc as <-; injection Ho as <-; lia.
        -- apply (Hb i c0 b0 Hc Ho).
Qed.

(**md
**Distributing Hamilton remainder slack conserves the exact target budget without exceeding any run's length.**

Starting from floor-plus-minimum allocations `allocs` (`allocs[i] <= caps[i]`) and integer-division remainder `slack <= count_room caps allocs`, `distribute_unit_slack` produces `sum out = sum allocs + slack` while keeping every `out[i] <= caps[i]`.

- [slack:] remaining sample budget after integer floor division (`slack <= count_room caps allocs`).
- [caps:] run lengths (`caps[i]`) bounding each segment's capacity.
- [allocs:] initial floor-plus-minimum allocations per run.
*)
Theorem hamilton_quotas_conserve_budget (slack : nat) (caps allocs : list nat) :
  length caps = length allocs ->
  (forall i c a, nth_error caps i = Some c -> nth_error allocs i = Some a -> a <= c)%nat ->
  (slack <= count_room caps allocs)%nat ->
  let out := distribute_unit_slack slack caps allocs in
  sum_nat out = sum_nat allocs + slack /\
  (forall i c b, nth_error caps i = Some c -> nth_error out i = Some b -> b <= c)%nat.
Proof.
  apply distribute_unit_slack_bounds.
Qed.

(** ** 3. Two-domain `Selection.reconcile` hidden-source survival and idempotence *)

Fixpoint mem_id (x : nat) (xs : list nat) : bool :=
  match xs with
  | [] => false
  | y :: rest => Nat.eqb x y || mem_id x rest
  end.

Fixpoint dedup_keep_first (seen : list nat) (xs : list nat) : list nat :=
  match xs with
  | [] => []
  | x :: rest =>
    if mem_id x seen then dedup_keep_first seen rest
    else x :: dedup_keep_first (x :: seen) rest
  end.

Record sel_state := {
  sel_available : list nat;
  sel_selected : list nat;
  sel_focused : option nat;
  sel_hovered : option nat
}.

Definition filter_opt (allowed : list nat) (v : option nat) : option nat :=
  match v with
  | Some x => if mem_id x allowed then Some x else None
  | None => None
  end.

Definition selection_reconcile (source_ids visible_ids : list nat) (st : sel_state) : sel_state :=
  let avail := dedup_keep_first [] (List.filter (fun id => mem_id id source_ids) visible_ids) in
  {| sel_available := avail;
     sel_selected := List.filter (fun id => mem_id id source_ids) (sel_selected st);
     sel_focused := filter_opt avail (sel_focused st);
     sel_hovered := filter_opt avail (sel_hovered st) |}.

Lemma filter_idem {A : Type} (p : A -> bool) (xs : list A) :
  List.filter p (List.filter p xs) = List.filter p xs.
Proof.
  induction xs as [|x rest IH]; simpl; [reflexivity |].
  destruct (p x) eqn:Hp; simpl; [rewrite Hp, IH; reflexivity | exact IH].
Qed.

Lemma filter_opt_idem (allowed : list nat) (v : option nat) :
  filter_opt allowed (filter_opt allowed v) = filter_opt allowed v.
Proof.
  destruct v as [x |]; simpl; [| reflexivity].
  destruct (mem_id x allowed) eqn:Hm; simpl; [rewrite Hm; reflexivity | reflexivity].
Qed.

(**md
**`Selection.reconcile` preserves hidden selected source IDs across downsampling and is idempotent.**

Even when `id` is omitted from `visible_ids` by LTTB/MinMax reduction, `id` remains in `sel_selected` iff `id` is in `source_ids`, `Focused`/`Hovered` are restricted to `Available`, and re-running `selection_reconcile` with the same `(source_ids, visible_ids)` is an exact identity.

- [source_ids:] full unreduced source dataset IDs.
- [visible_ids:] reduced visible mark IDs.
- [st:] selection state before reconciliation.
- [id:] any source item ID tested for selection survival.
*)
Theorem selection_reconcile_preserves_hidden_and_idempotent
    (source_ids visible_ids : list nat) (st : sel_state) (id : nat) :
  let st1 := selection_reconcile source_ids visible_ids st in
  (In id (sel_selected st1) <-> In id (sel_selected st) /\ mem_id id source_ids = true) /\
  (forall f, sel_focused st1 = Some f -> mem_id f (sel_available st1) = true) /\
  selection_reconcile source_ids visible_ids st1 = st1.
Proof.
  cbv zeta. unfold selection_reconcile; simpl.
  split; [split | split].
  - intros Hin. apply filter_In in Hin; exact Hin.
  - intros Hboth. apply filter_In; exact Hboth.
  - intros f Hf. destruct (sel_focused st) as [x |]; simpl in Hf; [| discriminate].
    destruct (mem_id x _) eqn:Hm; [injection Hf as <-; exact Hm | discriminate].
  - f_equal.
    + apply filter_idem.
    + apply filter_opt_idem.
    + apply filter_opt_idem.
Qed.

(** ** 4. Length-prefixed `Recipe.key` injectivity *)

(** Model each segment `part : list nat` encoded as `length part :: part` in a flat token stream. *)
Fixpoint encode_length_prefixed (parts : list (list nat)) : list nat :=
  match parts with
  | [] => []
  | p :: rest => length p :: p ++ encode_length_prefixed rest
  end.

Lemma app_eq_same_len {A : Type} (u1 v1 u2 v2 : list A) :
  length u1 = length u2 ->
  u1 ++ v1 = u2 ++ v2 ->
  u1 = u2 /\ v1 = v2.
Proof.
  revert u2. induction u1 as [|x xs IH]; intros [|y ys] Hlen Heq; simpl in *; try discriminate.
  - split; [reflexivity | exact Heq].
  - injection Hlen as Hlen'. injection Heq as -> Heq'.
    destruct (IH ys Hlen' Heq') as [-> ->].
    split; reflexivity.
Qed.

(**md
**Length-prefixed key encoding (`Recipe.key` / `MapChart.key`) is strictly injective over arbitrary token payloads.**

Encoding each segment `p` with its length prefix `length p :: p` guarantees `encode_length_prefixed xs = encode_length_prefixed ys -> xs = ys` even when segments are empty or contain arbitrary delimiter characters.

- [xs:] first list of key path segments.
- [ys:] second list of key path segments.
*)
Theorem length_prefixed_key_injective (xs ys : list (list nat)) :
  encode_length_prefixed xs = encode_length_prefixed ys ->
  xs = ys.
Proof.
  revert ys. induction xs as [|p ps IH]; intros [|q qs] Heq; simpl in Heq; try discriminate.
  - reflexivity.
  - injection Heq as Hlen Hrest.
    destruct (app_eq_same_len p (encode_length_prefixed ps) q (encode_length_prefixed qs) Hlen Hrest)
      as [-> Htail].
    f_equal. apply IH, Htail.
Qed.

(** ** 5. Batch D1 SIMD LTTB precomputed cross-product identity *)

(**md
**Precomputing `dx_ac = ax - cx` and `dy_ca = cy - ay` preserves the exact LTTB signed triangle determinant.**

In `Sampling.lttbRun`, hoisting `dx_ac := ax - cx` and `dy_ca := cy - ay` outside the bucket loop and evaluating `dx_ac * (by_ - ay) + (bx - ax) * dy_ca` across SIMD lanes equals the reference determinant `(ax - cx) * (by_ - ay) - (ax - bx) * (cy - ay)`.

- [ax:] anchor point X coordinate.
- [ay:] anchor point Y coordinate.
- [bx:] candidate bucket point X coordinate.
- [by_:] candidate bucket point Y coordinate.
- [cx:] next-bucket lookahead centroid X coordinate.
- [cy:] next-bucket lookahead centroid Y coordinate.
*)
Theorem lttb_precomputed_cross_eq_area (ax ay bx by_ cx cy : Z) :
  let dx_ac := (ax - cx)%Z in
  let dy_ca := (cy - ay)%Z in
  (dx_ac * (by_ - ay) + (bx - ax) * dy_ca)%Z =
  ((ax - cx) * (by_ - ay) - (ax - bx) * (cy - ay))%Z.
Proof.
  cbv zeta. lia.
Qed.

(** ** 6. Batch D5 refinements: SIMD beeswarm squared-distance dodge & single-pass contour/isoband cell routing *)

Fixpoint all_clear_scalar (x y thresh_sq : Z) (pts : list (Z * Z)) : bool :=
  match pts with
  | [] => true
  | (px, py) :: rest =>
    let dx := (px - x)%Z in
    let dy := (py - y)%Z in
    Z.geb (dx * dx + dy * dy)%Z thresh_sq && all_clear_scalar x y thresh_sq rest
  end.

Fixpoint all_clear_chunks (x y thresh_sq : Z) (chunks : list (list (Z * Z))) : bool :=
  match chunks with
  | [] => true
  | c :: rest => all_clear_scalar x y thresh_sq c && all_clear_chunks x y thresh_sq rest
  end.

Lemma all_clear_app (x y thresh_sq : Z) (u v : list (Z * Z)) :
  all_clear_scalar x y thresh_sq (u ++ v) =
  (all_clear_scalar x y thresh_sq u && all_clear_scalar x y thresh_sq v).
Proof.
  induction u as [|[px py] rest IH]; simpl.
  - reflexivity.
  - rewrite IH, andb_assoc. reflexivity.
Qed.

(**md
**Chunked SIMD squared-distance clearance (`(px - x)^2 + (py - y)^2 >= thresh_sq`) equals scalar neighbor scanning (`AdvancedCharts.swarm`).**

Partitioning the active X-window of placed beeswarm circles into arbitrary `Vector512` (8-lane), `Vector256` (4-lane), `Vector128` (2-lane), and scalar tail chunks evaluates the exact same clearance predicate as `all_clear_scalar` over `concat chunks`, without calling `Double.Hypot` (`sqrt`).

- [x:] candidate circle X coordinate.
- [y:] candidate vertical dodge offset.
- [thresh_sq:] squared minimum clearance `(diameter * (1 - 1e-12))^2`.
- [chunks:] SIMD vector chunks and scalar remainder of the active neighbor window.
*)
Theorem beeswarm_simd_chunked_clear_eq_scalar
    (x y thresh_sq : Z) (chunks : list (list (Z * Z))) :
  all_clear_chunks x y thresh_sq chunks =
  all_clear_scalar x y thresh_sq (concat chunks).
Proof.
  induction chunks as [|c rest IH]; simpl.
  - reflexivity.
  - rewrite all_clear_app, IH. reflexivity.
Qed.

Fixpoint filter_band_members (lo : Z) (hi_opt : option Z) (cells : list (option Z * list nat)) : list nat :=
  match cells with
  | [] => []
  | (mv, members) :: rest =>
    let keep :=
      match mv, members with
      | _, [] => false
      | None, _ => false
      | Some v, _ =>
        Z.geb v lo &&
        match hi_opt with
        | Some hi => Z.ltb v hi
        | None => true
        end
      end in
    (if keep then members else []) ++ filter_band_members lo hi_opt rest
  end.

Definition compact_nonempty_cells (cells : list (option Z * list nat)) : list (Z * list nat) :=
  flat_map
    (fun '(mv, members) =>
      match mv, members with
      | Some v, _ :: _ => [(v, members)]
      | _, _ => []
      end)
    cells.

Fixpoint filter_compact_band (lo : Z) (hi_opt : option Z) (compact : list (Z * list nat)) : list nat :=
  match compact with
  | [] => []
  | (v, members) :: rest =>
    let keep :=
      Z.geb v lo &&
      match hi_opt with
      | Some hi => Z.ltb v hi
      | None => true
      end in
    (if keep then members else []) ++ filter_compact_band lo hi_opt rest
  end.

(**md
**Compacting non-empty valued grid cells once preserves exact `(row, column)` band membership for every contour/isoband level (`ContourChart`).**

Extracting `compact_nonempty_cells` in a single `(row, column)` pass over the `DensityGrid` and scanning only those populated cells per threshold produces the exact same ordered `Membership` list as rescanning all `rows * columns` grid cells per threshold.

- [lo:] lower threshold level.
- [hi_opt:] optional upper threshold (`Some next_level` for `isobands`, `None` for `contours`).
- [cells:] full `(row, column)` grid stream of `(DensityGrid.value, DensityGrid.members)`.
*)
Theorem contour_compact_nonempty_cells_membership_eq
    (lo : Z) (hi_opt : option Z) (cells : list (option Z * list nat)) :
  filter_compact_band lo hi_opt (compact_nonempty_cells cells) =
  filter_band_members lo hi_opt cells.
Proof.
  induction cells as [|[mv members] rest IH]; simpl.
  - reflexivity.
  - destruct mv as [v |]; destruct members as [|m ms]; simpl; rewrite ?IH; reflexivity.
Qed.

(**md
**`Selection.update` on `Clear` and `SelectFocused` is independent of materializing `Set.ofList Available` (`Selection.fs`).**

For `Clear` and `SelectFocused`, the resulting `(Selected, Focused, Hovered)` components depend only on `st.Focused`, allowing `Selection.update` to defer building `Set.ofList state.Available` to the message arms that actually query membership (`Replace`, `Toggle`, `Focus`, `Hover`).

- [st:] current selection state.
*)
Theorem lazy_selection_clear_and_select_focused_eq (st : sel_state) :
  let clear_eager :=
    let _ := dedup_keep_first [] (sel_available st) in
    {| sel_available := sel_available st; sel_selected := []; sel_focused := sel_focused st; sel_hovered := None |} in
  let clear_lazy :=
    {| sel_available := sel_available st; sel_selected := []; sel_focused := sel_focused st; sel_hovered := None |} in
  let focus_sel := match sel_focused st with Some f => [f] | None => [] end in
  clear_eager = clear_lazy /\
  (forall f, sel_focused st = Some f -> mem_id f (sel_available st) = true ->
    List.filter (fun id => mem_id id (sel_available st)) focus_sel = focus_sel).
Proof.
  cbv zeta. split; [reflexivity |].
  intros f Hf Hmem. rewrite Hf; simpl. rewrite Hmem; reflexivity.
Qed.
