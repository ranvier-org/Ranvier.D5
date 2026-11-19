From stdpp Require Import prelude.
From Stdlib Require Import Arith Lia List Bool ZArith.
Import ListNotations.

Local Arguments Nat.eqb : simpl never.
Open Scope Z_scope.

(**md # `PickEncoding`, `PickRegistry3D`, `KeyedSlotSync`, `HoverPickCoalescer`, `UpAxis`, and `H3` proportional floors

Models the constructive discrete invariants of `Ranvier.D5.Wgpu` GPU picking, layer allocation, slot compaction, hover coalescing, `UpAxis` coordinate mapping, and `Ranvier.D5.Graph3D` proportional unit-ball radius floors. *)

(** ** 1. `PickEncoding` 32-bit layer/slot bijection and layer disjointness (`Interaction3D`) *)

Definition pick_slot_stride : Z := 16777216.

Definition encode_pick (layer slot : Z) : Z :=
  layer * pick_slot_stride + (slot + 1).

Definition decode_layer (pid : Z) : Z :=
  pid / pick_slot_stride.

Definition decode_slot (pid : Z) : Z :=
  (pid mod pick_slot_stride) - 1.

(**md
**Every valid `(layer, slot)` pair encodes to a strictly positive `PickId`, distinguishing active hits from the `0` background sentinel.**

For `1 <= layer <= 255` and `0 <= slot < 16777214`, `encode_pick layer slot` is at least `16777217 > 0`, ensuring background clear value `0` never aliases any pickable GPU primitive.

- [l:] pick layer index in `[1 .. 255]`.
- [s:] dense GPU slot index in `[0 .. 16777213]`.
*)
Theorem pick_encoding_nonzero (l s : Z) :
  1 <= l <= 255 ->
  0 <= s < 16777214 ->
  encode_pick l s > 0.
Proof.
  intros Hl Hs. unfold encode_pick, pick_slot_stride. lia.
Qed.

(**md
**`decode_layer` and `decode_slot` exactly invert `encode_pick` over all valid `(layer, slot)` pairs.**

Because `0 <= slot + 1 < 16777216`, Euclidean division and modulo by `2^24 = 16777216` recover `layer` and `slot` without bit truncation or cross-field bleed.

- [l:] pick layer index in `[1 .. 255]`.
- [s:] dense GPU slot index in `[0 .. 16777213]`.
*)
Theorem pick_encoding_roundtrip (l s : Z) :
  1 <= l <= 255 ->
  0 <= s < 16777214 ->
  decode_layer (encode_pick l s) = l /\
  decode_slot (encode_pick l s) = s.
Proof.
  intros Hl Hs.
  unfold decode_layer, decode_slot, encode_pick, pick_slot_stride.
  assert (Hs1 : 0 <= s + 1 < 16777216) by lia.
  split.
  - rewrite (Z.add_comm (l * 16777216) (s + 1)).
    rewrite Z.div_add; [| lia].
    rewrite Z.div_small; lia.
  - rewrite (Z.add_comm (l * 16777216) (s + 1)).
    rewrite Z.mod_add; [| lia].
    rewrite Z.mod_small; lia.
Qed.

(**md
**`encode_pick` is strictly injective, so distinct pick layers and distinct slots never alias.**

Whenever `encode_pick l1 s1 = encode_pick l2 s2` for valid `(l1, s1)` and `(l2, s2)`, necessarily `l1 = l2` and `s1 = s2`.

- [l1:] first layer index in `[1 .. 255]`.
- [s1:] first slot index in `[0 .. 16777213]`.
- [l2:] second layer index in `[1 .. 255]`.
- [s2:] second slot index in `[0 .. 16777213]`.
*)
Theorem pick_encoding_injective (l1 s1 l2 s2 : Z) :
  1 <= l1 <= 255 ->
  0 <= s1 < 16777214 ->
  1 <= l2 <= 255 ->
  0 <= s2 < 16777214 ->
  encode_pick l1 s1 = encode_pick l2 s2 ->
  l1 = l2 /\ s1 = s2.
Proof.
  intros Hl1 Hs1 Hl2 Hs2 Heq.
  destruct (pick_encoding_roundtrip l1 s1 Hl1 Hs1) as [Hdl1 Hds1].
  destruct (pick_encoding_roundtrip l2 s2 Hl2 Hs2) as [Hdl2 Hds2].
  split.
  - rewrite <- Hdl1, <- Hdl2, Heq. reflexivity.
  - rewrite <- Hds1, <- Hds2, Heq. reflexivity.
Qed.

(** ** 2. `PickRegistry3D` layer allocator uniqueness, explicit reservation, and reuse *)

Definition registry := list bool.

Definition empty_registry : registry := repeat false 255%nat.

Definition is_used (reg : registry) (l : nat) : bool :=
  match l with
  | 0%nat => true
  | S idx =>
    match nth_error reg idx with
    | Some b => b
    | None => true
    end
  end.

Fixpoint set_nth_bool (idx : nat) (v : bool) (xs : list bool) : list bool :=
  match idx, xs with
  | _, [] => []
  | 0%nat, _ :: rest => v :: rest
  | S k, x :: rest => x :: set_nth_bool k v rest
  end.

Fixpoint alloc_from (layer : nat) (xs : list bool) : option (nat * registry) :=
  match xs with
  | [] => None
  | false :: rest => Some (layer, true :: rest)
  | true :: rest =>
    match alloc_from (S layer) rest with
    | None => None
    | Some (l, rest') => Some (l, true :: rest')
    end
  end.

Definition alloc_layer (reg : registry) : option (nat * registry) :=
  alloc_from 1%nat reg.

Definition release_layer (l : nat) (reg : registry) : registry :=
  match l with
  | 0%nat => reg
  | S idx => set_nth_bool idx false reg
  end.

Definition reserve_explicit_layer (l : nat) (reg : registry) : option (nat * registry) :=
  match l with
  | 0%nat => None
  | S idx =>
    match nth_error reg idx with
    | Some false => Some (l, set_nth_bool idx true reg)
    | _ => None
    end
  end.

(**md
**Updating one boolean slot via `set_nth_bool` preserves list length.**

Structural induction confirms `length (set_nth_bool idx v xs) = length xs`.

- [idx:] zero-based index to update.
- [v:] replacement boolean value.
- [xs:] registry occupancy list.
*)
Lemma set_nth_bool_length (idx : nat) (v : bool) (xs : list bool) :
  length (set_nth_bool idx v xs) = length xs.
Proof.
  revert idx. induction xs as [|x rest IH]; intros [|k]; simpl; auto.
Qed.

(**md
**Reading at the updated index after `set_nth_bool` yields the newly written value when in bounds.**

For `idx < length xs`, `nth_error (set_nth_bool idx v xs) idx = Some v`.

- [idx:] zero-based index updated.
- [v:] replacement boolean value.
- [xs:] registry occupancy list.
*)
Lemma nth_error_set_nth_bool_eq (idx : nat) (v : bool) (xs : list bool) :
  (idx < length xs)%nat ->
  nth_error (set_nth_bool idx v xs) idx = Some v.
Proof.
  revert idx. induction xs as [|x rest IH]; intros [|k] Hlt; simpl in *; try lia.
  - reflexivity.
  - apply IH. lia.
Qed.

(**md
**Updating index `idx` via `set_nth_bool` leaves every other index `j <> idx` unchanged.**

For all `j <> idx`, `nth_error (set_nth_bool idx v xs) j = nth_error xs j`.

- [idx:] zero-based index updated.
- [j:] zero-based index queried.
- [v:] replacement boolean value.
- [xs:] registry occupancy list.
*)
Lemma nth_error_set_nth_bool_ne (idx j : nat) (v : bool) (xs : list bool) :
  idx <> j ->
  nth_error (set_nth_bool idx v xs) j = nth_error xs j.
Proof.
  revert idx j. induction xs as [|x rest IH]; intros [|k] [|m] Hne; simpl in *; try reflexivity.
  - contradiction.
  - apply IH. lia.
Qed.

(**md
**`alloc_from` picks an unused layer within `[base, base + length xs)`, marks only that slot `true`, and preserves all other slots.**

Inductive specification of `alloc_from base xs = Some (l, xs')` used to prove both freshness and two-allocation distinctness.

- [xs:] remaining occupancy flags.
- [base:] 1-based layer number of the head of `xs`.
- [l:] allocated layer number.
- [xs':] updated occupancy flags.
*)
Lemma alloc_from_spec (xs : list bool) (base l : nat) (xs' : list bool) :
  alloc_from base xs = Some (l, xs') ->
  (base <= l < base + length xs)%nat /\
  length xs' = length xs /\
  nth_error xs (l - base)%nat = Some false /\
  nth_error xs' (l - base)%nat = Some true /\
  (forall j, j <> (l - base)%nat -> nth_error xs' j = nth_error xs j).
Proof.
  revert base l xs'.
  induction xs as [|[|] rest IH]; intros base l xs' Halloc; simpl in Halloc.
  - discriminate.
  - destruct (alloc_from (S base) rest) as [[l0 rest']|] eqn:Hsub; [| discriminate].
    injection Halloc as <- <-.
    destruct (IH (S base) l0 rest' Hsub) as [Hrange [Hlen [Hold [Hnew Hoth]]]].
    assert (Hdiff : (l0 - base = S (l0 - S base))%nat) by lia.
    repeat split; simpl; try lia.
    + rewrite Hdiff; exact Hold.
    + rewrite Hdiff; exact Hnew.
    + intros [|j'] Hne; simpl; [reflexivity | apply Hoth; lia].
  - injection Halloc as <- <-.
    assert (Hzero : (base - base = 0)%nat) by lia.
    repeat split; simpl; try lia.
    + rewrite Hzero; reflexivity.
    + rewrite Hzero; reflexivity.
    + intros [|j'] Hne; simpl; [exfalso; apply Hne; lia | reflexivity].
Qed.

(**md
**`alloc_layer` always returns a fresh layer `l in [1 .. 255]`, marks `l` used, and leaves every previously allocated layer used.**

Whenever `alloc_layer reg = Some (l, reg')` on a 255-layer registry, `1 <= l <= 255`, `is_used reg l = false`, `is_used reg' l = true`, and every previously used layer remains `true`.

- [reg:] 255-slot pick-layer occupancy registry prior to allocation.
- [l:] freshly allocated layer ID.
- [reg':] updated occupancy registry after allocation.
*)
Theorem pick_registry_alloc_fresh (reg : registry) (l : nat) (reg' : registry) :
  length reg = 255%nat ->
  alloc_layer reg = Some (l, reg') ->
  (1 <= l <= 255)%nat /\
  length reg' = 255%nat /\
  is_used reg l = false /\
  is_used reg' l = true /\
  (forall l', is_used reg l' = true -> is_used reg' l' = true).
Proof.
  intros Hlen Halloc.
  destruct (alloc_from_spec reg 1%nat l reg' Halloc) as [Hrange [Hlen' [Hold [Hnew Hoth]]]].
  destruct l as [|idx]; [lia |].
  assert (Hidx : (S idx - 1 = idx)%nat) by lia.
  rewrite Hidx in Hold, Hnew.
  repeat split; try lia.
  - unfold is_used. rewrite Hold. reflexivity.
  - unfold is_used. rewrite Hnew. reflexivity.
  - intros [|idx'] Hused'; simpl in *; [reflexivity |].
    destruct (Nat.eq_dec idx' idx) as [Heq | Hne].
    + subst idx'. rewrite Hnew. reflexivity.
    + rewrite Hoth; [exact Hused' | lia].
Qed.

(**md
**Two consecutive `alloc_layer` calls without an intervening release always return strictly distinct layer IDs (`l1 <> l2`).**

Because the first allocation marks `l1` as used while the second allocation only selects an unused layer, `l1` and `l2` cannot collide.

- [reg:] initial 255-slot registry.
- [l1:] layer returned by the first `alloc_layer`.
- [reg1:] registry after the first allocation.
- [l2:] layer returned by the second `alloc_layer`.
- [reg2:] registry after the second allocation.
*)
Theorem pick_registry_two_allocs_distinct
    (reg : registry) (l1 : nat) (reg1 : registry) (l2 : nat) (reg2 : registry) :
  length reg = 255%nat ->
  alloc_layer reg = Some (l1, reg1) ->
  alloc_layer reg1 = Some (l2, reg2) ->
  l1 <> l2.
Proof.
  intros Hlen H1 H2.
  destruct (pick_registry_alloc_fresh reg l1 reg1 Hlen H1) as [_ [Hlen1 [_ [Hused1 _]]]].
  destruct (pick_registry_alloc_fresh reg1 l2 reg2 Hlen1 H2) as [_ [_ [Hfree2 _]]].
  intro Heq. subst l2. rewrite Hused1 in Hfree2. discriminate.
Qed.

(**md
**`release_layer l` frees layer `l` back to `false` without disturbing any other layer `l' <> l` or changing registry capacity.**

Releasing a valid layer `l in [1 .. 255]` yields a 255-slot registry with `is_used reg' l = false` and `is_used reg' l' = is_used reg l'` for all `l' <> l`.

- [reg:] 255-slot registry before release.
- [l:] layer ID in `[1 .. 255]` to release.
*)
Theorem pick_registry_release_reusable (reg : registry) (l : nat) :
  (1 <= l <= 255)%nat ->
  length reg = 255%nat ->
  let reg' := release_layer l reg in
  length reg' = 255%nat /\
  is_used reg' l = false /\
  (forall l', l' <> l -> is_used reg' l' = is_used reg l').
Proof.
  cbv zeta. intros Hl Hlen.
  destruct l as [|idx]; [lia |].
  unfold release_layer, is_used.
  repeat split.
  - rewrite set_nth_bool_length. exact Hlen.
  - rewrite nth_error_set_nth_bool_eq; [reflexivity | lia].
  - intros [|idx'] Hne; [reflexivity |].
    rewrite nth_error_set_nth_bool_ne; [reflexivity | lia].
Qed.

(**md
**`reserve_explicit_layer` succeeds iff `l` is valid and free, marking only `l` used.**

Supports explicit caller-chosen layer IDs in `PickRegistry3D` without breaking uniqueness against automatic allocations.

- [reg:] 255-slot registry before reservation.
- [l:] requested explicit layer ID.
- [l':] returned layer ID equal to `l`.
- [reg':] updated registry.
*)
Theorem pick_registry_reserve_explicit_fresh
    (reg : registry) (l l' : nat) (reg' : registry) :
  length reg = 255%nat ->
  reserve_explicit_layer l reg = Some (l', reg') ->
  l' = l /\
  (1 <= l <= 255)%nat /\
  length reg' = 255%nat /\
  is_used reg l = false /\
  is_used reg' l = true /\
  (forall l2, l2 <> l -> is_used reg' l2 = is_used reg l2).
Proof.
  intros Hlen Hres.
  destruct l as [|idx]; simpl in Hres; [discriminate |].
  destruct (nth_error reg idx) as [[|]|] eqn:Hnth; try discriminate.
  injection Hres as <- <-.
  assert (Hbound : (idx < length reg)%nat) by (apply nth_error_Some; rewrite Hnth; discriminate).
  unfold is_used. repeat split; try lia.
  - rewrite set_nth_bool_length; exact Hlen.
  - rewrite Hnth; reflexivity.
  - rewrite nth_error_set_nth_bool_eq; [reflexivity | exact Hbound].
  - intros [|idx2] Hne; [reflexivity |].
    rewrite nth_error_set_nth_bool_ne; [reflexivity | lia].
Qed.

(** ** 3. `KeyedSlotSync` tail-swap compaction bijection and immediate retirement (`Interaction3D`) *)

Fixpoint replace_nth {K : Type} (idx : nat) (v : K) (xs : list K) : list K :=
  match idx, xs with
  | _, [] => []
  | 0%nat, _ :: rest => v :: rest
  | S i, x :: rest => x :: replace_nth i v rest
  end.

Definition remove_swap {K : Type} (idx : nat) (slots : list K) : list K :=
  let n := length slots in
  if Nat.leb n 1 then
    if Nat.eqb idx 0%nat then [] else slots
  else if Nat.eqb idx (n - 1)%nat then
    removelast slots
  else
    match nth_error slots (n - 1)%nat with
    | None => slots
    | Some last_k => removelast (replace_nth idx last_k slots)
    end.

(**md
**`nth_error` at the last valid index `length xs` of `xs ++ [a]` yields `Some a`.**

Helper locating the tail key before swapping it into the vacated slot.

- [xs:] prefix list.
- [a:] tail element.
*)
Lemma nth_error_app_last {K : Type} (xs : list K) (a : K) :
  nth_error (xs ++ [a]) (length xs) = Some a.
Proof.
  rewrite nth_error_app2; [| lia].
  replace (length xs - length xs)%nat with 0%nat by lia.
  reflexivity.
Qed.

(**md
**`removelast` of `xs ++ [a]` drops exactly `[a]`, returning `xs`.**

Helper justifying tail retirement after copying `last slots` into `slots[idx]`.

- [xs:] prefix list.
- [a:] tail element removed.
*)
Lemma removelast_app_single {K : Type} (xs : list K) (a : K) :
  removelast (xs ++ [a]) = xs.
Proof.
  induction xs as [|x rest IH]; simpl.
  - reflexivity.
  - destruct rest as [|y ys]; simpl; [reflexivity | f_equal; exact IH].
Qed.

(**md
**`replace_nth` at index `length front` of `front ++ old :: tail` substitutes `v` for `old`.**

Directly aligns dense slot overwrite `slots[idx] <- last_k` with list concatenation decomposition.

- [front:] prefix before slot `idx`.
- [old:] retired key previously at `idx`.
- [v:] replacement key moved from tail.
- [tail:] remaining slice after `idx`.
*)
Lemma replace_nth_app_mid {K : Type} (front : list K) (old v : K) (tail : list K) :
  replace_nth (length front) v (front ++ old :: tail) = front ++ v :: tail.
Proof.
  induction front as [|x rest IH]; simpl.
  - reflexivity.
  - rewrite IH. reflexivity.
Qed.

(**md
**A `NoDup` list split around element `y` satisfies `NoDup` on both halves, excludes `y` from both, and keeps the halves disjoint.**

Both directions hold for `NoDup (xs ++ y :: ys)`, enabling single-step `NoDup` transfer under tail-swap compaction.

- [xs:] left prefix.
- [y:] middle element.
- [ys:] right suffix.
*)
Lemma NoDup_split_mid {K : Type} (xs : list K) (y : K) (ys : list K) :
  NoDup (xs ++ y :: ys) <->
  NoDup xs /\ NoDup ys /\ ~ In y xs /\ ~ In y ys /\ (forall z, In z xs -> ~ In z ys).
Proof.
  induction xs as [|x rest IH]; simpl.
  - split.
    + intros Hnd. inversion Hnd; subst.
      repeat split; [constructor | assumption | intros [] | assumption | intros z []].
    + intros [_ [Hys [_ [Hny _]]]]. constructor; assumption.
  - split.
    + intros Hnd. inversion Hnd as [|? ? Hnin Hrest]; subst.
      destruct (proj1 IH Hrest) as [Hnd_r [Hnd_y [Hyr [Hyys Hdisj]]]].
      rewrite in_app_iff in Hnin; simpl in Hnin.
      repeat split.
      * constructor; [intro Hin; apply Hnin; left; exact Hin | exact Hnd_r].
      * exact Hnd_y.
      * intros [-> | Hin]; [apply Hnin; right; left; reflexivity | apply Hyr; exact Hin].
      * exact Hyys.
      * intros z [-> | Hin] Hzy; [apply Hnin; right; right; exact Hzy | apply (Hdisj z Hin Hzy)].
    + intros [Hnd_x [Hnd_y [Hyx [Hyys Hdisj]]]].
      inversion Hnd_x as [|? ? Hnx Hnd_r]; subst.
      constructor.
      * rewrite in_app_iff; simpl. intros [Hin | [-> | Hin]].
        -- apply Hnx; exact Hin.
        -- apply Hyx; left; reflexivity.
        -- apply (Hdisj x (or_introl eq_refl) Hin).
      * apply (proj2 IH). repeat split.
        -- exact Hnd_r.
        -- exact Hnd_y.
        -- intro Hin; apply Hyx; right; exact Hin.
        -- exact Hyys.
        -- intros z Hin; apply Hdisj; right; exact Hin.
Qed.

(**md
**Tail-swap compaction in `KeyedSlotSync` (`remove_swap`) shrinks length by 1, preserves `NoDup`, immediately retires `k`, and preserves membership of every `k' <> k`.**

For any `NoDup slots` and key `k` at `idx` (`nth_error slots idx = Some k`), `remove_swap idx slots` has length `length slots - 1`, satisfies `NoDup`, excludes `k`, and satisfies `In k' slots <-> In k' slots'` for every `k' <> k`.

- [K:] key type.
- [idx:] dense slot index to remove.
- [k:] retired key stored at `slots[idx]`.
- [slots:] dense active key array satisfying `NoDup`.
*)
Theorem keyed_slot_tail_swap_preserves_nodup_and_membership
    {K : Type} (idx : nat) (k : K) (slots : list K) :
  NoDup slots ->
  nth_error slots idx = Some k ->
  let slots' := remove_swap idx slots in
  length slots' = (length slots - 1)%nat /\
  NoDup slots' /\
  ~ In k slots' /\
  (forall k', k' <> k -> In k' slots <-> In k' slots').
Proof.
  cbv zeta. intros Hnd Hnth.
  destruct (nth_error_split slots idx Hnth) as [front [tail [Hsplit Hflen]]].
  subst slots.
  destruct tail as [|t0 trest].
  - (* idx = length slots - 1: remove last element *)
    destruct front as [|f0 frest].
    + simpl in Hflen. subst idx.
      unfold remove_swap; simpl.
      split; [reflexivity |].
      split; [constructor |].
      split; [intros [] |].
      intros k' Hne; simpl; split; [intros [-> | []]; contradiction | intros []].
    + unfold remove_swap.
      simpl in Hflen.
      assert (Hlen2 : (2 <= length ((f0 :: frest) ++ [k]))%nat)
        by (rewrite length_app; simpl; lia).
      assert (Hleb : Nat.leb (length ((f0 :: frest) ++ [k])) 1 = false)
        by (apply Nat.leb_gt; lia).
      assert (Hlast_idx : (length ((f0 :: frest) ++ [k]) - 1)%nat = idx)
        by (rewrite length_app; simpl; lia).
      rewrite Hleb, Hlast_idx, Nat.eqb_refl.
      rewrite removelast_app_single.
      apply NoDup_split_mid in Hnd.
      destruct Hnd as [Hnd_f [_ [Hkf _]]].
      split; [simpl; exact Hflen |].
      split; [exact Hnd_f |].
      split; [exact Hkf |].
      intros k' Hne. rewrite in_app_iff; simpl.
      split; [intros [Hin | [-> | []]]; [exact Hin | contradiction] | intro Hin; left; exact Hin].
  - (* idx < length slots - 1: swap last_k into idx and drop tail *)
    assert (Htail_ne : t0 :: trest <> []) by discriminate.
    destruct (exists_last Htail_ne) as [mid [last_k Htail_eq]].
    rewrite Htail_eq.
    unfold remove_swap.
    assert (Hlen_eq : length (front ++ k :: mid ++ [last_k]) = (length front + length mid + 2)%nat)
      by (rewrite !length_app; simpl; rewrite length_app; simpl; lia).
    assert (Hleb : Nat.leb (length (front ++ k :: mid ++ [last_k])) 1 = false)
      by (apply Nat.leb_gt; lia).
    assert (Hneq : Nat.eqb idx (length (front ++ k :: mid ++ [last_k]) - 1)%nat = false)
      by (apply Nat.eqb_neq; lia).
    rewrite Hleb, Hneq.
    assert (Hlast_nth : nth_error (front ++ k :: mid ++ [last_k]) (length (front ++ k :: mid ++ [last_k]) - 1)%nat = Some last_k).
    { replace (front ++ k :: mid ++ [last_k]) with ((front ++ k :: mid) ++ [last_k])
        by (rewrite <- app_assoc; reflexivity).
      replace (length ((front ++ k :: mid) ++ [last_k]) - 1)%nat with (length (front ++ k :: mid))
        by (rewrite (length_app (front ++ k :: mid) [last_k]); simpl; lia).
      apply nth_error_app_last. }
    rewrite Hlast_nth.
    rewrite <- Hflen, replace_nth_app_mid.
    replace (front ++ last_k :: mid ++ [last_k]) with ((front ++ last_k :: mid) ++ [last_k])
      by (rewrite <- app_assoc; reflexivity).
    rewrite removelast_app_single.
    rewrite Htail_eq in Hnd.
    apply NoDup_split_mid in Hnd.
    destruct Hnd as [Hnd_f [Hnd_m1 [Hkf [Hk_m1 Hdisj_f_m1]]]].
    assert (Hnd_m1' : NoDup (mid ++ [last_k])) by exact Hnd_m1.
    apply NoDup_split_mid in Hnd_m1'.
    destruct Hnd_m1' as [Hnd_mid [_ [Hlast_mid _]]].
    assert (Hlast_f : ~ In last_k front).
    { intro Hin. apply (Hdisj_f_m1 last_k Hin). rewrite in_app_iff; right; left; reflexivity. }
    assert (Hk_mid : ~ In k mid).
    { intro Hin. apply Hk_m1. rewrite in_app_iff; left; exact Hin. }
    assert (Hk_last : k <> last_k).
    { intro Heq. subst last_k. apply Hk_m1. rewrite in_app_iff; right; left; reflexivity. }
    split; [rewrite Hlen_eq, length_app; simpl; lia |].
    split; [apply NoDup_split_mid; split; [exact Hnd_f | split; [exact Hnd_mid | split; [exact Hlast_f | split; [exact Hlast_mid | intros z Hzf Hzm; apply (Hdisj_f_m1 z Hzf); rewrite in_app_iff; left; exact Hzm]]]] |].
    split; [rewrite in_app_iff; simpl; intros [Hin | [Heq | Hin]]; [apply Hkf; exact Hin | apply Hk_last; symmetry; exact Heq | apply Hk_mid; exact Hin] |].
    intros k' Hne.
    rewrite !in_app_iff; simpl.
    split.
    * intros [Hin | [-> | Hin]].
      -- left; exact Hin.
      -- contradiction.
      -- rewrite in_app_iff in Hin; simpl in Hin.
         destruct Hin as [Hin | [-> | []]]; [right; right; exact Hin | right; left; reflexivity].
    * intros [Hin | [-> | Hin]].
      -- left; exact Hin.
      -- right; right; rewrite in_app_iff; right; left; reflexivity.
      -- right; right; rewrite in_app_iff; left; exact Hin.
Qed.

(** ** 4. `HoverPickCoalescer` single-flight coalescing, epoch fencing, and latest-cursor liveness *)

Record hover_coalescer := {
  hc_inflight : option nat;
  hc_pending : option (Z * Z);
  hc_settled : option (Z * Z * nat);
  hc_current_epoch : nat
}.

Definition on_pointer_move (x y : Z) (st : hover_coalescer)
    : hover_coalescer * option (Z * Z * nat) :=
  match hc_inflight st with
  | None =>
    ({| hc_inflight := Some (hc_current_epoch st);
        hc_pending := None;
        hc_settled := hc_settled st;
        hc_current_epoch := hc_current_epoch st |},
     Some (x, y, hc_current_epoch st))
  | Some _ =>
    ({| hc_inflight := hc_inflight st;
        hc_pending := Some (x, y);
        hc_settled := hc_settled st;
        hc_current_epoch := hc_current_epoch st |},
     None)
  end.

Definition on_scene_switch (st : hover_coalescer) : hover_coalescer :=
  {| hc_inflight := hc_inflight st;
     hc_pending := None;
     hc_settled := hc_settled st;
     hc_current_epoch := S (hc_current_epoch st) |}.

Definition on_pick_complete (resp_epoch : nat) (hit_xy : Z * Z) (st : hover_coalescer)
    : hover_coalescer * option (Z * Z * nat) :=
  let settled' :=
    if Nat.eqb resp_epoch (hc_current_epoch st)
    then Some (fst hit_xy, snd hit_xy, resp_epoch)
    else hc_settled st in
  match hc_pending st with
  | Some (px, py) =>
    ({| hc_inflight := Some (hc_current_epoch st);
        hc_pending := None;
        hc_settled := settled';
        hc_current_epoch := hc_current_epoch st |},
     Some (px, py, hc_current_epoch st))
  | None =>
    ({| hc_inflight := None;
        hc_pending := None;
        hc_settled := settled';
        hc_current_epoch := hc_current_epoch st |},
     None)
  end.

Fixpoint apply_pointer_moves (moves : list (Z * Z)) (st : hover_coalescer)
    : hover_coalescer * list (Z * Z * nat) :=
  match moves with
  | [] => (st, [])
  | (x, y) :: rest =>
    let '(st1, cmd1) := on_pointer_move x y st in
    let '(st2, cmds2) := apply_pointer_moves rest st1 in
    let cmds' := match cmd1 with Some c => c :: cmds2 | None => cmds2 end in
    (st2, cmds')
  end.

(**md
**Stale GPU readback completions (`resp_epoch < current_epoch`) never overwrite `hc_settled`.**

Even when a GPU pick from an earlier scene finishes after `on_scene_switch`, the epoch fence preserves the current `hc_settled` state unchanged.

- [resp_epoch:] epoch captured when the in-flight GPU pick was dispatched.
- [hit_xy:] decoded cursor/hit coordinates returned by the GPU readback.
- [st:] current `hover_coalescer` state.
*)
Theorem hover_coalescer_stale_epoch_rejected
    (resp_epoch : nat) (hit_xy : Z * Z) (st : hover_coalescer) :
  (resp_epoch < hc_current_epoch st)%nat ->
  hc_settled (fst (on_pick_complete resp_epoch hit_xy st)) = hc_settled st.
Proof.
  intros Hlt. unfold on_pick_complete.
  assert (Hneq : Nat.eqb resp_epoch (hc_current_epoch st) = false)
    by (apply Nat.eqb_neq; lia).
  rewrite Hneq. destruct (hc_pending st) as [[px py]|]; reflexivity.
Qed.

(**md
**Coalescing `N >= 1` pointer moves while a pick is in flight launches zero concurrent GPU picks and records the last coordinate `(xn, yn)`.**

Inductive helper showing that every move during `hc_inflight = Some ep` keeps single-flight intact and overwrites `hc_pending` with the final move `(xn, yn)`.

- [moves:] non-empty sequence of pointer moves ending at `(xn, yn)`.
- [st:] coalescer state with an active in-flight pick.
- [ep:] active dispatch epoch of the in-flight pick.
- [xn:] final X coordinate in `moves`.
- [yn:] final Y coordinate in `moves`.
*)
Lemma apply_pointer_moves_while_inflight
    (moves : list (Z * Z)) (st : hover_coalescer) (ep : nat) (xn yn : Z) :
  hc_inflight st = Some ep ->
  moves <> [] ->
  last moves (0, 0) = (xn, yn) ->
  let '(st', cmds) := apply_pointer_moves moves st in
  cmds = [] /\
  hc_inflight st' = Some ep /\
  hc_pending st' = Some (xn, yn) /\
  hc_current_epoch st' = hc_current_epoch st /\
  hc_settled st' = hc_settled st.
Proof.
  revert st.
  induction moves as [|[x y] rest IH]; intros st Hinf Hne Hlast; [contradiction |].
  simpl in *.
  unfold on_pointer_move. rewrite Hinf.
  destruct rest as [|m2 mrest].
  - simpl in Hlast. inversion Hlast; subst.
    simpl. repeat split; reflexivity.
  - assert (Hne' : m2 :: mrest <> []) by discriminate.
    specialize (IH
      {| hc_inflight := Some ep;
         hc_pending := Some (x, y);
         hc_settled := hc_settled st;
         hc_current_epoch := hc_current_epoch st |}
      eq_refl Hne' Hlast).
    destruct (apply_pointer_moves (m2 :: mrest) _) as [st' cmds'] eqn:Happ.
    exact IH.
Qed.

(**md
**Completing an in-flight pick after `N >= 1` coalesced moves dispatches one follow-up pick carrying the exact last coordinates `(xn, yn)`.**

While `hc_inflight = Some ep`, any non-empty sequence of pointer moves emits zero concurrent GPU picks; upon completion without a scene switch, `on_pick_complete` immediately launches a single follow-up pick at `(xn, yn)` in `hc_current_epoch st`.

- [prefix:] zero or more earlier pointer moves overwritten during coalescing.
- [xn:] final pointer X coordinate.
- [yn:] final pointer Y coordinate.
- [resp_epoch:] epoch of the completing in-flight GPU pick.
- [hit_xy:] readback result of the completing pick.
- [st:] coalescer state with `hc_inflight = Some ep`.
- [ep:] epoch of the active in-flight pick.
*)
Theorem hover_coalescer_latest_move_dispatched
    (prefix : list (Z * Z)) (xn yn : Z) (resp_epoch : nat) (hit_xy : Z * Z)
    (st : hover_coalescer) (ep : nat) :
  hc_inflight st = Some ep ->
  let moves := prefix ++ [(xn, yn)] in
  let '(st_after_moves, extra_cmds) := apply_pointer_moves moves st in
  let '(st_next, followup) := on_pick_complete resp_epoch hit_xy st_after_moves in
  extra_cmds = [] /\
  followup = Some (xn, yn, hc_current_epoch st) /\
  hc_inflight st_next = Some (hc_current_epoch st) /\
  hc_pending st_next = None.
Proof.
  cbv zeta. intros Hinf.
  assert (Hne : prefix ++ [(xn, yn)] <> [])
    by (destruct prefix; discriminate).
  assert (Hlast : last (prefix ++ [(xn, yn)]) (0, 0) = (xn, yn)).
  { clear Hne. induction prefix as [|p rest IH]; simpl; [reflexivity |].
    destruct (rest ++ [(xn, yn)]) eqn:Heq; [destruct rest; simpl in Heq; discriminate | exact IH]. }
  pose proof (apply_pointer_moves_while_inflight (prefix ++ [(xn, yn)]) st ep xn yn Hinf Hne Hlast) as Hmoves.
  destruct (apply_pointer_moves (prefix ++ [(xn, yn)]) st) as [st1 cmds1].
  destruct Hmoves as [Hcmds [Hinf1 [Hpend1 [Hep1 _]]]].
  unfold on_pick_complete. rewrite Hpend1, Hep1.
  repeat split; assumption.
Qed.

(** ** 5. `UpAxis` right-handed 3D coordinate transform involution (`Scale3D`) *)

Definition map_zup (p : Z * Z * Z) : Z * Z * Z :=
  let '(x, y, z) := p in (x, z, -y).

Definition inv_zup (w : Z * Z * Z) : Z * Z * Z :=
  let '(wx, wy, wz) := w in (wx, -wz, wy).

Definition map_yup (p : Z * Z * Z) : Z * Z * Z := p.

Definition inv_yup (w : Z * Z * Z) : Z * Z * Z := w.

(**md
**`UpAxis` coordinate mapping and inversion (`ZUp` `(x, z, -y)` / `(wx, -wz, wy)` and `YUp` identity) are exact mutual inverses.**

Mapping any integer 3D data triple `p` into right-handed Y-up GPU world space and inverting back (and conversely on world triple `w`) yields the exact input triple for both `UpAxis.ZUp` and `UpAxis.YUp`.

- [p:] data-space coordinate triple `(x, y, z)`.
- [w:] world-space coordinate triple `(wx, wy, wz)`.
*)
Theorem up_axis_map_invert_id (p w : Z * Z * Z) :
  inv_zup (map_zup p) = p /\
  map_zup (inv_zup w) = w /\
  inv_yup (map_yup p) = p /\
  map_yup (inv_yup w) = w.
Proof.
  destruct p as [[x y] z]. destruct w as [[wx wy] wz].
  simpl. split; [repeat f_equal; lia | split; [repeat f_equal; lia | split; reflexivity]].
Qed.

(** ** 6. `H3` / `Walrus` / `PoincareBallLift` proportional milli-unit floor scaling (`Z`) *)

Definition milli_floor (b_milli : Z) : Z :=
  (b_milli * 4) / 1000.

Definition milli_box_floor (b_milli : Z) : Z :=
  (b_milli * 8) / 1000.

(**md
**Proportional milli-unit radius floor reproduces `1.200` (`1200`) at `BallRadius = 300.0` and scales to `0.004` (`4`) at `BallRadius = 1.0`.**

In `H3`, `Walrus`, and `PoincareBallLift`, scaling the `1.2` radius floor (`2.4` box floor) by `BallRadius / 300.0` preserves exact legacy `300.0` pixel coordinates while shrinking unit-ball floors by `300x`.
*)
Theorem h3_milli_floor_at_300_exact :
  milli_floor 300000 = 1200 /\
  milli_floor 1000 = 4 /\
  milli_box_floor 300000 = 2400 /\
  milli_box_floor 1000 = 8.
Proof.
  vm_compute. split; [reflexivity | split; [reflexivity | split; reflexivity]].
Qed.

(**md
**For every positive ball radius `b > 0`, the proportional radius floor `milli_floor b` is nonnegative and strictly smaller than `b`.**

Because `0 <= (b * 4) / 1000 < b` for all `b > 0` (and likewise `(b * 8) / 1000 < b`), node floors can never exceed the enclosing hyperbolic sphere radius on unit balls (`b = 1.0`) or arbitrary positive scales.

- [b:] positive ball radius expressed in milli-units (`b > 0`).
*)
Theorem h3_milli_floor_strictly_inside_ball (b : Z) :
  0 < b ->
  0 <= milli_floor b /\ milli_floor b < b /\
  0 <= milli_box_floor b /\ milli_box_floor b < b.
Proof.
  intros Hb. unfold milli_floor, milli_box_floor.
  repeat split.
  - apply Z.div_pos; lia.
  - apply Z.div_lt_upper_bound; lia.
  - apply Z.div_pos; lia.
  - apply Z.div_lt_upper_bound; lia.
Qed.
