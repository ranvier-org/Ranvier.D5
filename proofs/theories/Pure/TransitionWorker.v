From stdpp Require Import prelude.
From Stdlib Require Import Arith Lia List Bool ZArith.
Import ListNotations.

Local Arguments Nat.eqb : simpl never.
Open Scope Z_scope.

(**md # `Transition` $C^0$ retargeting, `Gesture` capture/lasso invariants, `<svg>` ancestor isolation, and `ReactiveForce` worker revision fencing

Models the state machines in `Ranvier.D5.Transition`, `Ranvier.D5.Gesture`, `Ranvier.D5.View.InputAdapter`, `Ranvier.D5.View.ReactiveLayout`, and `Ranvier.D5.View.ReactiveForce`. *)

(** ** 1. `Transition` $C^0$ retargeting continuity *)

Record lin_segment := {
  seg_from : Z;
  seg_target : Z;
  seg_started : Z;
  seg_duration : Z
}.

(** Scaled sample (`denom = seg_duration` when `0 < seg_duration`): exact rational value is
    `seg_from + ((now - seg_started) * (seg_target - seg_from)) / seg_duration`. *)
Definition sample_step (now : Z) (s : lin_segment) : Z :=
  if Z.leb (seg_duration s) 0 || Z.geb (now - seg_started s) (seg_duration s) then seg_target s
  else if Z.leb now (seg_started s) then seg_from s
  else seg_from s + ((now - seg_started s) * (seg_target s - seg_from s)) / seg_duration s.

Definition retarget_step (now next_target next_dur : Z) (s : lin_segment) : lin_segment :=
  {| seg_from := sample_step now s;
     seg_target := next_target;
     seg_started := now;
     seg_duration := next_dur |}.

(**md
**Retargeting an active `Transition` at `now` is $C^0$-continuous at `now` and settles to `next_target` after `next_dur`.**

For positive `next_dur`, `sample_step now (retarget_step now next_target next_dur s) = sample_step now s` (zero visual jump on retargeting), while at `t >= now + next_dur` (or immediately when `next_dur <= 0`) it evaluates to `next_target`.

- [now:] current clock timestamp at which retargeting occurs.
- [next_target:] new target value.
- [next_dur:] new segment duration.
- [t_end:] any timestamp at or after `now + Z.max 0 next_dur`.
- [s:] active transition segment prior to retargeting.
*)
Theorem transition_retarget_c0_continuous
    (now next_target next_dur t_end : Z) (s : lin_segment) :
  now + Z.max 0 next_dur <= t_end ->
  let s' := retarget_step now next_target next_dur s in
  (0 < next_dur -> sample_step now s' = sample_step now s) /\
  sample_step t_end s' = next_target.
Proof.
  intros Hend. cbv zeta. unfold retarget_step, sample_step; simpl.
  split.
  - intros Hpos.
    assert (Hd : Z.leb next_dur 0 = false) by (apply Z.leb_gt; lia).
    assert (Hg : Z.geb (now - now) next_dur = false) by (rewrite Z.geb_leb; apply Z.leb_gt; lia).
    assert (Hl : Z.leb now now = true) by (apply Z.leb_le; lia).
    rewrite Hd, Hg, Hl; simpl. reflexivity.
  - destruct (Z.leb next_dur 0) eqn:Hd; simpl; [reflexivity |].
    apply Z.leb_gt in Hd.
    assert (Hg : Z.geb (t_end - now) next_dur = true) by (rewrite Z.geb_leb; apply Z.leb_le; lia).
    rewrite Hg; simpl; reflexivity.
Qed.

(** ** 2. `Gesture` pointer capture/release balance and retained-vertex lasso filtering *)

Inductive g_event :=
  | GDown (pid : nat) (x : Z) (target : option nat)
  | GMove (pid : nat) (x : Z)
  | GUp (pid : nat) (x : Z)
  | GCancel
  | GNodeExit (key : nat).

Inductive g_cmd :=
  | GCapture (pid : nat)
  | GRelease (pid : nat)
  | GSelectLasso (pts : list Z).

Record g_state := {
  gs_pointers : list (nat * Z);
  gs_target : option nat;
  gs_lasso : list Z
}.

Fixpoint assoc_mem (pid : nat) (ps : list (nat * Z)) : bool :=
  match ps with
  | [] => false
  | (k, _) :: rest => Nat.eqb pid k || assoc_mem pid rest
  end.

Fixpoint assoc_put (pid : nat) (x : Z) (ps : list (nat * Z)) : list (nat * Z) :=
  match ps with
  | [] => [(pid, x)]
  | (k, v) :: rest =>
    if Nat.eqb pid k then (pid, x) :: rest else (k, v) :: assoc_put pid x rest
  end.

Fixpoint assoc_remove (pid : nat) (ps : list (nat * Z)) : list (nat * Z) :=
  match ps with
  | [] => []
  | (k, v) :: rest =>
    if Nat.eqb pid k then assoc_remove pid rest else (k, v) :: assoc_remove pid rest
  end.

Definition gesture_cancel (st : g_state) : g_state * list g_cmd :=
  ({| gs_pointers := []; gs_target := None; gs_lasso := [] |},
   map (fun kv => GRelease (fst kv)) (gs_pointers st)).

Definition lasso_append_fixed (thresh : Z) (x : Z) (lasso : list Z) : list Z :=
  match lasso with
  | [] => [x]
  | last :: _ => if Z.ltb (Z.abs (x - last)) thresh then lasso else x :: lasso
  end.

Definition lasso_append_buggy_raw (thresh : Z) (prev_raw x : Z) (lasso : list Z) : list Z :=
  if Z.ltb (Z.abs (x - prev_raw)) thresh then lasso else x :: lasso.

Definition gesture_step_lasso (thresh : Z) (ev : g_event) (st : g_state) : g_state * list g_cmd :=
  match ev with
  | GCancel => gesture_cancel st
  | GNodeExit k =>
    match gs_target st with
    | Some tk => if Nat.eqb k tk then gesture_cancel st else (st, [])
    | None => (st, [])
    end
  | GDown pid x tgt =>
    if Nat.leb 2 (length (gs_pointers st)) || assoc_mem pid (gs_pointers st) then (st, [])
    else
      let first := match gs_pointers st with [] => true | _ => false end in
      ({| gs_pointers := gs_pointers st ++ [(pid, x)];
          gs_target := if first then tgt else gs_target st;
          gs_lasso := if first then [x] else gs_lasso st |},
       [GCapture pid])
  | GMove pid x =>
    if negb (assoc_mem pid (gs_pointers st)) then (st, [])
    else
      let ps' := assoc_put pid x (gs_pointers st) in
      if Nat.eqb (length ps') 2 then
        ({| gs_pointers := ps'; gs_target := gs_target st; gs_lasso := gs_lasso st |}, [])
      else
        ({| gs_pointers := ps';
            gs_target := gs_target st;
            gs_lasso := lasso_append_fixed thresh x (gs_lasso st) |}, [])
  | GUp pid x =>
    if Nat.eqb (length (gs_pointers st)) 2 && assoc_mem pid (gs_pointers st) then
      gesture_cancel st
    else if negb (assoc_mem pid (gs_pointers st)) then (st, [])
    else
      let las' := lasso_append_fixed thresh x (gs_lasso st) in
      ({| gs_pointers := assoc_remove pid (gs_pointers st);
          gs_target := None;
          gs_lasso := [] |},
       [GRelease pid; GSelectLasso (rev las')])
  end.

Lemma assoc_put_length pid x ps :
  assoc_mem pid ps = true ->
  length (assoc_put pid x ps) = length ps.
Proof.
  induction ps as [|[k v] rest IH]; simpl; [discriminate |].
  destruct (Nat.eqb pid k); simpl; [reflexivity | intros Hm; rewrite (IH Hm); reflexivity].
Qed.

Lemma assoc_remove_length pid ps :
  (length (assoc_remove pid ps) <= length ps)%nat.
Proof.
  induction ps as [|[k v] rest IH]; simpl; [lia |].
  destruct (Nat.eqb pid k); simpl; lia.
Qed.

(**md
**`Gesture` keeps at most two active pointers and releases every captured pointer on cancellation.**

Across every `GDown`, `GMove`, `GUp`, `GCancel`, and `GNodeExit`, `length gs_pointers <= 2` is preserved, and `gesture_cancel` emits `GRelease id` for every active pointer while resetting `gs_pointers` to `[]`.

- [thresh:] minimum distance threshold for recording lasso vertices.
- [ev:] input pointer or lifecycle event.
- [st:] current gesture state satisfying `(length (gs_pointers st) <= 2)%nat`.
*)
Theorem gesture_capture_release_balanced (thresh : Z) (ev : g_event) (st : g_state) :
  (length (gs_pointers st) <= 2)%nat ->
  let '(st', _) := gesture_step_lasso thresh ev st in
  (length (gs_pointers st') <= 2)%nat /\
  fst (gesture_cancel st) = {| gs_pointers := []; gs_target := None; gs_lasso := [] |} /\
  snd (gesture_cancel st) = map (fun kv => GRelease (fst kv)) (gs_pointers st).
Proof.
  intros Hlen. cbv zeta.
  assert (Hnext : (length (gs_pointers (fst (gesture_step_lasso thresh ev st))) <= 2)%nat).
  { destruct ev as [pid x tgt | pid x | pid x | | k]; cbn -[Nat.leb Nat.eqb].
    - destruct (Nat.leb 2%nat (length (gs_pointers st)) || assoc_mem pid (gs_pointers st)) eqn:Hg; cbn; [lia |].
      apply orb_false_elim in Hg as [Hlt _]. apply Nat.leb_gt in Hlt.
      rewrite length_app; cbn; lia.
    - destruct (assoc_mem pid (gs_pointers st)) eqn:Hm; cbn -[Nat.eqb]; [| lia].
      pose proof (assoc_put_length pid x (gs_pointers st) Hm) as Hpl.
      destruct (Nat.eqb (length (assoc_put pid x (gs_pointers st))) 2); cbn; lia.
    - destruct (Nat.eqb (length (gs_pointers st)) 2 && assoc_mem pid (gs_pointers st)); cbn; [lia |].
      destruct (assoc_mem pid (gs_pointers st)); cbn; [| lia].
      pose proof (assoc_remove_length pid (gs_pointers st)); lia.
    - lia.
    - destruct (gs_target st) as [tk |]; [destruct (Nat.eqb k tk); cbn; lia | cbn; lia]. }
  destruct (gesture_step_lasso thresh ev st) as [st' cmds]; simpl in Hnext |- *.
  split; [exact Hnext | split; reflexivity].
Qed.

(**md
**Measuring lasso threshold against `hd lasso` records cumulative motion across sub-threshold `Move` steps.**

When consecutive moves step from `0` to `6` and then to `12` under `thresh = 10`, `lasso_append_fixed` skips `6` (`|6 - 0| < 10`) and records `12` (`|12 - 0| = 12 >= 10`), retaining `[12; 0]`.

- [x0:] initial `Down` coordinate.
- [x1:] intermediate sub-threshold `Move` coordinate.
- [x2:] cumulative threshold-crossing `Move` coordinate.
- [thresh:] minimum vertex distance (`0 < thresh`).
*)
Theorem lasso_retained_distance_records_cumulative_motion (x0 x1 x2 thresh : Z) :
  Z.abs (x1 - x0) < thresh ->
  thresh <= Z.abs (x2 - x0) ->
  let l1 := lasso_append_fixed thresh x1 [x0] in
  let l2 := lasso_append_fixed thresh x2 l1 in
  l2 = [x2; x0].
Proof.
  intros H1 H2. cbv zeta. unfold lasso_append_fixed.
  apply Z.ltb_lt in H1. rewrite H1.
  assert (H2f : Z.ltb (Z.abs (x2 - x0)) thresh = false) by (apply Z.ltb_ge; lia).
  rewrite H2f. reflexivity.
Qed.

(**md
**Measuring lasso distance against `prev_raw` starves the entire stroke on sub-threshold steps (Issue #62).**

Stepping `0 -> 6 -> 12 -> 18` at `thresh = 10` (`|6| < 10` per event, `18 >= 10` total) leaves `lasso_append_buggy_raw` stuck at `[0]` while `lasso_append_fixed` records `[12; 0]`.

- [u:] unit witness triggering the counterexample.
*)
Theorem naive_lasso_raw_delta_starves_falsified (u : unit) :
  let _ := u in
  let buggy :=
    lasso_append_buggy_raw 10 12 18
      (lasso_append_buggy_raw 10 6 12
        (lasso_append_buggy_raw 10 0 6 [0])) in
  let fixed_ :=
    lasso_append_fixed 10 18
      (lasso_append_fixed 10 12
        (lasso_append_fixed 10 6 [0])) in
  buggy = [0] /\ fixed_ = [12; 0].
Proof.
  vm_compute. split; reflexivity.
Qed.

(** ** 3. `ReactiveForce` & `ReactiveLayout` `(generation, revision)` worker fencing (Issue #65) *)

Record force_ctrl := {
  fc_revision : nat;
  fc_current : nat;
  fc_frame_queued : bool
}.

Definition force_dispatch_sync (new_state : nat) (fc : force_ctrl) : force_ctrl :=
  {| fc_revision := S (fc_revision fc);
     fc_current := new_state;
     fc_frame_queued := fc_frame_queued fc |}.

Definition force_apply_worker_fenced (worker_rev worker_state : nat) (fc : force_ctrl) : force_ctrl :=
  if Nat.eqb worker_rev (fc_revision fc) then
    {| fc_revision := S (fc_revision fc);
       fc_current := worker_state;
       fc_frame_queued := fc_frame_queued fc |}
  else fc.

Definition force_apply_worker_unversioned (worker_state : nat) (fc : force_ctrl) : force_ctrl :=
  {| fc_revision := S (fc_revision fc);
     fc_current := worker_state;
     fc_frame_queued := fc_frame_queued fc |}.

Definition publish_frame_safe (request_frame_ok : bool) (fc : force_ctrl) : bool * force_ctrl :=
  if fc_frame_queued fc then (true, fc)
  else if request_frame_ok then
    (true, {| fc_revision := fc_revision fc; fc_current := fc_current fc; fc_frame_queued := true |})
  else
    (* Rollback frameQueued <- false in try/with on RequestFrame exception! *)
    (false, {| fc_revision := fc_revision fc; fc_current := fc_current fc; fc_frame_queued := false |}).

(**md
**Revision-fenced `ReactiveForce` preserves synchronous `Dispatch` edits and recovers cleanly from `RequestFrame` failures.**

Whenever a worker completion carries `worker_rev <= fc_revision fc` and a synchronous `Dispatch` increments `fc_revision`, `force_apply_worker_fenced` rejects the stale worker state (`fc_current = new_state`), and `publish_frame_safe false` resets `fc_frame_queued = false` so the next publication can enqueue a frame.

- [fc:] controller state before synchronous `Dispatch`.
- [new_state:] state installed synchronously by `OwnedForce.Dispatch`.
- [worker_rev:] revision captured when the stale background chunk started (`worker_rev <= fc_revision fc`).
- [worker_state:] stale chunk output arriving later in the same flush.
*)
Theorem reactive_force_revision_fences_stale_worker
    (fc : force_ctrl) (new_state worker_rev worker_state : nat) :
  (worker_rev <= fc_revision fc)%nat ->
  let fc1 := force_dispatch_sync new_state fc in
  let fc2 := force_apply_worker_fenced worker_rev worker_state fc1 in
  fc_current fc2 = new_state /\
  fc_revision fc2 = S (fc_revision fc) /\
  (fc_frame_queued fc = false ->
   fc_frame_queued (snd (publish_frame_safe false fc)) = false).
Proof.
  intros Hle. cbv zeta. unfold force_dispatch_sync, force_apply_worker_fenced; simpl.
  assert (Hneq : Nat.eqb worker_rev (S (fc_revision fc)) = false)
    by (apply Nat.eqb_neq; lia).
  rewrite Hneq.
  split; [reflexivity | split; [reflexivity |]].
  intros Hfq. unfold publish_frame_safe. rewrite Hfq; simpl; reflexivity.
Qed.

(**md
**Unversioned `ReactiveForce` overwrites synchronous `Dispatch` edits with stale worker state (Issue #65).**

Without checking `worker_rev = fc_revision`, applying `worker_state = 10` after `force_dispatch_sync 99` clobbers `fc_current` back to `10`.

- [u:] unit witness triggering the counterexample.
*)
Theorem naive_reactive_force_unversioned_clobbers_falsified (u : unit) :
  let _ := u in
  let fc0 := {| fc_revision := 1%nat; fc_current := 0%nat; fc_frame_queued := false |} in
  let fc1 := force_dispatch_sync 99%nat fc0 in
  fc_current (force_apply_worker_unversioned 10%nat fc1) = 10%nat /\
  fc_current (force_apply_worker_fenced 1%nat 10%nat fc1) = 99%nat.
Proof.
  vm_compute. split; reflexivity.
Qed.

(** ** 4. `<svg>`-bounded ancestor walk (`InputAdapter` / `Renderer.markPath` Issue #67) *)

Inductive dom_tag :=
  | DomPlain (node_id : nat)
  | DomMark (node_id key : nat)
  | DomGroup (node_id grp : nat).

Definition dom_node_id (d : dom_tag) : nat :=
  match d with
  | DomPlain id | DomMark id _ | DomGroup id _ => id
  end.

Fixpoint walk_to_root_svg (root_id : nat) (chain : list dom_tag) (acc : list nat) : option (list nat) :=
  match chain with
  | [] => None
  | d :: parents =>
    if Nat.eqb (dom_node_id d) root_id then
      match acc with [] => None | _ => Some acc end
    else
      let acc' :=
        match acc, d with
        | [], DomMark _ k => [k]
        | _ :: _, DomGroup _ g => g :: acc
        | _, _ => acc
        end in
      walk_to_root_svg root_id parents acc'
  end.

(**md
**Stopping the DOM ancestor walk at `root_id` (`<svg>`) isolates mark paths from any outer HTML ancestors (Issue #67).**

Appending arbitrary `outer_html` ancestors (including leaked `DomGroup` or `DomMark` attributes above `<svg>`) after `DomPlain root_id` has zero effect on `walk_to_root_svg`.

- [root_id:] node ID of the `<svg>` host element.
- [inner_chain:] ancestor chain from `event.target` up to (not including) `root_id`.
- [outer_html:] arbitrary ancestor HTML elements above `<svg>`.
*)
Theorem svg_bounded_mark_path_excludes_outer_ancestors
    (root_id : nat) (inner_chain outer_html : list dom_tag) :
  Forall (fun d => dom_node_id d <> root_id) inner_chain ->
  walk_to_root_svg root_id (inner_chain ++ DomPlain root_id :: outer_html) [] =
  walk_to_root_svg root_id (inner_chain ++ [DomPlain root_id]) [].
Proof.
  intros Hno.
  assert (Hgen : forall acc,
    walk_to_root_svg root_id (inner_chain ++ DomPlain root_id :: outer_html) acc =
    walk_to_root_svg root_id (inner_chain ++ [DomPlain root_id]) acc).
  { induction Hno as [|d rest Hne Hsub IH]; intros acc; simpl.
    - rewrite Nat.eqb_refl. reflexivity.
    - apply Nat.eqb_neq in Hne. rewrite Hne. apply IH. }
  apply Hgen.
Qed.
