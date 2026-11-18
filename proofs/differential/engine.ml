
type bool =
| True
| False

(** val negb : bool -> bool **)

let negb = function
| True -> False
| False -> True

type nat =
| O
| S of nat

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

(** val fst : ('a1, 'a2) prod -> 'a1 **)

let fst = function
| Pair (x, _) -> x

(** val snd : ('a1, 'a2) prod -> 'a2 **)

let snd = function
| Pair (_, y) -> y

type 'a list =
| Nil
| Cons of 'a * 'a list

(** val length : 'a1 list -> nat **)

let rec length = function
| Nil -> O
| Cons (_, l') -> S (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | Nil -> m
  | Cons (a, l1) -> Cons (a, (app l1 m))

type comparison =
| Eq
| Lt
| Gt

(** val compOpp : comparison -> comparison **)

let compOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt

(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val mul : nat -> nat -> nat **)

let rec mul n m =
  match n with
  | O -> O
  | S p -> add m (mul p m)

(** val sub : nat -> nat -> nat **)

let rec sub n m =
  match n with
  | O -> n
  | S k -> (match m with
            | O -> n
            | S l -> sub k l)

module Nat =
 struct
  (** val pred : nat -> nat **)

  let pred n = match n with
  | O -> n
  | S u -> u

  (** val eqb : nat -> nat -> bool **)

  let rec eqb n m =
    match n with
    | O -> (match m with
            | O -> True
            | S _ -> False)
    | S n' -> (match m with
               | O -> False
               | S m' -> eqb n' m')

  (** val leb : nat -> nat -> bool **)

  let rec leb n m =
    match n with
    | O -> True
    | S n' -> (match m with
               | O -> False
               | S m' -> leb n' m')

  (** val ltb : nat -> nat -> bool **)

  let ltb n m =
    leb (S n) m

  (** val divmod : nat -> nat -> nat -> nat -> (nat, nat) prod **)

  let rec divmod x y q u =
    match x with
    | O -> Pair (q, u)
    | S x' ->
      (match u with
       | O -> divmod x' y (S q) y
       | S u' -> divmod x' y q u')

  (** val div : nat -> nat -> nat **)

  let div x y = match y with
  | O -> y
  | S y' -> fst (divmod x y' O y')
 end

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| Nil -> Nil
| Cons (a, l0) -> Cons ((f a), (map f l0))

(** val firstn : nat -> 'a1 list -> 'a1 list **)

let rec firstn n l =
  match n with
  | O -> Nil
  | S n0 ->
    (match l with
     | Nil -> Nil
     | Cons (a, l0) -> Cons (a, (firstn n0 l0)))

(** val skipn : nat -> 'a1 list -> 'a1 list **)

let rec skipn n l =
  match n with
  | O -> l
  | S n0 -> (match l with
             | Nil -> Nil
             | Cons (_, l0) -> skipn n0 l0)

(** val nth_error : 'a1 list -> nat -> 'a1 option **)

let rec nth_error l = function
| O -> (match l with
        | Nil -> None
        | Cons (x, _) -> Some x)
| S n0 -> (match l with
           | Nil -> None
           | Cons (_, l') -> nth_error l' n0)

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| Nil -> Nil
| Cons (x, l') -> app (rev l') (Cons (x, Nil))

(** val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec filter f = function
| Nil -> Nil
| Cons (x, l0) ->
  (match f x with
   | True -> Cons (x, (filter f l0))
   | False -> filter f l0)

type positive =
| XI of positive
| XO of positive
| XH

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XO p ->
      (match y with
       | XI q -> XI (add p q)
       | XO q -> XO (add p q)
       | XH -> XI p)
    | XH -> (match y with
             | XI q -> XO (succ q)
             | XO q -> XI q
             | XH -> XO XH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XI (add_carry p q)
       | XO q -> XO (add_carry p q)
       | XH -> XI (succ p))
    | XO p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XH ->
      (match y with
       | XI q -> XI (succ q)
       | XO q -> XO (succ q)
       | XH -> XI XH)

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> compare_cont r p q
       | XO q -> compare_cont Gt p q
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q -> compare_cont Lt p q
       | XO q -> compare_cont r p q
       | XH -> Gt)
    | XH -> (match y with
             | XH -> r
             | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val eqb : positive -> positive -> bool **)

  let rec eqb p q =
    match p with
    | XI p0 -> (match q with
                | XI q0 -> eqb p0 q0
                | _ -> False)
    | XO p0 -> (match q with
                | XO q0 -> eqb p0 q0
                | _ -> False)
    | XH -> (match q with
             | XH -> True
             | _ -> False)
 end

module Z =
 struct
  (** val double : z -> z **)

  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (XO p)
  | Zneg p -> Zneg (XO p)

  (** val succ_double : z -> z **)

  let succ_double = function
  | Z0 -> Zpos XH
  | Zpos p -> Zpos (XI p)
  | Zneg p -> Zneg (Pos.pred_double p)

  (** val pred_double : z -> z **)

  let pred_double = function
  | Z0 -> Zneg XH
  | Zpos p -> Zpos (Pos.pred_double p)
  | Zneg p -> Zneg (XI p)

  (** val pos_sub : positive -> positive -> z **)

  let rec pos_sub x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double (pos_sub p q)
       | XO q -> succ_double (pos_sub p q)
       | XH -> Zpos (XO p))
    | XO p ->
      (match y with
       | XI q -> pred_double (pos_sub p q)
       | XO q -> double (pos_sub p q)
       | XH -> Zpos (Pos.pred_double p))
    | XH ->
      (match y with
       | XI q -> Zneg (XO q)
       | XO q -> Zneg (Pos.pred_double q)
       | XH -> Z0)

  (** val add : z -> z -> z **)

  let add x y =
    match x with
    | Z0 -> y
    | Zpos x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> Zpos (Pos.add x' y')
       | Zneg y' -> pos_sub x' y')
    | Zneg x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> pos_sub y' x'
       | Zneg y' -> Zneg (Pos.add x' y'))

  (** val opp : z -> z **)

  let opp = function
  | Z0 -> Z0
  | Zpos x0 -> Zneg x0
  | Zneg x0 -> Zpos x0

  (** val sub : z -> z -> z **)

  let sub m n =
    add m (opp n)

  (** val mul : z -> z -> z **)

  let mul x y =
    match x with
    | Z0 -> Z0
    | Zpos x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zpos (Pos.mul x' y')
       | Zneg y' -> Zneg (Pos.mul x' y'))
    | Zneg x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zneg (Pos.mul x' y')
       | Zneg y' -> Zpos (Pos.mul x' y'))

  (** val compare : z -> z -> comparison **)

  let compare x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> Eq
             | Zpos _ -> Lt
             | Zneg _ -> Gt)
    | Zpos x' -> (match y with
                  | Zpos y' -> Pos.compare x' y'
                  | _ -> Gt)
    | Zneg x' ->
      (match y with
       | Zneg y' -> compOpp (Pos.compare x' y')
       | _ -> Lt)

  (** val leb : z -> z -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> False
    | _ -> True

  (** val ltb : z -> z -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> True
    | _ -> False

  (** val eqb : z -> z -> bool **)

  let eqb x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> True
             | _ -> False)
    | Zpos p -> (match y with
                 | Zpos q -> Pos.eqb p q
                 | _ -> False)
    | Zneg p -> (match y with
                 | Zneg q -> Pos.eqb p q
                 | _ -> False)

  (** val max : z -> z -> z **)

  let max n m =
    match compare n m with
    | Lt -> m
    | _ -> n

  (** val min : z -> z -> z **)

  let min n m =
    match compare n m with
    | Gt -> m
    | _ -> n

  (** val pos_div_eucl : positive -> z -> (z, z) prod **)

  let rec pos_div_eucl a b =
    match a with
    | XI a' ->
      let Pair (q, r) = pos_div_eucl a' b in
      let r' = add (mul (Zpos (XO XH)) r) (Zpos XH) in
      (match ltb r' b with
       | True -> Pair ((mul (Zpos (XO XH)) q), r')
       | False -> Pair ((add (mul (Zpos (XO XH)) q) (Zpos XH)), (sub r' b)))
    | XO a' ->
      let Pair (q, r) = pos_div_eucl a' b in
      let r' = mul (Zpos (XO XH)) r in
      (match ltb r' b with
       | True -> Pair ((mul (Zpos (XO XH)) q), r')
       | False -> Pair ((add (mul (Zpos (XO XH)) q) (Zpos XH)), (sub r' b)))
    | XH ->
      (match leb (Zpos (XO XH)) b with
       | True -> Pair (Z0, (Zpos XH))
       | False -> Pair ((Zpos XH), Z0))

  (** val div_eucl : z -> z -> (z, z) prod **)

  let div_eucl a b =
    match a with
    | Z0 -> Pair (Z0, Z0)
    | Zpos a' ->
      (match b with
       | Z0 -> Pair (Z0, a)
       | Zpos _ -> pos_div_eucl a' b
       | Zneg b' ->
         let Pair (q, r) = pos_div_eucl a' (Zpos b') in
         (match r with
          | Z0 -> Pair ((opp q), Z0)
          | _ -> Pair ((opp (add q (Zpos XH))), (add b r))))
    | Zneg a' ->
      (match b with
       | Z0 -> Pair (Z0, a)
       | Zpos _ ->
         let Pair (q, r) = pos_div_eucl a' b in
         (match r with
          | Z0 -> Pair ((opp q), Z0)
          | _ -> Pair ((opp (add q (Zpos XH))), (sub b r)))
       | Zneg b' ->
         let Pair (q, r) = pos_div_eucl a' (Zpos b') in Pair (q, (opp r)))

  (** val div : z -> z -> z **)

  let div a b =
    let Pair (q, _) = div_eucl a b in q

  (** val geb : z -> z -> bool **)

  let geb x y =
    match compare x y with
    | Lt -> False
    | _ -> True

  (** val gtb : z -> z -> bool **)

  let gtb x y =
    match compare x y with
    | Gt -> True
    | _ -> False

  (** val abs : z -> z **)

  let abs = function
  | Zneg p -> Zpos p
  | x -> x
 end

(** val bisect_right_list : z list -> z -> nat **)

let rec bisect_right_list edges x =
  match edges with
  | Nil -> O
  | Cons (e, rest) ->
    (match Z.leb e x with
     | True -> S (bisect_right_list rest x)
     | False -> O)

(** val last_nonempty : z -> z list -> z **)

let rec last_nonempty x = function
| Nil -> x
| Cons (y, tl) -> last_nonempty y tl

(** val bin_index : z list -> z -> nat option **)

let bin_index edges x =
  match edges with
  | Nil -> None
  | Cons (lo, l) ->
    (match l with
     | Nil -> None
     | Cons (b, rest) ->
       let hi = last_nonempty b rest in
       (match match Z.ltb x lo with
              | True -> True
              | False -> Z.ltb hi x with
        | True -> None
        | False ->
          let k = S (length rest) in
          let raw = Nat.pred (bisect_right_list edges x) in
          Some
          (match Nat.ltb (Nat.pred k) raw with
           | True -> Nat.pred k
           | False -> raw)))

(** val mem_key : nat -> nat list -> bool **)

let rec mem_key k = function
| Nil -> False
| Cons (y, rest) ->
  (match Nat.eqb k y with
   | True -> True
   | False -> mem_key k rest)

type join_model = { jm_enter : nat list; jm_update : nat list;
                    jm_exit : nat list }

(** val data_join_keys : nat list -> nat list -> join_model **)

let data_join_keys prev next =
  { jm_enter = (filter (fun k -> negb (mem_key k prev)) next); jm_update =
    (filter (fun k -> mem_key k prev) next); jm_exit =
    (filter (fun k -> negb (mem_key k next)) prev) }

(** val clamp_z : z -> z -> z -> z **)

let clamp_z lo hi v =
  let mn = Z.min lo hi in let mx = Z.max lo hi in Z.max mn (Z.min mx v)

(** val floor_step : z -> z -> z **)

let floor_step x step =
  Z.mul (Z.div x step) step

(** val ceil_step : z -> z -> z **)

let ceil_step x step =
  Z.mul (Z.opp (Z.div (Z.opp x) step)) step

(** val nice_direct : z -> z -> z -> (z, z) prod **)

let nice_direct lo hi step =
  Pair ((floor_step lo step), (ceil_step hi step))

type pt2 = { px : z; py : z }

type aabb = { min_x : z; max_x : z; min_y : z; max_y : z }

(** val in_aabb : aabb -> pt2 -> bool **)

let in_aabb b p =
  match match match Z.leb b.min_x p.px with
              | True -> Z.leb p.px b.max_x
              | False -> False with
        | True -> Z.leb b.min_y p.py
        | False -> False with
  | True -> Z.leb p.py b.max_y
  | False -> False

(** val dist_sq : pt2 -> pt2 -> z **)

let dist_sq a b =
  let dx = Z.sub a.px b.px in
  let dy = Z.sub a.py b.py in Z.add (Z.mul dx dx) (Z.mul dy dy)

type kdtree =
| KdEmpty
| KdNode of bool * pt2 * nat * kdtree * kdtree

(** val kd_within : aabb -> kdtree -> (pt2, nat) prod list **)

let rec kd_within b = function
| KdEmpty -> Nil
| KdNode (sx, p, o, l, r) ->
  let c = match sx with
          | True -> p.px
          | False -> p.py in
  let lo = match sx with
           | True -> b.min_x
           | False -> b.min_y in
  let hi = match sx with
           | True -> b.max_x
           | False -> b.max_y in
  let go_l = match Z.leb lo c with
             | True -> kd_within b l
             | False -> Nil in
  let here =
    match in_aabb b p with
    | True -> Cons ((Pair (p, o)), Nil)
    | False -> Nil
  in
  let go_r = match Z.leb c hi with
             | True -> kd_within b r
             | False -> Nil in
  app go_l (app here go_r)

(** val bucket_pair : nat -> nat -> nat list **)

let bucket_pair lo hi =
  match Nat.eqb lo hi with
  | True -> Cons (lo, Nil)
  | False ->
    (match Nat.ltb lo hi with
     | True -> Cons (lo, (Cons (hi, Nil)))
     | False -> Cons (hi, (Cons (lo, Nil))))

(** val emit_interior_buckets : (nat, nat) prod list -> nat -> nat list **)

let rec emit_interior_buckets buckets last_idx =
  match buckets with
  | Nil -> Cons (last_idx, Nil)
  | Cons (p, rest) ->
    let Pair (i_min, i_max) = p in
    app (bucket_pair i_min i_max) (emit_interior_buckets rest last_idx)

(** val minmax_indices : nat -> (nat, nat) prod list -> nat list **)

let minmax_indices len buckets =
  match len with
  | O -> Nil
  | S last_idx ->
    (match last_idx with
     | O -> Cons (O, Nil)
     | S _ -> Cons (O, (emit_interior_buckets buckets last_idx)))

(** val mem_id : nat -> nat list -> bool **)

let rec mem_id x = function
| Nil -> False
| Cons (y, rest) ->
  (match Nat.eqb x y with
   | True -> True
   | False -> mem_id x rest)

(** val dedup_keep_first : nat list -> nat list -> nat list **)

let rec dedup_keep_first seen = function
| Nil -> Nil
| Cons (x, rest) ->
  (match mem_id x seen with
   | True -> dedup_keep_first seen rest
   | False -> Cons (x, (dedup_keep_first (Cons (x, seen)) rest)))

type sel_state = { sel_available : nat list; sel_selected : nat list;
                   sel_focused : nat option; sel_hovered : nat option }

(** val filter_opt : nat list -> nat option -> nat option **)

let filter_opt allowed = function
| Some x -> (match mem_id x allowed with
             | True -> Some x
             | False -> None)
| None -> None

(** val selection_reconcile :
    nat list -> nat list -> sel_state -> sel_state **)

let selection_reconcile source_ids visible_ids st =
  let avail =
    dedup_keep_first Nil (filter (fun id -> mem_id id source_ids) visible_ids)
  in
  { sel_available = avail; sel_selected =
  (filter (fun id -> mem_id id source_ids) st.sel_selected); sel_focused =
  (filter_opt avail st.sel_focused); sel_hovered =
  (filter_opt avail st.sel_hovered) }

(** val encode_length_prefixed : nat list list -> nat list **)

let rec encode_length_prefixed = function
| Nil -> Nil
| Cons (p, rest) -> Cons ((length p), (app p (encode_length_prefixed rest)))

type sph_pt = { s_lon : z; s_lat : z }

(** val canonical_pole : sph_pt -> sph_pt **)

let canonical_pole p =
  match Z.eqb (Z.abs p.s_lat) (Zpos (XO (XI (XO (XI (XI (XO XH))))))) with
  | True -> { s_lon = Z0; s_lat = p.s_lat }
  | False -> p

(** val seam_step : sph_pt -> sph_pt -> z **)

let seam_step a b =
  let d = Z.sub b.s_lon a.s_lon in
  (match Z.gtb d (Zpos (XO (XO (XI (XO (XI (XI (XO XH)))))))) with
   | True -> Zpos XH
   | False ->
     (match Z.ltb d (Zneg (XO (XO (XI (XO (XI (XI (XO XH)))))))) with
      | True -> Zneg XH
      | False -> Z0))

(** val canonical_seam_winding : sph_pt -> sph_pt list -> z **)

let rec canonical_seam_winding prev pts =
  let prev_c = canonical_pole prev in
  (match pts with
   | Nil -> Z0
   | Cons (p, rest) ->
     let pc = canonical_pole p in
     Z.add (seam_step prev_c pc) (canonical_seam_winding pc rest))

(** val sankey_clamp_move : z -> z -> z -> z **)

let sankey_clamp_move lo_bound hi_bound target_y =
  Z.max lo_bound (Z.min hi_bound target_y)

type lin_segment = { seg_from : z; seg_target : z; seg_started : z;
                     seg_duration : z }

(** val sample_step : z -> lin_segment -> z **)

let sample_step now s =
  match match Z.leb s.seg_duration Z0 with
        | True -> True
        | False -> Z.geb (Z.sub now s.seg_started) s.seg_duration with
  | True -> s.seg_target
  | False ->
    (match Z.leb now s.seg_started with
     | True -> s.seg_from
     | False ->
       Z.add s.seg_from
         (Z.div
           (Z.mul (Z.sub now s.seg_started) (Z.sub s.seg_target s.seg_from))
           s.seg_duration))

(** val retarget_step : z -> z -> z -> lin_segment -> lin_segment **)

let retarget_step now next_target next_dur s =
  { seg_from = (sample_step now s); seg_target = next_target; seg_started =
    now; seg_duration = next_dur }

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

(** val assoc_mem : nat -> (nat, z) prod list -> bool **)

let rec assoc_mem pid = function
| Nil -> False
| Cons (p, rest) ->
  let Pair (k, _) = p in
  (match Nat.eqb pid k with
   | True -> True
   | False -> assoc_mem pid rest)

(** val assoc_put : nat -> z -> (nat, z) prod list -> (nat, z) prod list **)

let rec assoc_put pid x = function
| Nil -> Cons ((Pair (pid, x)), Nil)
| Cons (p, rest) ->
  let Pair (k, v) = p in
  (match Nat.eqb pid k with
   | True -> Cons ((Pair (pid, x)), rest)
   | False -> Cons ((Pair (k, v)), (assoc_put pid x rest)))

(** val assoc_remove : nat -> (nat, z) prod list -> (nat, z) prod list **)

let rec assoc_remove pid = function
| Nil -> Nil
| Cons (p, rest) ->
  let Pair (k, v) = p in
  (match Nat.eqb pid k with
   | True -> assoc_remove pid rest
   | False -> Cons ((Pair (k, v)), (assoc_remove pid rest)))

(** val gesture_cancel : g_state -> (g_state, g_cmd list) prod **)

let gesture_cancel st =
  Pair ({ gs_pointers = Nil; gs_target = None; gs_lasso = Nil },
    (map (fun kv -> GRelease (fst kv)) st.gs_pointers))

(** val lasso_append_fixed : z -> z -> z list -> z list **)

let lasso_append_fixed thresh x lasso = match lasso with
| Nil -> Cons (x, Nil)
| Cons (last, _) ->
  (match Z.ltb (Z.abs (Z.sub x last)) thresh with
   | True -> lasso
   | False -> Cons (x, lasso))

(** val gesture_step_lasso :
    z -> g_event -> g_state -> (g_state, g_cmd list) prod **)

let gesture_step_lasso thresh ev st =
  match ev with
  | GDown (pid, x, tgt) ->
    (match match Nat.leb (S (S O)) (length st.gs_pointers) with
           | True -> True
           | False -> assoc_mem pid st.gs_pointers with
     | True -> Pair (st, Nil)
     | False ->
       let first =
         match st.gs_pointers with
         | Nil -> True
         | Cons (_, _) -> False
       in
       Pair ({ gs_pointers =
       (app st.gs_pointers (Cons ((Pair (pid, x)), Nil))); gs_target =
       (match first with
        | True -> tgt
        | False -> st.gs_target);
       gs_lasso =
       (match first with
        | True -> Cons (x, Nil)
        | False -> st.gs_lasso) },
       (Cons ((GCapture pid), Nil))))
  | GMove (pid, x) ->
    (match negb (assoc_mem pid st.gs_pointers) with
     | True -> Pair (st, Nil)
     | False ->
       let ps' = assoc_put pid x st.gs_pointers in
       (match Nat.eqb (length ps') (S (S O)) with
        | True ->
          Pair ({ gs_pointers = ps'; gs_target = st.gs_target; gs_lasso =
            st.gs_lasso }, Nil)
        | False ->
          Pair ({ gs_pointers = ps'; gs_target = st.gs_target; gs_lasso =
            (lasso_append_fixed thresh x st.gs_lasso) }, Nil)))
  | GUp (pid, x) ->
    (match match Nat.eqb (length st.gs_pointers) (S (S O)) with
           | True -> assoc_mem pid st.gs_pointers
           | False -> False with
     | True -> gesture_cancel st
     | False ->
       (match negb (assoc_mem pid st.gs_pointers) with
        | True -> Pair (st, Nil)
        | False ->
          let las' = lasso_append_fixed thresh x st.gs_lasso in
          Pair ({ gs_pointers = (assoc_remove pid st.gs_pointers);
          gs_target = None; gs_lasso = Nil }, (Cons ((GRelease pid), (Cons
          ((GSelectLasso (rev las')), Nil)))))))
  | GCancel -> gesture_cancel st
  | GNodeExit k ->
    (match st.gs_target with
     | Some tk ->
       (match Nat.eqb k tk with
        | True -> gesture_cancel st
        | False -> Pair (st, Nil))
     | None -> Pair (st, Nil))

type force_ctrl = { fc_revision : nat; fc_current : nat;
                    fc_frame_queued : bool }

(** val force_dispatch_sync : nat -> force_ctrl -> force_ctrl **)

let force_dispatch_sync new_state fc =
  { fc_revision = (S fc.fc_revision); fc_current = new_state;
    fc_frame_queued = fc.fc_frame_queued }

(** val force_apply_worker_fenced : nat -> nat -> force_ctrl -> force_ctrl **)

let force_apply_worker_fenced worker_rev worker_state fc =
  match Nat.eqb worker_rev fc.fc_revision with
  | True ->
    { fc_revision = (S fc.fc_revision); fc_current = worker_state;
      fc_frame_queued = fc.fc_frame_queued }
  | False -> fc

(** val publish_frame_safe : bool -> force_ctrl -> (bool, force_ctrl) prod **)

let publish_frame_safe request_frame_ok fc =
  match fc.fc_frame_queued with
  | True -> Pair (True, fc)
  | False ->
    Pair (request_frame_ok, { fc_revision = fc.fc_revision; fc_current =
      fc.fc_current; fc_frame_queued = request_frame_ok })

type dom_tag =
| DomPlain of nat
| DomMark of nat * nat
| DomGroup of nat * nat

(** val dom_node_id : dom_tag -> nat **)

let dom_node_id = function
| DomPlain id -> id
| DomMark (id, _) -> id
| DomGroup (id, _) -> id

(** val walk_to_root_svg :
    nat -> dom_tag list -> nat list -> nat list option **)

let rec walk_to_root_svg root_id chain acc =
  match chain with
  | Nil -> None
  | Cons (d, parents) ->
    (match Nat.eqb (dom_node_id d) root_id with
     | True -> (match acc with
                | Nil -> None
                | Cons (_, _) -> Some acc)
     | False ->
       let acc' =
         match acc with
         | Nil -> (match d with
                   | DomMark (_, k) -> Cons (k, Nil)
                   | _ -> acc)
         | Cons (_, _) ->
           (match d with
            | DomGroup (_, g) -> Cons (g, acc)
            | _ -> acc)
       in
       walk_to_root_svg root_id parents acc')

(** val observe_bin_and_join :
    z list -> z list -> nat list -> nat list -> (nat option list, join_model)
    prod **)

let observe_bin_and_join edges values prev_keys next_keys =
  Pair ((map (bin_index edges) values), (data_join_keys prev_keys next_keys))

(** val pow10_unit : nat -> z -> z -> z -> z **)

let rec pow10_unit fuel span count cur =
  match fuel with
  | O -> cur
  | S f ->
    (match Z.leb (Z.mul (Z.mul count cur) (Zpos (XO (XI (XO XH))))) span with
     | True -> pow10_unit f span count (Z.mul cur (Zpos (XO (XI (XO XH)))))
     | False -> cur)

(** val nice_step_exact : z -> z -> z -> z **)

let nice_step_exact lo hi count =
  let span = Z.sub hi lo in
  (match match Z.leb span Z0 with
         | True -> True
         | False -> Z.leb count Z0 with
   | True -> Z0
   | False ->
     let unit0 = pow10_unit (S (S (S (S (S (S O)))))) span count (Zpos XH) in
     let denom = Z.mul count unit0 in
     let lhs = Z.mul span span in
     let rhs = Z.mul denom denom in
     let factor =
       match Z.geb lhs (Z.mul (Zpos (XO (XI (XO (XO (XI XH)))))) rhs) with
       | True -> Zpos (XO (XI (XO XH)))
       | False ->
         (match Z.geb lhs (Z.mul (Zpos (XO (XI (XO XH)))) rhs) with
          | True -> Zpos (XI (XO XH))
          | False ->
            (match Z.geb lhs (Z.mul (Zpos (XO XH)) rhs) with
             | True -> Zpos (XO XH)
             | False -> Zpos XH))
     in
     Z.mul factor unit0)

(** val linear_map_exact : z -> z -> z -> z -> bool -> z -> z **)

let linear_map_exact dom_lo dom_hi rng_lo rng_hi clamped v =
  match Z.eqb dom_lo dom_hi with
  | True -> Z.div (Z.add rng_lo rng_hi) (Zpos (XO XH))
  | False ->
    let raw =
      Z.add rng_lo
        (Z.div (Z.mul (Z.sub v dom_lo) (Z.sub rng_hi rng_lo))
          (Z.sub dom_hi dom_lo))
    in
    (match clamped with
     | True -> clamp_z rng_lo rng_hi raw
     | False -> raw)

(** val observe_scale_and_nice :
    z -> z -> z -> z -> bool -> z list -> z -> z -> z -> (z list, (z, z)
    prod) prod **)

let observe_scale_and_nice dom_lo dom_hi rng_lo rng_hi clamped queries nice_lo nice_hi nice_count =
  let mapped =
    map (linear_map_exact dom_lo dom_hi rng_lo rng_hi clamped) queries
  in
  let lo = Z.min nice_lo nice_hi in
  let hi = Z.max nice_lo nice_hi in
  let step = nice_step_exact lo hi nice_count in
  let Pair (nlo, nhi) =
    match Z.leb step Z0 with
    | True -> Pair (lo, hi)
    | False -> nice_direct lo hi step
  in
  let nice_dom =
    match Z.leb nice_lo nice_hi with
    | True -> Pair (nlo, nhi)
    | False -> Pair (nhi, nlo)
  in
  Pair (mapped, nice_dom)

(** val insert_nat_sorted : nat -> nat list -> nat list **)

let rec insert_nat_sorted x xs = match xs with
| Nil -> Cons (x, Nil)
| Cons (y, rest) ->
  (match Nat.leb x y with
   | True -> Cons (x, xs)
   | False -> Cons (y, (insert_nat_sorted x rest)))

(** val sort_nats : nat list -> nat list **)

let rec sort_nats = function
| Nil -> Nil
| Cons (x, rest) -> insert_nat_sorted x (sort_nats rest)

(** val entry_lt_x : (pt2, nat) prod -> (pt2, nat) prod -> bool **)

let entry_lt_x a b =
  let Pair (pa, oa) = a in
  let Pair (pb, ob) = b in
  (match Z.ltb pa.px pb.px with
   | True -> True
   | False ->
     (match Z.eqb pa.px pb.px with
      | True -> Nat.ltb oa ob
      | False -> False))

(** val entry_lt_y : (pt2, nat) prod -> (pt2, nat) prod -> bool **)

let entry_lt_y a b =
  let Pair (pa, oa) = a in
  let Pair (pb, ob) = b in
  (match Z.ltb pa.py pb.py with
   | True -> True
   | False ->
     (match Z.eqb pa.py pb.py with
      | True -> Nat.ltb oa ob
      | False -> False))

(** val insert_entry :
    bool -> (pt2, nat) prod -> (pt2, nat) prod list -> (pt2, nat) prod list **)

let rec insert_entry sx x xs = match xs with
| Nil -> Cons (x, Nil)
| Cons (y, rest) ->
  let le =
    match sx with
    | True -> negb (entry_lt_x y x)
    | False -> negb (entry_lt_y y x)
  in
  (match le with
   | True -> Cons (x, xs)
   | False -> Cons (y, (insert_entry sx x rest)))

(** val sort_entries :
    bool -> (pt2, nat) prod list -> (pt2, nat) prod list **)

let rec sort_entries sx = function
| Nil -> Nil
| Cons (x, rest) -> insert_entry sx x (sort_entries sx rest)

(** val build_kdtree : nat -> bool -> (pt2, nat) prod list -> kdtree **)

let rec build_kdtree fuel sx entries =
  match fuel with
  | O -> KdEmpty
  | S f ->
    (match entries with
     | Nil -> KdEmpty
     | Cons (_, _) ->
       let sorted = sort_entries sx entries in
       let mid = Nat.div (length sorted) (S (S O)) in
       (match nth_error sorted mid with
        | Some p0 ->
          let Pair (p, o) = p0 in
          let left_es = firstn mid sorted in
          let right_es = skipn (S mid) sorted in
          KdNode (sx, p, o, (build_kdtree f (negb sx) left_es),
          (build_kdtree f (negb sx) right_es))
        | None -> KdEmpty))

(** val index_pts : nat -> pt2 list -> (pt2, nat) prod list **)

let rec index_pts idx = function
| Nil -> Nil
| Cons (p, rest) -> Cons ((Pair (p, idx)), (index_pts (S idx) rest))

(** val kd_nearest_acc :
    pt2 -> kdtree -> (z, nat) prod option -> (z, nat) prod option **)

let rec kd_nearest_acc q t best =
  match t with
  | KdEmpty -> best
  | KdNode (sx, p, o, l, r) ->
    let d2 = dist_sq q p in
    let best1 =
      match best with
      | Some p0 ->
        let Pair (bd2, bo) = p0 in
        (match match Z.ltb d2 bd2 with
               | True -> True
               | False ->
                 (match Z.eqb d2 bd2 with
                  | True -> Nat.ltb o bo
                  | False -> False) with
         | True -> Some (Pair (d2, o))
         | False -> best)
      | None -> Some (Pair (d2, o))
    in
    let delta =
      match sx with
      | True -> Z.sub q.px p.px
      | False -> Z.sub q.py p.py
    in
    (match Z.ltb delta Z0 with
     | True ->
       let best2 = kd_nearest_acc q l best1 in
       let search_far =
         match best2 with
         | Some p0 -> let Pair (bd2, _) = p0 in Z.leb (Z.mul delta delta) bd2
         | None -> True
       in
       (match search_far with
        | True -> kd_nearest_acc q r best2
        | False -> best2)
     | False ->
       let best2 = kd_nearest_acc q r best1 in
       let search_far =
         match best2 with
         | Some p0 -> let Pair (bd2, _) = p0 in Z.leb (Z.mul delta delta) bd2
         | None -> True
       in
       (match search_far with
        | True -> kd_nearest_acc q l best2
        | False -> best2))

(** val observe_spatial :
    pt2 list -> aabb -> pt2 -> (nat list, nat option) prod **)

let observe_spatial pts query_box query_pt =
  let tree = build_kdtree (S (length pts)) True (index_pts O pts) in
  let hits = sort_nats (map snd (kd_within query_box tree)) in
  let near_ord =
    match kd_nearest_acc query_pt tree None with
    | Some p -> let Pair (_, o) = p in Some o
    | None -> None
  in
  Pair (hits, near_ord)

(** val scan_bucket_extrema :
    nat -> nat -> z list -> nat -> nat -> z -> z -> (nat, nat) prod **)

let rec scan_bucket_extrema idx stop ys lo_i hi_i lo_v hi_v =
  match stop with
  | O -> Pair (lo_i, hi_i)
  | S rem ->
    (match nth_error ys idx with
     | Some v ->
       (match Z.ltb v lo_v with
        | True ->
          (match Z.gtb v hi_v with
           | True -> scan_bucket_extrema (S idx) rem ys idx idx v v
           | False -> scan_bucket_extrema (S idx) rem ys idx hi_i v hi_v)
        | False ->
          (match Z.gtb v hi_v with
           | True -> scan_bucket_extrema (S idx) rem ys lo_i idx lo_v v
           | False -> scan_bucket_extrema (S idx) rem ys lo_i hi_i lo_v hi_v))
     | None -> Pair (lo_i, hi_i))

(** val compute_minmax_buckets_fuel :
    nat -> nat -> nat -> nat -> z list -> (nat, nat) prod list **)

let rec compute_minmax_buckets_fuel rem b buckets interior ys =
  match rem with
  | O -> Nil
  | S r ->
    let first = add (S O) (Nat.div (mul b interior) buckets) in
    let last_ = add (S O) (Nat.div (mul (S b) interior) buckets) in
    let v0 = match nth_error ys first with
             | Some v -> v
             | None -> Z0 in
    let pair =
      scan_bucket_extrema (S first) (sub last_ (S first)) ys first first v0 v0
    in
    Cons (pair, (compute_minmax_buckets_fuel r (S b) buckets interior ys))

(** val seq_nats : nat -> nat -> nat list **)

let rec seq_nats start = function
| O -> Nil
| S c -> Cons (start, (seq_nats (S start) c))

(** val run_minmax_exact : nat -> z list -> nat list **)

let run_minmax_exact target ys =
  let n = length ys in
  (match Nat.leb n target with
   | True -> seq_nats O n
   | False ->
     let buckets = Nat.div (sub target (S (S O))) (S (S O)) in
     let interior = sub n (S (S O)) in
     let pairs = compute_minmax_buckets_fuel buckets O buckets interior ys in
     minmax_indices n pairs)

(** val observe_sampling_selection :
    nat -> z list -> nat list -> nat list -> sel_state -> nat list list ->
    ((nat list, sel_state) prod, nat list) prod **)

let observe_sampling_selection target ys source_ids visible_ids st key_parts =
  Pair ((Pair ((run_minmax_exact target ys),
    (selection_reconcile source_ids visible_ids st))),
    (encode_length_prefixed key_parts))

(** val contour_2x2_shells : bool -> nat -> nat **)

let contour_2x2_shells saddle_pos code = match code with
| O -> O
| S n ->
  (match n with
   | O ->
     (match Nat.ltb code (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
              O)))))))))))))))) with
      | True -> S O
      | False -> O)
   | S n0 ->
     (match n0 with
      | O ->
        (match Nat.ltb code (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                 O)))))))))))))))) with
         | True -> S O
         | False -> O)
      | S n1 ->
        (match n1 with
         | O ->
           (match Nat.ltb code (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                    (S O)))))))))))))))) with
            | True -> S O
            | False -> O)
         | S n2 ->
           (match n2 with
            | O ->
              (match Nat.ltb code (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                       (S (S O)))))))))))))))) with
               | True -> S O
               | False -> O)
            | S n3 ->
              (match n3 with
               | O -> (match saddle_pos with
                       | True -> S O
                       | False -> S (S O))
               | S n4 ->
                 (match n4 with
                  | O ->
                    (match Nat.ltb code (S (S (S (S (S (S (S (S (S (S (S (S
                             (S (S (S (S O)))))))))))))))) with
                     | True -> S O
                     | False -> O)
                  | S n5 ->
                    (match n5 with
                     | O ->
                       (match Nat.ltb code (S (S (S (S (S (S (S (S (S (S (S
                                (S (S (S (S (S O)))))))))))))))) with
                        | True -> S O
                        | False -> O)
                     | S n6 ->
                       (match n6 with
                        | O ->
                          (match Nat.ltb code (S (S (S (S (S (S (S (S (S (S
                                   (S (S (S (S (S (S O)))))))))))))))) with
                           | True -> S O
                           | False -> O)
                        | S n7 ->
                          (match n7 with
                           | O ->
                             (match Nat.ltb code (S (S (S (S (S (S (S (S (S
                                      (S (S (S (S (S (S (S O)))))))))))))))) with
                              | True -> S O
                              | False -> O)
                           | S n8 ->
                             (match n8 with
                              | O ->
                                (match saddle_pos with
                                 | True -> S (S O)
                                 | False -> S O)
                              | S _ ->
                                (match Nat.ltb code (S (S (S (S (S (S (S (S
                                         (S (S (S (S (S (S (S (S
                                         O)))))))))))))))) with
                                 | True -> S O
                                 | False -> O)))))))))))

(** val observe_contour_sankey_geo :
    bool -> nat -> z -> z -> z -> z -> z -> sph_pt -> sph_pt list -> ((nat,
    (z, z) prod) prod, z) prod **)

let observe_contour_sankey_geo saddle_pos code lo_b hi_b y0 target_y link_y ring_start ring_rest =
  let shells = contour_2x2_shells saddle_pos code in
  let y' = sankey_clamp_move lo_b hi_b target_y in
  let link_y' = Z.add link_y (Z.sub y' y0) in
  let winding =
    canonical_seam_winding ring_start (app ring_rest (Cons (ring_start, Nil)))
  in
  Pair ((Pair (shells, (Pair (y', link_y')))), winding)

(** val run_gesture_events :
    z -> g_event list -> g_state -> (g_state, g_cmd list) prod **)

let rec run_gesture_events thresh evs st =
  match evs with
  | Nil -> Pair (st, Nil)
  | Cons (e, rest) ->
    let Pair (st1, c1) = gesture_step_lasso thresh e st in
    let Pair (st2, c2) = run_gesture_events thresh rest st1 in
    Pair (st2, (app c1 c2))

(** val collect_lassos : g_cmd list -> z list list **)

let rec collect_lassos = function
| Nil -> Nil
| Cons (g, rest) ->
  (match g with
   | GSelectLasso pts -> Cons (pts, (collect_lassos rest))
   | _ -> collect_lassos rest)

(** val count_releases : g_cmd list -> nat **)

let rec count_releases = function
| Nil -> O
| Cons (g, rest) ->
  (match g with
   | GRelease _ -> S (count_releases rest)
   | _ -> count_releases rest)

(** val observe_transition_gesture_worker :
    lin_segment -> z -> z -> z -> z -> g_event list -> bool -> nat -> nat ->
    bool -> nat -> dom_tag list -> (((z, ((nat, nat) prod, z list list) prod)
    prod, (nat, bool) prod) prod, nat list option) prod **)

let observe_transition_gesture_worker s0 retarget_t next_val sample_t lasso_thresh evs stale_worker sync_val worker_val req_ok root_id dom_chain =
  let s1 = retarget_step retarget_t next_val s0.seg_duration s0 in
  let sampled = sample_step sample_t s1 in
  let Pair (gst, gcmds) =
    run_gesture_events lasso_thresh evs { gs_pointers = Nil; gs_target =
      None; gs_lasso = Nil }
  in
  let fc0 = { fc_revision = O; fc_current = O; fc_frame_queued = False } in
  let fc1 = force_dispatch_sync sync_val fc0 in
  let worker_rev = match stale_worker with
                   | True -> O
                   | False -> S O in
  let fc2 = force_apply_worker_fenced worker_rev worker_val fc1 in
  let Pair (_, fc3) = publish_frame_safe req_ok fc2 in
  let mark_path = walk_to_root_svg root_id dom_chain Nil in
  Pair ((Pair ((Pair (sampled, (Pair ((Pair ((length gst.gs_pointers),
  (count_releases gcmds))), (collect_lassos gcmds))))), (Pair
  (fc3.fc_current, fc3.fc_frame_queued)))), mark_path)
