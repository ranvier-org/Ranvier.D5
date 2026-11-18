(* OCaml driver for Ranvier.D5 differential conformance.
   Reads one S-expression program per line from stdin, runs the extracted Rocq pure model
   from [Engine], and prints [<index>\tok\t<canonical_observation>] per line. *)

open Engine

type sexp = Atom of string | Node of sexp Stdlib.List.t

let parse_sexp (text : string) : sexp =
  let n = Stdlib.String.length text in
  let i = ref 0 in
  let skip () =
    while !i < n && (text.[!i] = ' ' || text.[!i] = '\t' || text.[!i] = '\r') do
      incr i
    done
  in
  let rec one () =
    skip ();
    if text.[!i] = '(' then begin
      incr i;
      let items = ref [] in
      skip ();
      while text.[!i] <> ')' do
        items := one () :: !items;
        skip ()
      done;
      incr i;
      Node (Stdlib.List.rev !items)
    end else begin
      let start = !i in
      while !i < n && text.[!i] <> ' ' && text.[!i] <> '\t' && text.[!i] <> '\r'
            && text.[!i] <> ')' && text.[!i] <> '(' do
        incr i
      done;
      Atom (Stdlib.String.sub text start (!i - start))
    end
  in
  one ()

let rec nat_of_int (n : int) : nat = if n <= 0 then O else S (nat_of_int (n - 1))

let int_of_nat (n : nat) : int =
  let rec go acc = function O -> acc | S m -> go (acc + 1) m in
  go 0 n

let rec positive_of_int (n : int) : positive =
  if n = 1 then XH
  else if n land 1 = 1 then XI (positive_of_int (n lsr 1))
  else XO (positive_of_int (n lsr 1))

let rec int_of_positive (p : positive) : int =
  match p with
  | XH -> 1
  | XO q -> 2 * int_of_positive q
  | XI q -> (2 * int_of_positive q) + 1

let z_of_int (n : int) : z =
  if n = 0 then Z0 else if n > 0 then Zpos (positive_of_int n) else Zneg (positive_of_int (-n))

let int_of_z (v : z) : int =
  match v with
  | Z0 -> 0
  | Zpos p -> int_of_positive p
  | Zneg p -> -int_of_positive p

let bool_of_atom = function
  | Atom "1" | Atom "true" -> True
  | Atom "0" | Atom "false" -> False
  | _ -> failwith "expected bool"

let str_of_bool = function True -> "true" | False -> "false"

let atom_int = function
  | Atom a -> int_of_string a
  | Node _ -> failwith "expected int atom"

let rec coq_list_of_ocaml = function
  | [] -> Nil
  | x :: xs -> Cons (x, coq_list_of_ocaml xs)

let rec ocaml_list_of_coq = function
  | Nil -> []
  | Cons (x, xs) -> x :: ocaml_list_of_coq xs

let nat_list_of_sexp = function
  | Node xs -> coq_list_of_ocaml (Stdlib.List.map (fun x -> nat_of_int (atom_int x)) xs)
  | Atom _ -> failwith "expected nat list"

let z_list_of_sexp = function
  | Node xs -> coq_list_of_ocaml (Stdlib.List.map (fun x -> z_of_int (atom_int x)) xs)
  | Atom _ -> failwith "expected z list"

let opt_nat_of_sexp = function
  | Node [ Atom "none" ] -> None
  | Node [ Atom "some"; v ] -> Some (nat_of_int (atom_int v))
  | _ -> failwith "expected opt nat"

let fmt_nat_list (xs : nat list) : string =
  "[" ^ Stdlib.String.concat ";" (Stdlib.List.map (fun n -> string_of_int (int_of_nat n)) (ocaml_list_of_coq xs)) ^ "]"

let fmt_z_list (xs : z list) : string =
  "[" ^ Stdlib.String.concat ";" (Stdlib.List.map (fun v -> string_of_int (int_of_z v)) (ocaml_list_of_coq xs)) ^ "]"

let fmt_opt_nat = function
  | Some n -> Printf.sprintf "Some(%d)" (int_of_nat n)
  | None -> "None"

let fmt_opt_nat_list (xs : nat option list) : string =
  "[" ^ Stdlib.String.concat ";" (Stdlib.List.map fmt_opt_nat (ocaml_list_of_coq xs)) ^ "]"

(* 1. bin-join *)
let run_bin_join = function
  | [ edges; values; prev_keys; next_keys ] ->
    let Pair (bins, jm) =
      observe_bin_and_join
        (z_list_of_sexp edges)
        (z_list_of_sexp values)
        (nat_list_of_sexp prev_keys)
        (nat_list_of_sexp next_keys)
    in
    Printf.sprintf "bins=%s;enter=%s;update=%s;exit=%s"
      (fmt_opt_nat_list bins)
      (fmt_nat_list jm.jm_enter)
      (fmt_nat_list jm.jm_update)
      (fmt_nat_list jm.jm_exit)
  | _ -> failwith "bad bin-join"

(* 2. scale-nice *)
let run_scale_nice = function
  | [ dom_lo; dom_hi; rng_lo; rng_hi; clamped; queries; nice_lo; nice_hi; nice_cnt ] ->
    let Pair (mapped, Pair (nlo, nhi)) =
      observe_scale_and_nice
        (z_of_int (atom_int dom_lo))
        (z_of_int (atom_int dom_hi))
        (z_of_int (atom_int rng_lo))
        (z_of_int (atom_int rng_hi))
        (bool_of_atom clamped)
        (z_list_of_sexp queries)
        (z_of_int (atom_int nice_lo))
        (z_of_int (atom_int nice_hi))
        (z_of_int (atom_int nice_cnt))
    in
    Printf.sprintf "mapped=%s;nlo=%d;nhi=%d"
      (fmt_z_list mapped)
      (int_of_z nlo)
      (int_of_z nhi)
  | _ -> failwith "bad scale-nice"

(* 3. spatial *)
let read_pt2 = function
  | Node [ x; y ] -> { px = z_of_int (atom_int x); py = z_of_int (atom_int y) }
  | _ -> failwith "bad pt2"

let run_spatial = function
  | [ Node pts; Node [ min_x; max_x; min_y; max_y ]; qpt ] ->
    let box_ =
      { min_x = z_of_int (atom_int min_x);
        max_x = z_of_int (atom_int max_x);
        min_y = z_of_int (atom_int min_y);
        max_y = z_of_int (atom_int max_y) }
    in
    let Pair (hits, near_ord) =
      observe_spatial
        (coq_list_of_ocaml (Stdlib.List.map read_pt2 pts))
        box_
        (read_pt2 qpt)
    in
    Printf.sprintf "hits=%s;near=%s" (fmt_nat_list hits) (fmt_opt_nat near_ord)
  | _ -> failwith "bad spatial"

(* 4. sampling-sel *)
let run_sampling_sel = function
  | [ target; ys; src_ids; vis_ids; Node [ avail; sel; foc; hov ]; Node kparts ] ->
    let st0 =
      { sel_available = nat_list_of_sexp avail;
        sel_selected = nat_list_of_sexp sel;
        sel_focused = opt_nat_of_sexp foc;
        sel_hovered = opt_nat_of_sexp hov }
    in
    let kp = coq_list_of_ocaml (Stdlib.List.map nat_list_of_sexp kparts) in
    let Pair (Pair (mm, st1), kstream) =
      observe_sampling_selection
        (nat_of_int (atom_int target))
        (z_list_of_sexp ys)
        (nat_list_of_sexp src_ids)
        (nat_list_of_sexp vis_ids)
        st0
        kp
    in
    Printf.sprintf "mm=%s;avail=%s;sel=%s;foc=%s;hov=%s;key=%s"
      (fmt_nat_list mm)
      (fmt_nat_list st1.sel_available)
      (fmt_nat_list st1.sel_selected)
      (fmt_opt_nat st1.sel_focused)
      (fmt_opt_nat st1.sel_hovered)
      (fmt_nat_list kstream)
  | _ -> failwith "bad sampling-sel"

(* 5. contour-sankey-geo *)
let read_sph_pt = function
  | Node [ lo; la ] -> { s_lon = z_of_int (atom_int lo); s_lat = z_of_int (atom_int la) }
  | _ -> failwith "bad sph_pt"

let run_contour_sankey_geo = function
  | [ saddle_pos; code; Node [ lo_b; hi_b; y0; target_y; link_y ]; r0; Node rrest ] ->
    let Pair (Pair (shells, Pair (y', link_y')), winding) =
      observe_contour_sankey_geo
        (bool_of_atom saddle_pos)
        (nat_of_int (atom_int code))
        (z_of_int (atom_int lo_b))
        (z_of_int (atom_int hi_b))
        (z_of_int (atom_int y0))
        (z_of_int (atom_int target_y))
        (z_of_int (atom_int link_y))
        (read_sph_pt r0)
        (coq_list_of_ocaml (Stdlib.List.map read_sph_pt rrest))
    in
    Printf.sprintf "shells=%d;y=%d;linky=%d;winding=%d"
      (int_of_nat shells)
      (int_of_z y')
      (int_of_z link_y')
      (int_of_z winding)
  | _ -> failwith "bad contour-sankey-geo"

(* 6. trans-gesture-worker *)
let read_gevent = function
  | Node [ Atom "down"; pid; x; tgt ] ->
    GDown (nat_of_int (atom_int pid), z_of_int (atom_int x), opt_nat_of_sexp tgt)
  | Node [ Atom "move"; pid; x ] ->
    GMove (nat_of_int (atom_int pid), z_of_int (atom_int x))
  | Node [ Atom "up"; pid; x ] ->
    GUp (nat_of_int (atom_int pid), z_of_int (atom_int x))
  | Node [ Atom "cancel" ] -> GCancel
  | Node [ Atom "exit"; k ] -> GNodeExit (nat_of_int (atom_int k))
  | _ -> failwith "bad g_event"

let read_dom_tag = function
  | Node [ Atom "plain"; nid ] -> DomPlain (nat_of_int (atom_int nid))
  | Node [ Atom "mark"; nid; k ] -> DomMark (nat_of_int (atom_int nid), nat_of_int (atom_int k))
  | Node [ Atom "group"; nid; g ] -> DomGroup (nat_of_int (atom_int nid), nat_of_int (atom_int g))
  | _ -> failwith "bad dom_tag"

let run_trans_gesture_worker = function
  | [ Node [ s_from; s_tgt; s_start; s_dur ];
      Node [ ret_t; next_v; samp_t ];
      Node evs;
      Node [ stale_w; sync_v; work_v; req_ok ];
      Node [ root_id; Node doms ] ] ->
    let seg0 =
      { seg_from = z_of_int (atom_int s_from);
        seg_target = z_of_int (atom_int s_tgt);
        seg_started = z_of_int (atom_int s_start);
        seg_duration = z_of_int (atom_int s_dur) }
    in
    let Pair (Pair (Pair (sampled, Pair (Pair (ptrs, rels), lassos)), Pair (fcur, fq)), mpath) =
      observe_transition_gesture_worker
        seg0
        (z_of_int (atom_int ret_t))
        (z_of_int (atom_int next_v))
        (z_of_int (atom_int samp_t))
        (z_of_int 8)
        (coq_list_of_ocaml (Stdlib.List.map read_gevent evs))
        (bool_of_atom stale_w)
        (nat_of_int (atom_int sync_v))
        (nat_of_int (atom_int work_v))
        (bool_of_atom req_ok)
        (nat_of_int (atom_int root_id))
        (coq_list_of_ocaml (Stdlib.List.map read_dom_tag doms))
    in
    let fmt_lassos =
      "[" ^ Stdlib.String.concat "|" (Stdlib.List.map fmt_z_list (ocaml_list_of_coq lassos)) ^ "]"
    in
    let fmt_mpath =
      match mpath with
      | None -> "None"
      | Some ks -> "Some(" ^ fmt_nat_list ks ^ ")"
    in
    Printf.sprintf "samp=%d;ptrs=%d;rel=%d;lassos=%s;fcur=%d;fq=%s;mpath=%s"
      (int_of_z sampled)
      (int_of_nat ptrs)
      (int_of_nat rels)
      fmt_lassos
      (int_of_nat fcur)
      (str_of_bool fq)
      fmt_mpath
  | _ -> failwith "bad trans-gesture-worker"

let eval_prog = function
  | Node (Atom "bin-join" :: args) -> run_bin_join args
  | Node (Atom "scale-nice" :: args) -> run_scale_nice args
  | Node (Atom "spatial" :: args) -> run_spatial args
  | Node (Atom "sampling-sel" :: args) -> run_sampling_sel args
  | Node (Atom "contour-sankey-geo" :: args) -> run_contour_sankey_geo args
  | Node (Atom "trans-gesture-worker" :: args) -> run_trans_gesture_worker args
  | _ -> failwith "unknown program head"

let () =
  let idx = ref 0 in
  try
    while true do
      let line = input_line stdin in
      if Stdlib.String.length (Stdlib.String.trim line) > 0 then begin
        let obs = eval_prog (parse_sexp line) in
        Printf.printf "%d\tok\t%s\n" !idx obs;
        incr idx
      end
    done
  with End_of_file -> ()
