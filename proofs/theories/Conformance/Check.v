From Stdlib Require Import List ZArith.
From RanvierD5.Pure Require Import DataScale Spatial SamplingSelection GeometryGeo TransitionWorker.
From RanvierD5 Require Import Extract.
From RanvierD5.Conformance Require Import Expected.
Import ListNotations.
Open Scope nat_scope.

(** Each [check_Sxx] verifies by [vm_compute] that the Rocq pure model matches the
    concrete output [exp_Sxx] recorded by [proofs/conformance/Scenarios.fsx] from
    running the real F# implementation. *)

Example check_S01 :
  observe_bin_and_join [0%Z; 10%Z; 20%Z; 30%Z] [(-5)%Z; 0%Z; 9%Z; 10%Z; 20%Z; 30%Z; 31%Z]
    [1; 2; 3; 4] [3; 5; 1; 6] = exp_S01.
Proof. vm_compute. reflexivity. Qed.

Example check_S02 :
  observe_scale_and_nice 0%Z 10%Z 1000%Z 0%Z true [(-5)%Z; 0%Z; 3%Z; 10%Z; 15%Z]
    101%Z 199%Z 2%Z = exp_S02.
Proof. vm_compute. reflexivity. Qed.

Example check_S03 :
  observe_scale_and_nice (-10)%Z 10%Z (-200)%Z 200%Z false [(-15)%Z; (-10)%Z; 0%Z; 5%Z; 12%Z]
    88%Z 12%Z 5%Z = exp_S03.
Proof. vm_compute. reflexivity. Qed.

Example check_S04 :
  observe_spatial
    [{| px := 10%Z; py := 10%Z |}; {| px := (-10)%Z; py := 10%Z |}; {| px := 10%Z; py := (-10)%Z |};
     {| px := 3%Z; py := 4%Z |}; {| px := (-3)%Z; py := (-4)%Z |}; {| px := 25%Z; py := 25%Z |}]
    {| min_x := (-12)%Z; max_x := 12%Z; min_y := (-5)%Z; max_y := 12%Z |}
    {| px := 0%Z; py := 0%Z |} = exp_S04.
Proof. vm_compute. reflexivity. Qed.

Example check_S05 :
  observe_sampling_selection 6
    [10%Z; 50%Z; 2%Z; 30%Z; 80%Z; (-5)%Z; 40%Z; 12%Z; 90%Z; 20%Z]
    [1; 2; 3; 4; 5] [1; 3; 5; 3; 99]
    {| sel_available := [1; 2; 3; 4; 5]; sel_selected := [2; 3; 99];
       sel_focused := Some 2; sel_hovered := Some 3 |}
    [[1; 2]; []; [0; 3; 4]] = exp_S05.
Proof. vm_compute. reflexivity. Qed.

Example check_S06 :
  observe_contour_sankey_geo false 5
    10%Z 50%Z 20%Z 90%Z 25%Z
    {| s_lon := 20%Z; s_lat := 80%Z |}
    [{| s_lon := (-170)%Z; s_lat := 90%Z |}; {| s_lon := 10%Z; s_lat := 80%Z |}] = exp_S06.
Proof. vm_compute. reflexivity. Qed.

Example check_S07 :
  observe_contour_sankey_geo true 5
    10%Z 50%Z 30%Z (-20)%Z 35%Z
    {| s_lon := (-150)%Z; s_lat := (-60)%Z |}
    [{| s_lon := (-30)%Z; s_lat := (-60)%Z |}; {| s_lon := 90%Z; s_lat := (-60)%Z |};
     {| s_lon := 170%Z; s_lat := (-60)%Z |}] = exp_S07.
Proof. vm_compute. reflexivity. Qed.

Example check_S08 :
  observe_transition_gesture_worker
    {| seg_from := 0%Z; seg_target := 64%Z; seg_started := 0%Z; seg_duration := 8%Z |}
    4%Z 128%Z 6%Z
    8%Z [GDown 1 0%Z (Some 7); GMove 1 5%Z; GMove 1 10%Z; GMove 1 15%Z; GUp 1 15%Z]
    true 42 99 true
    100 [DomMark 1 7; DomGroup 2 3; DomPlain 100; DomGroup 3 88] = exp_S08.
Proof. vm_compute. reflexivity. Qed.

Example check_S09 :
  observe_transition_gesture_worker
    {| seg_from := 64%Z; seg_target := 0%Z; seg_started := 10%Z; seg_duration := 8%Z |}
    12%Z (-64)%Z 20%Z
    8%Z [GDown 1 0%Z (Some 5); GDown 2 16%Z None; GUp 1 8%Z]
    false 42 99 false
    100 [DomMark 1 7; DomGroup 2 3; DomPlain 50] = exp_S09.
Proof. vm_compute. reflexivity. Qed.

Example check_S10 :
  observe_bin_and_join [(-20)%Z; 0%Z; 20%Z] [(-20)%Z; (-20)%Z; 0%Z; 0%Z; 20%Z; 20%Z]
    [10; 20] [20; 30] = exp_S10.
Proof. vm_compute. reflexivity. Qed.
