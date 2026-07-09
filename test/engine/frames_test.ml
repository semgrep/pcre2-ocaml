(* Engine module-initialization asserts for [Pcre2_engine.Frames], migrated
   verbatim from src/engine/frames.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Frames

(* Slot offsets and simulated sizes are load-bearing for the interpreter
   chunk and for heap-limit parity: pin them. *)
let test_0 () =
  (* Header slot layout: one slot per field in struct order. *)
  assert (Int.equal slot_ecode 0);
  assert (Int.equal slot_temp_sptr_0 1);
  assert (Int.equal slot_temp_sptr_1 2);
  assert (Int.equal slot_length 3);
  assert (Int.equal slot_back_frame 4);
  assert (Int.equal slot_temp_size 5);
  assert (Int.equal slot_rdepth 6);
  assert (Int.equal slot_group_frame_type 7);
  assert (Int.equal slot_temp_32_0 8);
  assert (Int.equal slot_temp_32_3 11);
  assert (Int.equal slot_return_id 12);
  assert (Int.equal slot_op 13);
  assert (Int.equal slot_occu 14);
  (* occu occupies 6 slots: 14..19, then the copy region starts. *)
  assert (Int.equal slot_eptr (slot_occu + 6));
  assert (Int.equal slot_ovector 28);
  assert (Int.equal frame_header_ints 28);
  (* Simulated C offsets: eptr at 80, ovector at 136; the copied region
     eptr..offset_top is 7 * 8 = 56 bytes = ovector - eptr. *)
  assert (Int.equal offsetof_eptr 80);
  assert (Int.equal offsetof_ovector 136);
  assert (Int.equal (offsetof_ovector - offsetof_eptr) 56);
  (* frame_size (pcre2_match.c:7032-7034) = 136 + 16 * top_bracket. *)
  assert (Int.equal (frame_size_bytes_for ~top_bracket:0) 136);
  assert (Int.equal (frame_size_bytes_for ~top_bracket:1) 152);
  assert (Int.equal (frame_size_bytes_for ~top_bracket:2) 168);
  assert (Int.equal (frame_size_bytes_for ~top_bracket:65535) 1048696);
  (* GF constants (pcre2_match.c:105-119). *)
  assert (Int.equal (gf_idmask (gf_capture lor 3)) gf_capture);
  assert (Int.equal (gf_datamask (gf_capture lor 3)) 3);
  assert (Int.equal (gf_idmask (gf_recurse lor 65535)) gf_recurse);
  assert (Int.equal recurse_unset 0xffffffff)

(* create: default-limit sizing (pcre2_match.c:7053-7054), frame-0 ovector
   all-unset (7079-7083), header zeros. *)
let test_1 () =
  (match
     create ~use_scratch:false ~top_bracket:2 ~heap_limit:Limits.heap_limit
   with
  | Ok a ->
      assert (Int.equal a.frame_size_bytes 168);
      assert (Int.equal a.frame_size_ints 32);
      (* 168 * 10 = 1680 < START_FRAMES_SIZE = 20480. *)
      assert (Int.equal a.heapframes_size 20480);
      (* capacity = floor(20480 / 168) = 121 frames. *)
      assert (Int.equal (Array.length a.frames) (121 * 32));
      (* Frame 0: ovector slots -1, header slots 0. *)
      for i = 0 to slot_ovector - 1 do
        assert (Int.equal a.frames.(i) 0)
      done;
      for i = slot_ovector to a.frame_size_ints - 1 do
        assert (Int.equal a.frames.(i) unset)
      done
  | Error _ -> assert false);
  (* At least 10 frames when the frame is huge: top_bracket = 200 gives
     frame_size 3336, initial size 33360 > START_FRAMES_SIZE. *)
  (match
     create ~use_scratch:false ~top_bracket:200 ~heap_limit:Limits.heap_limit
   with
  | Ok a ->
      assert (Int.equal a.frame_size_bytes 3336);
      assert (Int.equal a.heapframes_size 33360);
      assert (Int.equal (Array.length a.frames) (10 * a.frame_size_ints))
  | Error _ -> assert false);
  (* create-time clamp (pcre2_match.c:7055-7060): 1 KiB limit clamps
     20480 -> 1024; a 0 KiB limit cannot hold one frame -> HEAPLIMIT. *)
  (match create ~use_scratch:false ~top_bracket:0 ~heap_limit:1 with
  | Ok a ->
      assert (Int.equal a.heapframes_size 1024);
      assert (Int.equal (Array.length a.frames) (7 * 28))
  | Error _ -> assert false);
  match create ~use_scratch:false ~top_bracket:0 ~heap_limit:0 with
  | Error e -> assert (Int.equal e Errors.error_heaplimit)
  | Ok _ -> assert false

(* push: copy boundary and rdepth (pcre2_match.c:745-754), frame
   isolation, and the RMATCH copy carrying frame-0 captures upward. *)
let test_2 () =
  match
    create ~use_scratch:false ~top_bracket:2 ~heap_limit:Limits.heap_limit
  with
  | Error _ -> assert false
  | Ok a ->
      (* Populate frame 0: header scratch + copied fields + captures. *)
      a.frames.(slot_ecode) <- 111;
      a.frames.(slot_temp_sptr_0) <- 222;
      a.frames.(slot_op) <- 33;
      a.frames.(slot_eptr) <- 5;
      a.frames.(slot_start_match) <- 3;
      a.frames.(slot_mark) <- unset;
      a.frames.(slot_recurse_last_used) <- 9;
      a.frames.(slot_current_recurse) <- recurse_unset;
      a.frames.(slot_capture_last) <- 1;
      a.frames.(slot_last_group_offset) <- unset;
      a.frames.(slot_offset_top) <- 2;
      a.frames.(slot_ovector) <- 10;
      a.frames.(slot_ovector + 1) <- 11;
      let n = push a 0 in
      assert (Int.equal n 1);
      let b1 = base a 1 in
      (* Copied region: eptr .. ovector end. *)
      assert (Int.equal a.frames.(b1 + slot_eptr) 5);
      assert (Int.equal a.frames.(b1 + slot_start_match) 3);
      assert (Int.equal a.frames.(b1 + slot_mark) unset);
      assert (Int.equal a.frames.(b1 + slot_recurse_last_used) 9);
      assert (Int.equal a.frames.(b1 + slot_current_recurse) recurse_unset);
      assert (Int.equal a.frames.(b1 + slot_capture_last) 1);
      assert (Int.equal a.frames.(b1 + slot_last_group_offset) unset);
      assert (Int.equal a.frames.(b1 + slot_offset_top) 2);
      assert (Int.equal a.frames.(b1 + slot_ovector) 10);
      assert (Int.equal a.frames.(b1 + slot_ovector + 1) 11);
      assert (Int.equal a.frames.(b1 + slot_ovector + 2) unset);
      assert (Int.equal a.frames.(b1 + slot_ovector + 3) unset);
      (* Header slots are NOT copied. *)
      assert (Int.equal a.frames.(b1 + slot_ecode) 0);
      assert (Int.equal a.frames.(b1 + slot_temp_sptr_0) 0);
      assert (Int.equal a.frames.(b1 + slot_op) 0);
      (* rdepth = previous + 1. *)
      assert (Int.equal a.frames.(b1 + slot_rdepth) 1);
      (* Frame isolation: writing N's ovector leaves F intact. *)
      a.frames.(b1 + slot_ovector) <- 99;
      assert (Int.equal a.frames.(slot_ovector) 10);
      (* Chained push: rdepth increments per frame. *)
      let n2 = push a 1 in
      assert (Int.equal n2 2);
      assert (Int.equal a.frames.(base a 2 + slot_rdepth) 2);
      assert (Int.equal a.frames.(base a 2 + slot_ovector) 99)

(* Heap-limit accounting parity. Worked against pcre2_match.c:662-712 by
   hand for frame_size 136 (top_bracket 0):

   - heap_limit 20 KiB: initial vector 20480 bytes = frames 0..149 usable
     (frame 150 would end at 20536 >= 20480 -> grow; doubling to 40960
     trips 40960/1024 = 40 >= 20 with heap_limit <= old_size 20 ->
     HEAPLIMIT). No growth ever succeeds.
   - heap_limit 21 KiB: same trigger at frame 150, but now the doubling is
     clamped to old_size 20 + 1 KiB -> newsize 21504; frames 0..157 usable
     (frame 158 would end at 21624 >= 21504 -> grow; 21504/1024 = 21 and
     heap_limit <= 21 -> HEAPLIMIT). *)
let test_3 () =
  (match create ~use_scratch:false ~top_bracket:0 ~heap_limit:20 with
  | Error _ -> assert false
  | Ok a ->
      let f = ref 0 in
      let rc = ref 0 in
      while !rc >= 0 && !f < 1000 do
        rc := push a !f;
        if !rc >= 0 then f := !rc
      done;
      assert (Int.equal !f 149);
      assert (Int.equal !rc Errors.error_heaplimit);
      assert (Int.equal a.heapframes_size 20480));
  (match create ~use_scratch:false ~top_bracket:0 ~heap_limit:21 with
  | Error _ -> assert false
  | Ok a ->
      a.frames.(slot_eptr) <- 77 (* survives growth + copies to every N *);
      let f = ref 0 in
      let rc = ref 0 in
      while !rc >= 0 && !f < 1000 do
        rc := push a !f;
        if !rc >= 0 then f := !rc
      done;
      assert (Int.equal !f 157);
      assert (Int.equal !rc Errors.error_heaplimit);
      (* The clamped growth landed exactly on 21 KiB. *)
      assert (Int.equal a.heapframes_size 21504);
      assert (Int.equal (Array.length a.frames) (21504 / 136 * 28));
      (* Data survived the reallocation, at both ends of the chain. *)
      assert (Int.equal a.frames.(slot_eptr) 77);
      assert (Int.equal a.frames.(base a 157 + slot_eptr) 77);
      assert (Int.equal a.frames.(base a 157 + slot_rdepth) 157));
  (* The over_bytes adjustment (pcre2_match.c:691-692): initial size 33360
     (top_bracket 200, frame 3336 bytes) is not a whole KiB (over 592);
     with heap_limit 40, growth at frame 9 clamps 66720 down to
     33360 + 1024*(40-32) - (1024-592) = 41120. *)
  match create ~use_scratch:false ~top_bracket:200 ~heap_limit:40 with
  | Error _ -> assert false
  | Ok a ->
      a.frames.(slot_ovector) <- 12345;
      let f = ref 0 in
      for _ = 1 to 9 do
        let n = push a !f in
        assert (n >= 0);
        f := n
      done;
      assert (Int.equal !f 9);
      assert (Int.equal a.heapframes_size 41120);
      (* Used prefix (including frame 0's captures) copied by grow. *)
      assert (Int.equal a.frames.(slot_ovector) 12345);
      assert (Int.equal a.frames.(base a 9 + slot_ovector) 12345)

(* Scratch-slot protocol (pcre2_match.c:7062-7077 reuse + the busy flag
   and retention-cap DEVIATIONs): acquire, busy fallback, write-back,
   dirty reuse with the frame-0 ovector re-fill, undersized replacement,
   cap drop. Runs last and leaves the slot exactly as module init found
   it (empty, not busy) — the fresh-path creates above never touch it. *)
let test_4 () =
  assert (Int.equal (Array.length !scratch_arena) 0);
  assert (not (Atomic.get scratch_busy));
  (match
     create ~use_scratch:true ~top_bracket:2 ~heap_limit:Limits.heap_limit
   with
  | Error _ -> assert false
  | Ok a ->
      (* Empty slot: fresh (zeroed) arena, but the slot is now held. *)
      assert a.holds_scratch;
      assert (Atomic.get scratch_busy);
      for i = 0 to slot_ovector - 1 do
        assert (Int.equal a.frames.(i) 0)
      done;
      (* Busy slot: a nested/concurrent match falls back to a fresh
         arena and its release is a no-op. *)
      (match
         create ~use_scratch:true ~top_bracket:2 ~heap_limit:Limits.heap_limit
       with
      | Error _ -> assert false
      | Ok b ->
          assert (not b.holds_scratch);
          assert (not (b.frames == a.frames));
          release b;
          assert (Atomic.get scratch_busy);
          assert (Int.equal (Array.length !scratch_arena) 0));
      a.frames.(slot_ecode) <- 424242 (* dirt: must survive reuse *);
      release a;
      assert (not (Atomic.get scratch_busy));
      assert (!scratch_arena == a.frames));
  (* Reuse: the same array comes back, NOT re-zeroed; only frame 0's
     ovector region is re-marked unset (pcre2_match.c:7079-7083). *)
  (match
     create ~use_scratch:true ~top_bracket:2 ~heap_limit:Limits.heap_limit
   with
  | Error _ -> assert false
  | Ok a ->
      assert a.holds_scratch;
      assert (a.frames == !scratch_arena);
      assert (Int.equal a.frames.(slot_ecode) 424242);
      for i = slot_ovector to a.frame_size_ints - 1 do
        assert (Int.equal a.frames.(i) unset)
      done;
      release a);
  (* Undersized slot: a bigger pattern gets a fresh arena, and the
     write-back replaces the slot's old contents (the C's free + malloc,
     pcre2_match.c:7064-7076). *)
  (match
     create ~use_scratch:true ~top_bracket:200 ~heap_limit:Limits.heap_limit
   with
  | Error _ -> assert false
  | Ok a ->
      assert a.holds_scratch;
      assert (not (a.frames == !scratch_arena));
      release a;
      assert (!scratch_arena == a.frames));
  (* Retention cap: an arena grown past [scratch_max_retained_ints]
     (simulated here by re-pointing [frames] like [grow] does) is dropped
     at release instead of staying pinned in the slot. *)
  (match
     create ~use_scratch:true ~top_bracket:2 ~heap_limit:Limits.heap_limit
   with
  | Error _ -> assert false
  | Ok a ->
      assert a.holds_scratch;
      a.frames <- Array.make (scratch_max_retained_ints + 1) 0;
      release a;
      assert (not (Atomic.get scratch_busy));
      assert (Int.equal (Array.length !scratch_arena) 0));
  (* The slot is back to its pristine state. *)
  assert (Int.equal (Array.length !scratch_arena) 0);
  assert (not (Atomic.get scratch_busy))

let tests =
  [
    Alcotest.test_case "frames 0" `Quick test_0;
    Alcotest.test_case "frames 1" `Quick test_1;
    Alcotest.test_case "frames 2" `Quick test_2;
    Alcotest.test_case "frames 3" `Quick test_3;
    Alcotest.test_case "frames 4" `Quick test_4;
  ]
