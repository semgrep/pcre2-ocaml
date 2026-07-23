(* OCaml < 5.0 (the library still supports >= 4.14): there are no domains, so
   a single global cell per key is correct and contention-free — this is
   exactly the pre-change behavior. Selected by the dune rule on
   %{ocaml_version}; see dls_compat_5.ml for the domains-aware variant.
   Only [new_key]/[get] are used by callers ([set] provided for completeness);
   the stored value is typically a mutable record or a ref that callers mutate
   in place. *)
module DLS = struct
  type 'a key = 'a ref

  let new_key (init : unit -> 'a) : 'a key = ref (init ())
  let get (k : 'a key) : 'a = !k
  let set (k : 'a key) (v : 'a) : unit = k := v
end
