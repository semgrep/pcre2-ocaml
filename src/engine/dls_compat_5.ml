(* OCaml >= 5.0: real per-domain storage. Selected by the dune rule on
   %{ocaml_version}; see dls_compat_4.ml for the pre-domains fallback. *)
module DLS = Domain.DLS
