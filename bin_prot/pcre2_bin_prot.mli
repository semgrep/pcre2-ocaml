(** Bin_prot serializers for [Pcre2] compiled patterns.

    Built on top of [Pcre2.Interp.to_bytes] / [Pcre2.Interp.of_bytes]. The same
    caveats apply: blobs are PCRE2-version + platform specific, and JIT state
    is not preserved (re-apply [Pcre2.Jit.of_interp] after deserialization). *)

module Compiled_pattern : sig
  type t = Pcre2.Interp.t

  include Bin_prot.Binable.S with type t := t
end
