module Compiled_pattern = struct
  type t = Pcre2.Interp.t

  include Bin_prot.Utils.Make_binable_with_uuid (struct
    module Binable = struct
      type t = bytes

      let bin_shape_t = Bin_prot.Std.bin_shape_bytes
      let bin_size_t = Bin_prot.Std.bin_size_bytes
      let bin_write_t = Bin_prot.Std.bin_write_bytes
      let bin_read_t = Bin_prot.Std.bin_read_bytes
      let __bin_read_t__ = Bin_prot.Std.__bin_read_bytes__
    end

    type nonrec t = t

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string
        "997a175f-050b-424b-a9dc-41f22f4e3ed0"

    let to_binable t =
      match Pcre2.Interp.to_bytes t with
      | Ok b -> b
      | Error e ->
          failwith
            ("Pcre2_bin_prot.Compiled_pattern: "
            ^ Pcre2.Interp.show_serialize_error e)

    let of_binable b =
      match Pcre2.Interp.of_bytes b with
      | Ok t -> t
      | Error e ->
          failwith
            ("Pcre2_bin_prot.Compiled_pattern: "
            ^ Pcre2.Interp.show_serialize_error e)
  end)
end
