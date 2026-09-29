(* Probe without -pthread: that flag can hide a separate libpthread link
   dependency on systems with older glibc. *)
let pthread_probe =
  {|
#include <pthread.h>
static pthread_key_t key;
static pthread_once_t once = PTHREAD_ONCE_INIT;
static void make_key(void) { pthread_key_create(&key, NULL); }
int main(void) {
        pthread_once(&once, make_key);
        return pthread_setspecific(key, NULL);
}
|}

let () =
  let module C = Configurator.V1 in
  C.main ~name:"pcre2" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = [ "-lpcre2-8" ]; cflags = [] }
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
            Option.value (C.Pkg_config.query pc ~package:"libpcre2-8") ~default
      in
      let uses_pthread =
        match C.ocaml_config_var c "system" with
        | Some ("win32" | "win64" | "mingw" | "mingw64" | "msvc") -> false
        | _ -> true
      in
      let needs_lpthread = uses_pthread && not (C.c_test c pthread_probe) in
      let cflags = if uses_pthread then conf.cflags @ [ "-pthread" ] else conf.cflags in
      let libs = if needs_lpthread then conf.libs @ [ "-lpthread" ] else conf.libs in
      C.Flags.write_sexp "c_flags.sexp" cflags;
      C.Flags.write_sexp "c_library_flags.sexp" libs)
