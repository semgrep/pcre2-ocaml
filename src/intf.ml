(* Matcher-API factoring (fast-engine milestone chunk A): the shared
   convenience types and the [Matcher] signature moved verbatim to
   src/matcher/intf.ml (library pcre2.matcher) so additional engines
   (pcre2.fast) can share them. This include keeps the frozen src/pcre2.mli
   references (Intf.Matcher, Intf.substitution, ...) resolving unchanged. *)
include Pcre2_matcher.Intf
