(* Data-line (subject) escape processing, ported from pcre2test.c
   process_data() (pcre2test.c:6971-7245), 8-bit mode.

   Input: the raw data line already stripped of trailing whitespace, and the
   index of the first non-space character. Output: the subject byte string
   plus, when the string was terminated by \=, the index where the modifier
   text begins. Fatal errors emit their message and return None (the C
   returns PR_OK without matching). *)

type result = {
  subject : string;
  mods_from : int option; (* index into the original line, after \= *)
}

let decode ~utf ~subject_literal ~(emit : string -> unit) (s : string)
    ~(start : int) ~(len : int) : result option =
  (* len = effective length of s (trailing whitespace stripped) *)
  let at i = Cstr.at ~limit:len s i in
  let q = Buffer.create (len + 8) in
  let start_rep = ref (-1) in
  let p = ref start in
  let mods_from = ref None in
  let fatal = ref false in
  let fail msg =
    emit msg;
    fatal := true
  in
  (try
     while not (!fatal || at !p = '\000') do
       let c = ref (Char.code (at !p)) in
       incr p;
       (* ] may mark the end of a replicated sequence (pcre2test.c:6982) *)
       if !c = Char.code ']' && !start_rep >= 0 then begin
         if at !p <> '{' then fail "** Expected '{' after \\[....]"
         else begin
           incr p;
           match Modifiers.parse_s32 at !p with
           | None -> fail "** Repeat count too large"
           | Some (li, j) ->
               p := j;
               if at !p <> '}' then fail "** Expected '}' after \\[...]{..."
               else begin
                 incr p;
                 if li <= 0 then fail "** Zero or negative repeat not allowed"
                 else begin
                   let rep = Buffer.sub q !start_rep (Buffer.length q - !start_rep) in
                   for _k = 2 to li do
                     Buffer.add_string q rep
                   done;
                   start_rep := -1
                 end
               end
         end
       end
       else begin
         let store = ref true in
         (* Handle a non-escaped character (pcre2test.c:7057-7069) *)
         if !c <> Char.code '\\' || subject_literal then begin
           if utf && !c >= 0xc0 then begin
             (* GETUTF8INC (pcre2_internal.h:305-338); input was validated *)
             let rc, v = Pchars.utf82ord ~limit:len s (!p - 1) in
             if rc > 0 then begin
               c := v;
               p := !p - 1 + rc
             end
           end
         end
         else begin
           (* Handle backslash escapes (pcre2test.c:7071-7167) *)
           let e = Char.code (at !p) in
           incr p;
           c := e;
           match Char.chr e with
           | '\\' -> ()
           | 'a' -> c := 7
           | 'b' -> c := 8
           | 'e' -> c := 27
           | 'f' -> c := 12
           | 'n' -> c := 10
           | 'r' -> c := 13
           | 't' -> c := 9
           | 'v' -> c := 11
           | '0' .. '7' ->
               c := e - Char.code '0';
               let i = ref 0 in
               while
                 !i < 2
                 && Cstr.isdigit (at !p)
                 && at !p <> '8'
                 && at !p <> '9'
               do
                 c := (!c * 8) + (Char.code (at !p) - Char.code '0');
                 incr i;
                 incr p
               done
           | 'o' ->
               if at !p = '{' then begin
                 let pt = ref (!p + 1) in
                 c := 0;
                 let i = ref 0 in
                 while Cstr.isdigit (at !pt) && at !pt <> '8' && at !pt <> '9' do
                   incr i;
                   (* the C skips only digit #12 itself (pcre2test.c:7101-7105) *)
                   if !i = 12 then
                     emit
                       "** Too many octal digits in \\o{...} item; using only \
                        the first twelve."
                   else
                     (* uint32 arithmetic in the C: wrap at 32 bits *)
                     c :=
                       ((!c * 8) + (Char.code (at !pt) - Char.code '0'))
                       land 0xFFFFFFFF;
                   incr pt
                 done;
                 if at !pt = '}' then p := !pt + 1
                 else emit "** Missing } after \\o{ (assumed)"
               end
           | 'x' ->
               let handled = ref false in
               if at !p = '{' then begin
                 let pt = ref (!p + 1) in
                 let v = ref 0 in
                 let i = ref 0 in
                 while Cstr.isxdigit (at !pt) do
                   incr i;
                   (* the C skips only digit #9 itself (pcre2test.c:7126-7130) *)
                   if !i = 9 then
                     emit
                       "** Too many hex digits in \\x{...} item; using only \
                        the first eight."
                   else
                     (* uint32 arithmetic in the C: wrap at 32 bits *)
                     v := ((!v * 16) + Cstr.hexval (at !pt)) land 0xFFFFFFFF;
                   incr pt
                 done;
                 if at !pt = '}' then begin
                   c := !v;
                   p := !pt + 1;
                   handled := true
                 end
                 (* else: not correct form for \x{...}; fall through *)
               end;
               if not !handled then begin
                 (* \x without {} defines one byte in 8-bit mode
                    (pcre2test.c:7148-7166) *)
                 c := 0;
                 let i = ref 0 in
                 while !i < 2 && Cstr.isxdigit (at !p) do
                   c := (!c * 16) + Cstr.hexval (at !p);
                   incr i;
                   incr p
                 done;
                 if utf then begin
                   (* just copy the byte in UTF-8 mode *)
                   Buffer.add_char q (Char.chr !c);
                   store := false
                 end
               end
           | '\000' ->
               (* \ followed by EOF allows for an empty line *)
               decr p;
               store := false
           | '=' ->
               (* \= terminates the data, starts modifiers *)
               mods_from := Some !p;
               raise Exit
           | '[' ->
               if !start_rep >= 0 then begin
                 fail "** Nested replication is not supported";
                 store := false
               end
               else begin
                 start_rep := Buffer.length q;
                 store := false
               end
           | ch ->
               if Cstr.isalnum ch then begin
                 fail (Printf.sprintf "** Unrecognized escape sequence \"\\%c\"" ch);
                 store := false
               end
         end;
         (* Store the code point (pcre2test.c:7169-7207, 8-bit arm) *)
         if !store && not !fatal then begin
           if utf then begin
             if !c > 0x7fffffff then
               fail
                 (Printf.sprintf
                    "** Character \\x{%x} is greater than 0x7fffffff and so \
                     cannot be converted to UTF-8"
                    !c)
             else Buffer.add_string q (Pchars.ord2utf8 !c)
           end
           else begin
             if !c > 0xff then begin
               emit
                 (Printf.sprintf
                    "** Character \\x{%x} is greater than 255 and UTF-8 mode \
                     is not enabled."
                    !c);
               emit "** Truncation will probably give the wrong result."
             end;
             Buffer.add_char q (Char.chr (!c land 0xff))
           end
         end
       end
     done
   with Exit -> ());
  if !fatal then None
  else Some { subject = Buffer.contents q; mods_from = !mods_from }
