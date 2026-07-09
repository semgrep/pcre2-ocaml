# Upstream PCRE2 bugs found by differential fuzzing

Three out-of-bounds-read bugs in PCRE2 (confirmed against the 10.44 release
sources; two re-confirmed live in 10.46) surfaced while differential-fuzzing a
pure-OCaml reimplementation against the C library as an oracle. All three are
**reads before or past the subject buffer** driven by UTF-8 continuation-byte
walks that lack a lower/upper bound the surrounding code assumes. Each is
reachable in the **8-bit** library (bug 2 also 16-bit) from ordinary API calls;
none requires `PCRE2_NO_UTF_CHECK` — `PCRE2_MATCH_INVALID_UTF`, or in one case
plain `PCRE2_UTF` with a start offset, is enough.

In the reporting project these are pinned to defined behavior in the pure
engine and mirrored by dev-only patches applied *only* to the fuzzing oracle's
private libpcre2 build (`oracle/patches/`); the vendored 10.44 tree is never
modified. This document is the write-up for reporting them upstream.

Common shape: PCRE2's `BACKCHAR` macro (8-bit) is an **unbounded** reverse walk

```c
#define BACKCHAR(eptr) while((*eptr & 0xc0u) == 0x80u) eptr--
```

(`pcre2_intmodedep.h`). It is correct only when a non-continuation byte
(equivalently, the subject start) is guaranteed to sit at or above the walk's
starting point. Three call sites violate that guarantee once an all-
continuation-byte prefix can appear below the position being decoded.

---

## Bug 1 — `PRIV(was_newline)`: `BACKCHAR` reads before the subject start

**Location:** `pcre2_newline.c` (the `BACKCHAR(ptr)` at the top of
`PRIV(was_newline)`, ~line 178 in 10.44).
**Build:** 8-bit and 16-bit (UTF). **Confirmed:** 10.44, still present in 10.46.

### Mechanism
`WAS_NEWLINE(p)` is guarded by callers against reading below
`mb->start_subject` (the true subject start). Under
`PCRE2_MATCH_INVALID_UTF`, the matcher's *bad-start skip*
(`pcre2_match.c:6829-6851`) advances `start_match` past invalid leading code
units, but the startline bump-along scan and `OP_CIRCM` then call
`WAS_NEWLINE(start_match)`, and `was_newline`'s own `BACKCHAR(ptr)` (with
`ptr = start_match - 1`) walks backwards over continuation bytes with **no**
lower bound. If every byte from `mb->start_subject` up to `start_match - 1` is a
UTF-8 continuation byte (`0x80..0xBF`), the walk steps to `subject[-1]` and
reads unowned memory.

### Minimal reproducer (validated; ASan faults at `pcre2_newline.c:178`)
`pcre2test` input:
```
/(*LIMIT_MATCH=80000)()()((()()))/multiline,match_invalid_utf,match_line,newline=anycrlf
    \xb9\x74\=offset=0
```
Essential ingredients (the empty groups above are incidental — they came from
the fuzzer): `PCRE2_MATCH_INVALID_UTF`, a subject whose **first byte is a UTF-8
continuation byte** (here `0xB9`), and a match that reaches `WAS_NEWLINE` at the
first valid position after the bad-start skip (`match_line`/`^` under multiline
forces the newline machinery). Under ASan the read faults; without ASan it
usually reads adjacent mapped memory and silently affects nothing, so the bug
is normally invisible.

### Suggested fix
Bound the walk at `mb->start_subject` (the same floor `OP_REVERSE` uses), or
have callers pass the floor into `was_newline`.

---

## Bug 2 — 8-bit/16-bit `GET_UCD`: unbounded UCD table index for out-of-range code points

**Location:** `pcre2_internal.h`, the non-32-bit `GET_UCD` macro (~line 1870 in
10.44); triggered via `pcre2_xclass.c` (`GET_UCD(c)` at ~line 137).
**Build:** 8-bit and 16-bit (UTF). **Confirmed:** 10.44, still present in 10.46.

### Mechanism
The 32-bit library guards the Unicode property lookup:
```c
#define GET_UCD(ch) ((ch > MAX_UTF_CODE_POINT)? PRIV(dummy_ucd_record) : REAL_GET_UCD(ch))
```
but the 8-bit/16-bit builds do **not**:
```c
#define GET_UCD(ch) REAL_GET_UCD(ch)   /* no bounds check */
```
`REAL_GET_UCD(ch)` indexes `PRIV(ucd_stage1)[ch / 128]`. Under
`PCRE2_MATCH_INVALID_UTF` (or `PCRE2_NO_UTF_CHECK`), an invalid lead byte is
decoded to an out-of-range code point: e.g. `GETUTF8INC` on `0xFF` takes its
6-byte-lead branch and yields a code point `>= 0x40000000`. When such a
character reaches a `\p{...}` / `[...\p...]` test, the `OP_XCLASS` path calls
`GET_UCD(c)` with `c ≈ 0x60000000`, and `ucd_stage1[c/128]` reads ~25 MB past
the static table — usually mapped garbage, occasionally an unmapped page (a rare
`SIGSEGV` inside `libpcre2-8`).

### Minimal reproducer (validated; ASan faults at `pcre2_xclass.c:137`)
`pcre2test` input (hand-minimized from the fuzzer find; verified against an
ASan build of the vendored 10.44 sources):
```
/(?<=h{,2}\p{Greek})/match_invalid_utf
    \xff\x60\=offset=0
```
`0xFF` decodes to `c ≈ 0x60000000`; the `\p{Greek}` reaches the `OP_XCLASS` UCD
lookup with that out-of-range code point. The essential ingredients are a class
compiled to `OP_XCLASS` carrying a `\p{...}` test and a subject with a `>= 0xF5`
lead byte (here reached inside a variable-length lookbehind, which is how the
fuzzer arrived at it, but the lookbehind is not essential to the OOB read
itself).

### Suggested fix
Apply the 32-bit build's guard to all widths: clamp / dummy-record before
`REAL_GET_UCD` when `ch > MAX_UTF_CODE_POINT`. (The reporting project clamps to
`MAX_UTF_CODE_POINT` rather than returning `dummy_ucd_record`, because the dummy
record's `bidiclass`/boolean-properties differ from U+10FFFF's; either is a
valid safety fix, but note that choice affects `\p{bidi...}` results on
out-of-range input.)

---

## Bug 3 — `OP_VREVERSE`: `BACKCHAR` reads before the subject in variable-length lookbehind

**Location:** `pcre2_match.c`, the `OP_VREVERSE` back-step (`Feptr--;
BACKCHAR(Feptr);`, ~lines 5854-5855 in 10.44).
**Build:** 8-bit and 16-bit (UTF). **Confirmed:** 10.44, still present in 10.46.

### Mechanism
`OP_VREVERSE` steps back one character per iteration with an unbounded
`BACKCHAR`. Its stop test (`Feptr == mb->start_subject`, ~line 5848) only
catches the case where the *step lands exactly* on the subject start; the
`BACKCHAR` walk *inside* a step has no floor. Whenever the code units from
`mb->start_subject` up to the step position are all continuation bytes, the walk
crosses below the subject. Three independent ways to arrange that prefix, none
excluded by any option:

1. **`PCRE2_MATCH_INVALID_UTF`** — the bad-start skip leaves an invalid prefix
   below where matching begins.
2. **Plain `PCRE2_UTF` with a start offset** — `valid_utf` validates only from
   `check_subject` (`pcre2_match.c:6891`), so an all-continuation prefix *below*
   `check_subject` is never checked, and a **nested** lookbehind reaches it,
   because 10.44's `max_lookbehind` does not count nested lookbehinds
   ("A nested lookbehind does not contribute any length",
   `pcre2_compile.c:9604-9612`), so `check_subject = start_match -
   max_lookbehind` can sit above the real reach of an inner `OP_VREVERSE`.
3. **`PCRE2_NO_UTF_CHECK`** with a malformed subject.

Consequence: the lookbehind branch matches from `subject - 1`, reads unowned
memory as subject content, and a capturing group opened there records
`P->eptr - mb->start_subject == (PCRE2_SIZE)(-1) == PCRE2_UNSET`
(`pcre2_match.c:6081`) into the ovector — callers get an "unset" start with a
set end, and `pcre2test`'s substring printer indexes `subject[-1]`.

### Minimal reproducers (validated against real `pcre2test`)
Invalid-UTF route (unpatched oracle returns group 1 = `(-1, 1)` and prints the
byte it read at `subject[-1]`):
```
/(?<=(.)?)/match_invalid_utf
    \x80\=offset=0
```
Plain-UTF nested route (all reads that C makes are otherwise in bounds; the only
out-of-bounds read is the `BACKCHAR` under-run):
```
/(?<=(?<=(a{3,5}))a)/utf
    \x80\x80aaaaaa\=offset=7
```

### A subtlety worth flagging to upstream
The obvious "floor the walk at `mb->check_subject`" fix (mirroring `OP_REVERSE`)
is **wrong**: it changes *defined* behavior. Because `max_lookbehind` undercounts
nested lookbehinds, an inner `OP_VREVERSE` legitimately walks below
`check_subject` on fully valid data — e.g. `/(?<=(?<=(a{3,4}))a)/` in UTF mode
on `"aaaaaa"` at offset 5 has `check_subject = 1`, and real 10.44 correctly
walks the inner lookbehind back to offset 0, yielding group 1 = `(0,4)`; a
`check_subject` floor would wrongly yield `(1,4)`. The correct fix bounds only
the `BACKCHAR` under-run at `mb->start_subject` and treats a step that would
cross below it as a failed back-step (the loop's existing too-few/cap path),
leaving all in-bounds behavior byte-identical. (The `max_lookbehind`
nested-undercount may itself deserve a separate look upstream.)

---

## Reporting status

Not yet filed with the PCRE2 project. Suggested channel: the PCRE2 mailing list
/ the maintainer's GitHub (`PhilipHazel/pcre2`). Each item above is
self-contained: file:line, mechanism, a `pcre2test` reproducer, and a fix
sketch. Bugs 1 and 3 are the same root cause (unbounded `BACKCHAR`) at two call
sites; bug 2 is the missing width-generalization of an existing 32-bit guard.
