.PHONY: all clean doc test test-conformance

all:
	dune build @install

# Runs the Alcotest unit suites: the engine per-module module-init asserts,
# the pcre2 functor suite (Interp + Jit), the C-oracle drift run, and the
# pcre2test harness unit tests — everything wired onto the default @runtest.
test:
	dune runtest

# Heavier differential suites kept off the default runtest.
test-conformance:
	dune exec test/conformance/runner.exe

clean:
	dune clean

doc:
	dune build @doc
