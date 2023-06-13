.PHONY: build test clean

test: build
	emacs -batch -l ert -L . -l clippy-flymake-test.el -f ert-run-tests-batch-and-exit

build: clean
	emacs -batch -L . -f batch-byte-compile clippy-flymake.el

clean:
	rm -f clippy-flymake.elc
