.PHONY: build test clean

test: build
	emacs -batch -l ert -L . -l flymake-clippy-test.el -f ert-run-tests-batch-and-exit

build: clean
	emacs -batch -L . -f batch-byte-compile flymake-clippy.el

clean:
	rm -f flymake-clippy.elc
