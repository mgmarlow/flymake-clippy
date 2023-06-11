.PHONY: build clean

build: clean
	emacs -batch -L . -f batch-byte-compile clippy-flymake.el

clean:
	rm -f clippy-flymake.elc
