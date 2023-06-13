# flymake-clippy

A Flymake backend for [Clippy](https://doc.rust-lang.org/stable/clippy/index.html), the Rust linter.

## Instructions

Use with [rust-mode](https://elpa.nongnu.org/nongnu/rust-mode.html):

```elisp
(use-package clippy-flymake
  :vc (:fetcher sourcehut :repo mgmarlow/clippy-flymake))

(add-hook 'rust-mode-hook #'clippy-flymake-setup-backend)
```

## Eglot users

Eglot [fully manages Flymake](https://github.com/joaotavora/eglot/issues/268) so you'll need some extra code to make it cooperate:

```elisp
(add-to-list 'eglot-stay-out-of 'flymake)

(add-hook 'eglot--managed-mode-hook
          (lambda ()
            (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)))
```
