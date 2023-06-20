# clippy-flymake

A Flymake backend for [Clippy](https://doc.rust-lang.org/stable/clippy/index.html), the Rust linter.

## Instructions

Use with [rust-mode](https://elpa.nongnu.org/nongnu/rust-mode.html):

```elisp
(use-package clippy-flymake
  :vc (:fetcher sourcehut :repo mgmarlow/clippy-flymake)
  :hook (rust-mode . clippy-flymake-setup-backend))
```

## Eglot users

Eglot [fully manages Flymake](https://github.com/joaotavora/eglot/issues/268) so you'll need some extra code to make it cooperate:

```elisp
;; Instruct Eglot to stop managing Flymake
(add-to-list 'eglot-stay-out-of 'flymake)

;; Add the Eglot hook to Flymake diagnostic functions so we don't lose Eglot
;; functionality
(add-hook 'eglot--managed-mode-hook
          (lambda ()
            (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
            (flymake-mode 1)))
```

### Complete use-package example

```elisp
(use-package clippy-flymake
  :vc (:fetcher sourcehut :repo mgmarlow/clippy-flymake)
  :hook (rust-mode . clippy-flymake-setup-backend))

(defun manually-activate-flymake ()
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)
         (eglot--managed-mode . manually-activate-flymake))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake))
```

You can confirm that Flymake is running correctly by opening up a Rust buffer and examining `flymake-running-backends':

```
Running backends: clippy-flymake, eglot-flymake-backend
```
