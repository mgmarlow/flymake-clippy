# clippy-flymake

A Flymake backend for [Clippy](https://doc.rust-lang.org/stable/clippy/index.html), the Rust linter.

## Instructions

You probably want to install [rust-mode](https://github.com/rust-lang/rust-mode) first.

With Emacs 29 and [vc-use-package](https://github.com/slotThe/vc-use-package):

```elisp
(use-package clippy-flymake
  :vc (:fetcher sourcehut :repo mgmarlow/clippy-flymake)
  :hook (rust-mode . clippy-flymake-setup-backend))
```

With Emacs 30:

``` elisp
(use-package clippy-flymake
  :vc (:url "https://git.sr.ht/~mgmarlow/clippy-flymake" :branch main)
  :hook (rust-mode . clippy-flymake-setup-backend))
```

Alternatively, clone the repo and update your load path:

```
git clone https://git.sr.ht/~mgmarlow/clippy-flymake /path/to/clippy-flymake
```

```elisp
(add-to-list 'load-path "/path/to/clippy-flymake")
(require 'clippy-flymake)
```

### Eglot users

Eglot users require [a little extra setup](https://github.com/joaotavora/eglot/issues/268) to enable running multiple Flymake backends simultaneously. Add the following to your Emacs config:

```elisp
;; Instruct Eglot to stop managing Flymake
(add-to-list 'eglot-stay-out-of 'flymake)

;; Manually re-enable Eglot's Flymake backend
(defun manually-activate-flymake ()
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

(add-hook 'eglot-managed-mode-hook #'manually-activate-flymake nil t)
```

(Nb. prior to eglot 1.6, this hook was called `eglot--managed-mode-hook)

You can confirm that Flymake is running correctly by opening up a Rust buffer and examining `flymake-running-backends':

```
M-x flymake-running-backends

Running backends: clippy-flymake-backend, eglot-flymake-backend
```

### Complete eglot + rust-mode + use-package example

Prior to emacs 30 for the `:vc` specification, and prior to eglot 1.6 for the minor mode hook:

```elisp
(use-package rust-mode
  :ensure t)

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

Emacs 30, eglot 1.6+:

```elisp
(use-package rust-mode
  :ensure t)

(use-package clippy-flymake
  :vc (:url "https://git.sr.ht/~mgmarlow/clippy-flymake" :branch main)
  :hook (rust-mode . clippy-flymake-setup-backend))

(defun manually-activate-flymake ()
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)
         (eglot-managed-mode . manually-activate-flymake))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake))
```

## Contributing

Please direct bug reports or patches to the [the mailing list](https://lists.sr.ht/~mgmarlow/public-inbox).

## License

Licensed under [GPL-3.0](./LICENSE).
