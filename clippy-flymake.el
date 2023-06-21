;;; flymake-clippy.el --- Flymake backend for Clippy  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Flymake backend for Clippy, the Rust linter.
;; https://doc.rust-lang.org/stable/clippy/index.html

;;; Code:

(require 'cl-lib)

;; Capture group source example:
;; "warning: ..."
;;    --> src/filename.rs
;; 98 | ...
(defun clippy-flymake--build-regexp ()
  "Create a regular expression to search Clippy warnings for FILENAME."
  (rx (seq line-start
           ;; Message
           (group "warning:"
                  (zero-or-more nonl))
           "\n"
           (zero-or-more nonl)
           "--> "
           ;; File
           (group
            (zero-or-more nonl))
           ":"
           ;; Line
           (group
            (one-or-more
             (any "0-9")))
           ":"
           ;; Col
           (group
            (one-or-more
             (any "0-9")))
           line-end)))

(defvar-local clippy-flymake--proc nil
  "Clippy subprocess object, used to ensure obsolete processes aren't reused.")

(defun clippy-flymake-backend (report-fn &rest _args)
  "Flymake backend for Clippy, the Rust linter. Calls REPORT-FN with a
list of Flymake diagnostics for the current buffer.

Use `clippy-flymake-setup-backend' to register the backend
with the appropriate Flymake hook."
  (unless (executable-find "cargo")
    (error "Cannot find cargo"))

  (let* ((source (current-buffer))
         (filename (buffer-file-name source)))
    (save-restriction
      (widen)
      (setq clippy-flymake--proc
            (make-process
             :name "clippy-flymake" :noquery t :connection-type 'pipe
             :buffer (generate-new-buffer "*clippy-flymake*")
             :command '("cargo" "clippy")
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     (if (with-current-buffer source (eq proc clippy-flymake--proc))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           ;; Collect output buffer into diagnostic messages/locations,
                           ;; exposing them via `report-fn'.
                           (cl-loop
                            while (search-forward-regexp
                                   (clippy-flymake--build-regexp)
                                   nil t)
                            for msg = (match-string 1)
                            for sourcefile = (match-string 2)
                            for (beg . end) = (flymake-diag-region
                                               source
                                               (string-to-number (match-string 3)))
                            for type = (if (string-match "^warning" msg)
                                           :warning
                                         :error)
                            when (and sourcefile (string-match-p sourcefile filename))
                            collect (flymake-make-diagnostic source beg end type msg)
                            into diags
                            finally (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s" proc))
                   ;; Cleanup temp buffer.
                   (kill-buffer (process-buffer proc)))))))
      (process-send-region clippy-flymake--proc (point-min) (point-max))
      (process-send-eof clippy-flymake--proc))))

(defun clippy-flymake-setup-backend ()
  "Add `clippy-flymake' to `flymake-diagnostic-functions' hook."
  (add-hook 'flymake-diagnostic-functions #'clippy-flymake-backend nil t))

(provide 'clippy-flymake)
;;; clippy-flymake.el ends here
