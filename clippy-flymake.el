;;; flymake-clippy.el --- Flymake backend for Clippy  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <mgmarlow@Grahams-Mac-mini.local>
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

(defun clippy-flymake (report-fn &rest _args)
  "Flymake backend for cargo clippy."
  (unless (executable-find "cargo")
    (error "Cannot find cargo"))

  (let* ((source (current-buffer))
         (filename (file-name-nondirectory (buffer-file-name source))))
    (save-restriction
      (widen)
      (setq clippy--flymake-proc
            (make-process
             :name "clippy-flymake" :noquery t :connection-type 'pipe
             :buffer (generate-new-buffer "*clippy-flymake*")
             :command '("cargo" "clippy")
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     (if (with-current-buffer source (eq proc clippy--flymake-proc))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           ;; Collect output buffer into diagnostic messages/locations,
                           ;; exposing them via `report-fn'.
                           (cl-loop
                            while (search-forward-regexp
                                   ;; Capture group source example:
                                   ;; "warning: ..."
                                   ;;    --> src/filename.rs
                                   ;; 98 | ...
                                   (concat "^\\(warning:.*\\)\n\\(.*" filename "\\):\\([0-9]+\\):\\([0-9]+\\)$")
                                   nil t)
                            for msg = (match-string 1)
                            for (beg . end) = (flymake-diag-region
                                               source
                                               (string-to-number (match-string 3)))
                            for type = (if (string-match "^warning" msg)
                                           :warning
                                         :error)
                            collect (flymake-make-diagnostic source beg end type msg)
                            into diags
                            finally (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s" proc))
                   ;; Cleanup temp buffer.
                   (kill-buffer (process-buffer proc)))))))
      (process-send-region clippy--flymake-proc (point-min) (point-max))
      (process-send-eof clippy--flymake-proc))))

(defun clippy-flymake-setup-backend ()
  "Add `clippy-flymake' to `flymake-diagnostic-functions' hook."
  (add-hook 'flymake-diagnostic-functions #'clippy-flymake nil t))

(provide 'flymake-clippy)
;;; flymake-clippy.el ends here
