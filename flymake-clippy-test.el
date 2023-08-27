;;; flymake-clippy-test.el   -*- lexical-binding: t; -*-

(require 'flymake-clippy)
(require 'ert)

(defmacro with-text (text &rest body)
  `(with-temp-buffer
    (insert ,text)
    (goto-char 0)
    ,@body))

(defun run-regexp ()
  (set-match-data nil)
  (search-forward-regexp (flymake-clippy--build-regexp) nil t)
  (list (match-string 1)
        (match-string 2)
        (match-string 3)))

(defvar warning-text
"warning: unused variable: `user`
  --> src/database/foo.rs:42:9
   |
42 |         user: &User,
   |         ^^^^ help: if this is intentional, prefix it with an underscore: `_user`
   |
   = note: `#[warn(unused_variables)]` on by default")

(ert-deftest clippy-test-warnings ()
  "Tests warning diagnostics."
  (should (equal (with-text warning-text (run-regexp))
                 '("warning: unused variable: `user`" "src/database/foo.rs" "42"))))

(defvar error-text
"error: expected one of `!` or `::`, found foobar
  --> src/main.rs:20:9")

(defvar error-text-with-error-num
"error[E0407]: method `build_string` is not defined
  --> src/features.rs:106:5")

(ert-deftest clippy-test-errors ()
  "Tests error diagnostics."
  (should (equal (with-text error-text (run-regexp))
                 '("error: expected one of `!` or `::`, found foobar" "src/main.rs" "20")))
  (should (equal (with-text error-text-with-error-num (run-regexp))
                 '("error[E0407]: method `build_string` is not defined" "src/features.rs" "106"))))

(defvar multiline-text
"warning: unused variable: `user`
  --> src/database/foo.rs:42:9
   |
42 |         user: &User,
   |         ^^^^ help: if this is intentional, prefix it with an underscore: `_user`
   |
   = note: `#[warn(unused_variables)]` on by default

error: expected one of `!` or `::`, found foobar
  --> src/main.rs:20:9")

(ert-deftest clippy-test-multiline ()
  "Tests consecutive `run-regexp' calls."
  (should (equal (with-text multiline-text (run-regexp))
                 '("warning: unused variable: `user`" "src/database/foo.rs" "42")))
  (should (equal (with-text multiline-text (run-regexp) (run-regexp))
                 '("error: expected one of `!` or `::`, found foobar" "src/main.rs" "20"))))
