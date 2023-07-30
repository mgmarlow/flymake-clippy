;;; flymake-clippy-test.el   -*- lexical-binding: t; -*-

(require 'flymake-clippy)
(require 'ert)

(defun run-regexp ()
  (set-match-data nil)
  (search-forward-regexp (flymake-clippy--build-regexp) nil t)
  (list (match-string 1)
        (match-string 2)
        (match-string 3)))

(ert-deftest clippy-test-regexp ()
  "Tests regexp matches diagnostic information."
  (should (equal (with-temp-buffer
                   (insert-file-contents "./test/fixture.txt")
                   (run-regexp))
                 '("warning: unused variable: `user`" "src/database/foo.rs" "42")))
  (should (equal (with-temp-buffer
                   (insert-file-contents "./test/fixture.txt")
                   (run-regexp)
                   (run-regexp))
                 '("warning: using `clone` on type `Status` which implements the `Copy` trait" "src/foo.rs" "31")))
  (should (equal (with-temp-buffer
                   (insert-file-contents "./test/fixture.txt")
                   (run-regexp)
                   (run-regexp)
                   (run-regexp))
                 '("warning: unused variable: `user`" "src/foobar/user.rs" "42"))))
