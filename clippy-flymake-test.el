;;; clippy-flymake-test.el   -*- lexical-binding: t; -*-

(require 'clippy-flymake)
(require 'ert)

(defun run-regexp (filename)
  (set-match-data nil)
  (search-forward-regexp (clippy-flymake--build-regexp filename) nil t)
  (list (match-string 1)
        (match-string 2)
        (match-string 3)))

(ert-deftest clippy-test-regexp ()
  "Tests regexp matches diagnostic information."
  (should (equal (with-temp-buffer
                   (insert-file-contents "./test/fixture.txt")
                   (run-regexp "foo.rs"))
                 '("warning: unused variable: `user`" "  --> src/database/foo.rs" "42")))
  (should (equal (with-temp-buffer
                   (insert-file-contents "./test/fixture.txt")
                   (run-regexp "foo.rs")
                   (run-regexp "foo.rs"))
                 '("warning: using `clone` on type `Status` which implements the `Copy` trait" "  --> src/foo.rs" "31")))
  (should (equal (with-temp-buffer
                   (insert-file-contents "./test/fixture.txt")
                   (run-regexp "user.rs"))
                 '("warning: unused variable: `user`" "  --> src/foobar/user.rs" "42")))
  (should (equal (with-temp-buffer
                   (insert-file-contents "./test/fixture.txt")
                   (run-regexp "notfound.rs"))
                 '(nil nil nil))))
