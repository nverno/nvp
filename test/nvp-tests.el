(require 'ert)
(require 'nvp-test-helpers)
(require 'nvp)
(require 'nvp)

;;; Newline dwim tests
;; lisp doesn't do anything special
(ert-deftest el-newline-dwim-code ()
  "Newline dwim in elisp code."
  :tags '(newline)
  (nvp--buffer-should-change
    "
(|)"
    "
(
 |)" nil
  (call-interactively 'nvp-newline-dwim)))

(ert-deftest el-newline-dwim-string ()
  "Newline dwim in elisp string."
  :tags '(newline)
  (nvp--buffer-should-change
    "
\"(|)\""
    "
\"(
|)\"" nil (call-interactively 'nvp-newline-dwim)))

(ert-deftest el-newline-dwim-comment ()
  "Newline dwim in elisp comment."
  :tags '(newline)
  (nvp--buffer-should-change
    "
;; (|)"
    "
;; (
|)" nil (call-interactively 'nvp-newline-dwim)))

(ert-deftest el-newline-dwim-braces ()
  "Newline dwim in elisp braces."
  :tags '(newline)
  (nvp--buffer-should-change
    "
(list {|})"
    "
(list {
      |})" nil (call-interactively 'nvp-newline-dwim)))

(provide 'nvp-tests)
