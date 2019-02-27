(require 'ert)
(require 'nvp-test-helpers)
(require 'nvp)
(require 'nvp-basic)

;;; Newline dwim tests
;; lisp doesn't do anything special
(ert-deftest el-newline-dwim-code ()
  (nvp--buffer-should-change
    "
(|)"
    "
(
 |)" nil
  (call-interactively 'nvp-newline-dwim)))

(ert-deftest el-newline-dwim-string ()
  (nvp--buffer-should-change
    "
\"(|)\""
    "
\"(
|)\"" nil (call-interactively ' nvp-newline-dwim)))

(ert-deftest el-newline-dwim-comment ()
  (nvp--buffer-should-change
    "
;; (|)"
    "
;; (
|)" nil (call-interactively ' nvp-newline-dwim)))

(ert-deftest el-newline-dwim-braces ()
  (nvp--buffer-should-change
    "
(list {|})"
    "
(list {
      |})" nil (call-interactively ' nvp-newline-dwim)))

(provide 'test-newline-dwim)
