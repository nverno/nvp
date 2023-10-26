;; -*- lexical-binding: t; -*-
(require 'nvp-test-helpers)
(require 'nvp-hippie)

(ert-deftest nvp-he-flex-camel/snake ()
  "Test default camel/snake matcher."
  (let (case-fold-search)
    (cl-labels ((fn
                 (prefix str)
                 (string-match-p (nvp-he-flex-camel/snake prefix) str)))
      (should (fn "sS" "setState"))
      (should (fn "setS" "setState"))
      (should (fn "this.sS" "this.setState"))
      (should (fn "this.sSta" "this.setState"))
      (should (fn "th.seSta" "this.setState"))
      (should (fn "t.sS" "this.setState"))
      (should (fn "F.sS" "F.snotSnuff.call"))
      (should (fn "f_b" "foo_bar"))
      (should (fn "f_B" "foo_Bar"))
      (should-not (fn "f_B" "foo_bar"))
      (should-not (fn "sS" "StateSet"))
      (should-not (fn "sS" "State.set"))
      (should-not (fn "s.S" "State.set"))
      (should-not (fn "s.S" "SsState.Set"))
      (should-not (fn "t.sS" "setState"))
      (should-not (fn "th.seSSta" "this.setState"))
      (should-not (fn "t.Ss" "this.setState")))))

(provide 'nvp-hippie-tests)
