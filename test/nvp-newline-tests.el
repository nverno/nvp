(require 'nvp-test-helpers)
(require 'nvp-c)
(require 'nvp-c++)

(defun c-mode-buffer-setup ()
  (setq c-basic-offset 4))

(defun c++-mode-buffer-setup ()
  (setq c-basic-offset 4))

;; -------------------------------------------------------------------
;;; Lisp neline dwim => doesn't do anything special

(ert-deftest el-newline-dwim-code ()
  "Newline dwim in elisp code."
  :tags '(:newline)
  (nvp--buffer-should-change
    "
(|)"
    "
(
 |)" nil
    (call-interactively 'nvp-newline-dwim)))

(ert-deftest el-newline-dwim-string ()
  "Newline dwim in elisp string."
  :tags '(:newline)
  (nvp--buffer-should-change
    "
\"(|)\""
    "
\"(
|)\"" nil (call-interactively 'nvp-newline-dwim)))

(ert-deftest el-newline-dwim-comment ()
  "Newline dwim in elisp comment."
  :tags '(:newline)
  (nvp--buffer-should-change
    "
;; (|)"
    "
;; (
|)" nil (call-interactively 'nvp-newline-dwim)))

(ert-deftest el-newline-dwim-braces ()
  "Newline dwim in elisp braces."
  :tags '(:newline)
  (nvp--buffer-should-change
    "
(list {|})"
    "
(list {
      |})" nil (call-interactively 'nvp-newline-dwim)))

;; -------------------------------------------------------------------
;;; C/C++
;; - Should newline+indent in code braces
;; - TODO: Should insert escape in newline in strings
;; - Shouldn't continue // style comments
;; - Should insert comment continuation in /* style comments

(ert-deftest c-newline-dwim-braces ()
  "Newline dwim in C braces."
  :tags '(:newline :c-mode)
  (nvp--buffer-should-change
    "
int main() {|}"
    "
int main() {
    |
}" 'c-mode (call-interactively 'nvp-newline-dwim)))

;; (ert-deftest c-newline-dwim-string ()
;;   "Newline dwim in C string."
;;   :tags '(:newline :c-mode)
;;   (nvp--buffer-should-change
;;     "
;; \"(|)\""
;;     "
;; \"(
;; |)\"" 'c-mode (call-interactively 'nvp-newline-dwim)))

(ert-deftest c-newline-dwim-comment-no-continue ()
  "Newline dwim in C single-line comment."
  :tags '(:newline :c-mode)
  (nvp--buffer-should-change
    "
// (|)"
    "
// (
|)" 'c-mode (call-interactively 'nvp-newline-dwim)))

(ert-deftest c-newline-dwim-comment-no-continue ()
  "Newline dwim in C++ single-line comment."
  :tags '(:newline :c++-mode)
  (nvp--buffer-should-change
    "
// (|)"
    "
// (
|)" 'c++-mode (call-interactively 'nvp-newline-dwim)))

(ert-deftest c-newline-dwim-comment-continue ()
  "Newline dwim in C continued comment."
  :tags '(:newline :c-mode)
  (nvp--buffer-should-change
    "
/* | */"
    "
/* 
 * | */" 'c-mode (call-interactively 'nvp-newline-dwim)))

(ert-deftest c++-newline-dwim-comment-continue ()
  "Newline dwim in C continued comment."
  :tags '(:newline :c++-mode)
  (nvp--buffer-should-change
    "
/* | */"
    "
/* 
 * | */" 'c++-mode (call-interactively 'nvp-newline-dwim)))
