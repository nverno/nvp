(require 'nvp-test-helpers)

;; -------------------------------------------------------------------
;;; Point

;;--- Comments

(ert-deftest point-start-of-comment-same-line ()
  "Find position of start of comment on line."
  :tags '(:point)
  (nvp--buffer-should-change
    "
(let| ((a 1)) ; abcs
  )"
    "
(let ((a 1)) |; abcs
  )" nil (goto-char (nvp-point 'csl))))

(ert-deftest point-end-of-comment-same-line-el ()
  "Find position of end of elisp comment on line."
  :tags '(:point)
  (nvp--buffer-should-change
    "
(let| ((a 1)) ; abcs
  )"
    "
(let ((a 1)) ; abcs|
  )" nil (goto-char (nvp-point 'cel))))

(ert-deftest point-end-of-comment-same-line-c ()
  "Find position of end of C comment on line."
  :tags '(:point)
  (nvp--buffer-should-change
    "
|     fprintf(stderr, /* a poo */ \"%\" OFFSET \"s\n\", \"a\");"
    "
     fprintf(stderr, /* a poo */| \"%\" OFFSET \"s\n\", \"a\");"
    'c-mode (goto-char (nvp-point 'cel))))

(ert-deftest point-start-of-multiline-comment-c ()
  "Find position of start of multiline C comment."
  :tags '(:point)
  (nvp--buffer-should-change
    "
/* 
| * Jah lives chilren
 */
int main(int argc, char *argv[]) {
    return 0;
}"
    "
|/* 
 * Jah lives chilren
 */
int main(int argc, char *argv[]) {
    return 0;
}"
    'c-mode (goto-char (nvp-point 'cs))))

(ert-deftest point-end-of-multiline-comment-c ()
  "Find position of end of multiline C comment."
  :tags '(:point)
  (nvp--buffer-should-change
    "
/* 
 * Jah| lives chilren
 */
int main(int argc, char *argv[]) {
    return 0;
}"
    "
/* 
 * Jah lives chilren
 */|
int main(int argc, char *argv[]) {
    return 0;
}"
    'c-mode (goto-char (nvp-point 'ce))))

;;--- Line positions

(ert-deftest point-end-of-logical-line-make ()
  "Find position of end of logical line in makefile - should pass escaped NLs."
  :tags '(:point)
  (nvp--buffer-should-change
    "
.depend:
	@for| f in $(EL); do                                                  \
	    sed -n                                                           \
		\"s/.*(require '\(${PKG}[^) ]*\).*).*$$/$${f}c: \1.elc/p\" $$f \
		>> .depend;                                                  \
	done"
    "
.depend:
	@for f in $(EL); do                                                  \
	    sed -n                                                           \
		\"s/.*(require '\(${PKG}[^) ]*\).*).*$$/$${f}c: \1.elc/p\" $$f \
		>> .depend;                                                  \
	done|"
    'makefile-gmake-mode (goto-char (nvp-point 'eoll))))

(provide 'nvp-tests)
