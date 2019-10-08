;;; oat-mode.el --- Major mode for oat source -*- lexical-binding: t; -*-

;;; Commentary:

;; language-dependent variables:
;; - font-lock => cc-font
;; - rest      => cc-langs
;; see `c-basic-common-init' and `awk-mode'

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'cc-langs)
  (require 'cc-fonts))
(require 'cc-mode)

(defgroup oat nil
  "Major mode for editing Oat source files."
  :group 'languages
  :prefix "oat-mode-")

(defcustom oat-indent-offset 2
  "Amount by which expressions are indented."
  :type 'integer
  :group 'oat)

(eval-and-compile
  (c-add-language 'oat-mode 'c-mode))

;;; Font-lock

;; FIXME: modify types in `c-primitive-type-kwds' (cc-langs)
;; (c-lang-defconst )

;;;###autoload
(define-derived-mode oat-mode prog-mode "Oat"
  "Major mode for editing oat files.
The hook `c-mode-common-hook' is run with no args at mode
initialization, then `oat-mode-hook'.

\\{oat-mode-map}"
  :after-hook (c-update-modeline)
  ;; indentation
  (setq c-basic-offset oat-indent-offset)
  (make-local-variable 'c-offsets-alist)
  ;; (c-set-offset 'knr-argdecl-intro 0)

  ;; initialize cc-mode stuff
  (c-initialize-cc-mode t)
  ;; java defaults are better match for `int[] var` syntax
  (c-init-language-vars-for 'java-mode)
  (c-common-init 'c-mode)
  (c-run-mode-hooks 'c-mode-common-hook)

  ;; remove auto hungries
  (c-toggle-electric-state -1)
  (c-toggle-auto-hungry-state -1)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state -1))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.oat\\'" . oat-mode))

(provide 'oat-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; oat-mode.el ends here
