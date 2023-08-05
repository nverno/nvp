;;; nvp-find.el --- nvp-find-keymap; find stuff -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; - grep/rgrep/lgrep/zgrep
;; - shared vars bw ag and rg
;; - imenu support for ag and rg result buffers
;;
;; TODO: fix rg overrides to work with wgrep-rg
;;
;; * Interface
;; 
;; Defaults are determined in the following orders
;; - root: project root if known, otherwise `default-directory'.
;; - search term: region if active, symbol at point, prompt interactively
;; 
;; Prefix args:
;; - none: literal search
;; - single: regexp search
;; - double: prompt (literal search)
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:req 'nvp-find 'subrs)

(defvar nvp-search-history () "Store search history inputs.")

;; -------------------------------------------------------------------
;;; Ag/rg shared

(defconst nvp-ag/rg-grouped-file-regex
  "^\\(?:File:\\s-*\\)?\\([^ \t].*\\)$"
  "Support either `xterm-color' filtered results or defaults.")

;; `ag/file-column-pattern-group'
(defconst nvp-ag/rg-file-column-regex
  "^\\([[:digit:]]+\\):\\([[:digit:]]+\\):")

;; imenu that should work for ag/rg grouped results buffers
;; Note: override `rg-configure-imenu' to use w/ `imenu-default-create-index-function'
(defun nvp-ag/rg-imenu-function ()
  (cl-block nil
    (when (re-search-backward nvp-ag/rg-file-column-regex nil 'move)
      (beginning-of-line)
      (while (and (not (bobp))
                  (looking-at-p nvp-ag/rg-file-column-regex))
        (forward-line -1))
      (and (looking-at "^\\(?:File: \\)?\\([^ \t].*\\)$")
           (cl-return t)))))

(defvar nvp-ag/rg-imenu-expression '((nil nvp-ag/rg-imenu-function 1)))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-rgrep-dwim (root search &optional regex)
  (interactive (nvp:find-defaults current-prefix-arg "Rgrep search"))
  (require 'nvp-grep-config)
  (let ((default-directory root))
    (grep-compute-defaults)
    (rgrep (nvp:find-symbol search regex) "*.*" nil (nvp:prefix 16))))

(provide 'nvp-find)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-find.el ends here
