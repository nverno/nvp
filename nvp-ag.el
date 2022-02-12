;;; nvp-ag.el --- ag mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Make ag, wgrep-ag, and xterm-color work together
;;
;; Problems:
;; Both ag.el and rg.el use the same sort of regex based interpretation of the
;; results in the compilation output buffer, and both break when using
;; `xterm-color'.
;;
;; ag.el is patched to work with xterm-color by overriding `ag-filter' to
;; insert "File: " before file paths - wgrep-ag relies on this pattern.
;;
;; `ag-filter' replaces escape sequences with 'File: ' by regex parsing
;; and then relies on that text to match `compilation-error-regexp's to jump
;; to error locations. However, this is fucked up by xterm-color's
;; `compilation-start-hook' which processes the string prior to passing it to
;; `compilation-filter', and then to `ag-filter'. It seems that
;; `xterm-color-filter' modifies the output so `ag-filter' no longer matches
;; and replaces text with 'File: ', hence the resulting compilation jumps
;; don't know the proper locations.
;;
;; See : #<marker at 24916 in ag.el>. ag.el doesn't support wgrep out-of-the-box
;;
;; Ag runs `shell-command' which invokes `shell-mode-hook' on the output
;; results. (`shell-command' advised to ignore `nvp-shell-mode-hook' in nvp.el)
;;
;; Fixes for compilation:
;; (1) Can either disable xterm-color
;; (define-advice ag/search (:around (orig-fn &rest args) "no-xterm-color")
;;   (let (compilation-start-hook)
;;     (apply orig-fn args)))
;; (2) Or override ag's `compilation-error-regexp-alist' matching function
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:req 'nvp-find 'subrs)
(require 'ag)
(require 'nvp)
(require 'nvp-find)
(nvp:decls)

(defconst nvp-ag-file-prefix "File: "
  "Prefix added by `ag-filter' to grouped matches.")

;; Match grouped filenames in rg/ag searches
(defun nvp-ag/compilation-match-grouped-filename ()
  (nvp:match-grouped-filename
   nvp-ag/rg-file-column-regex nvp-ag/rg-grouped-file-regex))

;; `ag-filter' adds "File: " based on escape codes that have already been
;; parsed by xterm-color. wgrep-ag relies on the "File: " prefix.
;; this overrides `ag-filter' to add the "File: " prefix where necessary
(defun nvp-ag-filter ()
  (save-excursion
    ;; skip past line-number:column-number lines
    (let (ok)
      (while (>= (point) compilation-filter-start)
        (forward-line 0)
        (setq ok nil)
        (while (and (>= (point) compilation-filter-start)
                    (looking-at-p ag/file-column-pattern-group))
          (setq ok (or ok t))
          (forward-line -1))
        ;; add "File: " prefix to group start
        (when (and ok (>= (point) compilation-filter-start)
                   (looking-at nvp-ag/rg-grouped-file-regex))
          (replace-match (concat nvp-ag-file-prefix (match-string 1)) t t))
        (forward-line -1)))))

(defun nvp-ag-setup-xterm ()
  (advice-add 'ag-filter :override #'nvp-ag-filter)
  (advice-add 'ag/compilation-match-grouped-filename
              :override #'nvp-ag/compilation-match-grouped-filename)
  (remove-hook 'ag-search-finished-hook #'nvp-ag-setup-xterm))

(defun nvp-ag-remove-xterm (&rest _)
  (advice-remove 'ag-filter #'nvp-ag-filter)
  (advice-remove 'ag/compilation-match-grouped-filename
                 #'nvp-ag/compilation-match-grouped-filename))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-ag-recompile ()
  "Recompile without xterm-color filter and original `ag-filter'."
  (interactive)
  (let ((compilation-start-hook
         (delq 'nvp-compilation-start-hook compilation-start-hook)))
    (add-hook 'ag-search-finished-hook #'nvp-ag-setup-xterm)
    (nvp-ag-remove-xterm)
    (call-interactively #'recompile)))

;;;###autoload
(defun nvp-ag-dwim (root search &optional regex)
  "Run ag search."
  (interactive (nvp:find-defaults current-prefix-arg "Ag search"))
  (unless (integerp current-prefix-arg) ; used as context=%d in ag/search
    (setq current-prefix-arg nil))
  (ag/search (nvp:find-symbol search regex) root :regexp t))

;;;###autoload
(defun nvp-ag-elisp-dwim (root search &optional regex)
  "Run ag search including `package-user-dir' from `user-emacs-directory'.
Ignore elpa directory by default, but with any prefix, prompt to include."
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (list nvp/emacs
           (nvp:find-search-term "Ag elisp" (eq 16 arg))
           (eq 16 arg))))
  (let* ((elpa (nvp:path 'ds package-user-dir))
         (ag-ignore-list
          (nvp:prefix '>=4
            (if (y-or-n-p "Include elpa? ")
                (cl-callf2 cl-remove elpa ag-ignore-list :test #'string=)
              ag-ignore-list)
            (cl-pushnew elpa ag-ignore-list :test #'string=))))
    (unless (integerp current-prefix-arg)
      (setq current-prefix-arg nil))
    (ag/search (nvp:find-symbol search regex) root :regexp t)))

;;;###autoload
(defun nvp-ag-dired (arg)
  "Call ag dired functions.
(0) `ag-project-dired-regexp'
(1) `ag-project-dired'
(2) `ag-dired-regexp'
(3) `ag-dired'" 
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (call-interactively
   (pcase arg
     (`4  #'ag-project-dired)
     (`16 #'ag-dired-regexp)
     (`64 #'ag-dired)
     (_   #'ag-project-dired-regexp))))


(provide 'nvp-ag)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ag.el ends here
