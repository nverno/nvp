;;; nvp-find.el --- nvp-find-keymap; find stuff -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Finding files + things in files:
;; - recentf
;; - grep/rgrep/lgrep/zgrep
;; - ag
;; - rgrep
;; - wgrep (ag, ripgrep, grep)
;; - adds imenu support for ag and rg result buffers
;;
;; Searching from project roots is preferred, and the interface tries to
;; follow the same as searching functions in projectile.
;; However, projectile uses ripgrep, whereas I use rg, so that needs to be 
;; wrapped.
;; Additionally, both ag.el and rg.el use the same sort of regex based
;; interpretation of the results in the compilation output buffer, which
;; are both broken when using `xterm-color' (which is both nicer and faster IMO).
;; So, wrappers are added to make both libraries work (with some minor features
;; lost in the case of rg.el) with `xterm-color'.
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
(require 'nvp-display)
(require 'nvp)
(nvp-decls :f (wgrep-exit
               wgrep-save-all-buffers wgrep-abort-changes
               wgrep-remove-change wgrep-remove-all-change
               wgrep-toggle-readonly-area wgrep-mark-deletion
               wgrep-change-to-wgrep-mode
               ;; ag
               ag-project-dired ag-dired-regexp ag-dired ag-project-dired-regexp
               ag/search
               ;; rg
               rg-filter)
           :v (rg-mode-hook             ; rg
               ;; ag
               ag/file-column-pattern-group ag-ignore-list))

(defvar nvp-search-history () "Store search history inputs.")

;; -------------------------------------------------------------------
;;; wgrep

;;; XXX: remove this?
;;;###autoload(autoload 'nvp-wgrep-hydra/body "nvp-find")
(nvp-hydra-set-property 'nvp-wgrep-hydra)
(defhydra nvp-wgrep-hydra (:color red)
  ("q" wgrep-exit "exit" :exit t)
  ("s" wgrep-save-all-buffers "save all")
  ("a" wgrep-abort-changes "abort" :exit t)
  ("r" wgrep-remove-change "remove region change")
  ("R" wgrep-remove-all-change "remove all changes")
  ("t" wgrep-toggle-readonly-area "toggle r/o")
  ("m" wgrep-mark-deletion "mark deletion"))

;; -------------------------------------------------------------------
;;; Recentf 

;; Find a recent file using ido completion, only abbreviating
;; filenames.
;;;###autoload
(defun nvp-find-recentf (action)
  "Find recentf file, displaying with ACTION."
  (interactive "P")
  (if (not (bound-and-true-p recentf-mode))
      (recentf-mode))
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
	            (cons (abbreviate-file-name x) x))
	          recentf-list))
	 (filename-list
	  (cl-remove-duplicates (mapcar #'car file-assoc-list) :test #'string=))
	 (filename (nvp-completing-read "Recent File: " filename-list nil t)))
    (when filename
      (nvp-display-location (cdr (assoc filename file-assoc-list))
                            :file (car action)))))


;; -------------------------------------------------------------------
;;; Determine defaults for ag/rg/rgrep

(eval-when-compile
  ;; get thing to search for: region, symbol-at-point, or prompt
  (defsubst nvp-find--search-term (search-prompt &optional force-prompt)
    (let ((search-term
           (--if-let (nvp-tap 'dwim)
               (if force-prompt
                   (read-from-minibuffer
                    (format (concat search-prompt " (%s): ") it) it)
                 it)
             (read-from-minibuffer (concat search-prompt ": ")))))
      (add-to-history 'nvp-search-history search-term)
      search-term))

  ;; determine the search root directory
  (defsubst nvp-find--seach-root (&optional root prompt)
    (or root
        (and prompt (read-directory-name "Search root: "))
        (--if-let (nvp-project-root) it
          (read-directory-name
           (format "Search root ('%s'):" (file-name-directory default-directory))))))

  ;; return (search-root search-term regexp-p)
  (defsubst nvp-find--defaults (arg search-prompt &optional root)
    (let ((prompt (equal '(16) arg)))
      (list (nvp-find--seach-root root prompt)
            (nvp-find--search-term search-prompt prompt)
            (if prompt (y-or-n-p "Use regex?") (equal '(4) arg))))))


;; -------------------------------------------------------------------
;;; Ag / Ripgrep (using rg.el)

;; Notes pertinent to both ag.el and rg.el
;; ---------------------------------------
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
;; results. Since I have so much config in my `nvp-shell-mode-hook', `shell-command'
;; is advised to run without my hook in nvp.el.
;;
;; Fixes for compilation:
;; (1) Can either disable xterm-color, which sucks
;; (define-advice ag/search (:around (orig-fn &rest args) "no-xterm-color")
;;   (let (compilation-start-hook)
;;     (apply orig-fn args)))
;; (2) Or override ag's `compilation-error-regexp-alist' matching function

(eval-and-compile
  (defconst nvp-ag/rg-grouped-file-regex "^\\(?:File:\\s-*\\)?\\([^ \t].*\\)$"
    "Support either `xterm-color' filtered results or defaults.")

  (defconst nvp-ag/rg-file-column-regex "^\\([[:digit:]]+\\):\\([[:digit:]]+\\):"))

(eval-when-compile
  (defsubst nvp-match-grouped-filename (file-column-re grouped-re)
    "Match grouped filename in compilation output, not relying on escape codes."
    (save-match-data
      (save-excursion
        (beginning-of-line)
        (while (and (not (bobp))
                    (looking-at-p file-column-re))
          (forward-line -1))
        (and (looking-at grouped-re)
             (list (match-string 1))))))

  ;; imenu that should work for ag/rg grouped results buffers
  (defmacro nvp-ag/rg-imenu-function ()
    `(lambda ()
       (cl-block nil
         (when (re-search-backward ,nvp-ag/rg-file-column-regex nil 'move)
           (beginning-of-line)
           (while (and (not (bobp))
                       (looking-at-p ,nvp-ag/rg-file-column-regex))
             (forward-line -1))
           (and (looking-at "^\\(?:File: \\)?\\([^ \t].*\\)$")
                (cl-return t)))))))


;; -------------------------------------------------------------------
;;; Ag

(with-eval-after-load 'ag
  ;; Match grouped filenames in rg/ag searches
  (defun nvp-ag-match-grouped-filename ()
    (nvp-match-grouped-filename
     ag/file-column-pattern-group nvp-ag/rg-grouped-file-regex))

  (defun nvp-ag-recompile ()
    (interactive)
    (let ((compilation-start-hook
           (delq 'nvp-compilation-start-hook compilation-start-hook)))
      (call-interactively 'recompile)))

  ;; override ag's function
  (setf (symbol-function 'ag/compilation-match-grouped-filename)
        'nvp-ag-match-grouped-filename)

  (defvar nvp-ag-imenu-expression `((nil ,(nvp-ag/rg-imenu-function) 1))))

;; -------------------------------------------------------------------
;;; Ripgrep (rg.el)

;; Override rg's `compilation-error-regexp-alist' matching
;; to use with `xterm-color-filter'
(with-eval-after-load 'rg
  (defun nvp-rg-match-grouped-filename-xc ()
    (nvp-match-grouped-filename
     nvp-ag/rg-file-column-regex nvp-ag/rg-grouped-file-regex))

  ;; use my own filter that works with `xterm-color'
  (defalias 'rg-filter 'ignore)

  ;; rg-mode is compilation-derived mode for results
  ;; this hook lets it work with xterm-color
  (add-hook 'rg-mode-hook
            (nvp-def nvp-rg-mode-hook ()
              (setq-local compilation-transform-file-match-alist nil)
              (push 'nvp-rg-group-xc compilation-error-regexp-alist)
              (push (cons 'nvp-rg-group-xc
                          (list nvp-ag/rg-file-column-regex
                                'nvp-rg-match-grouped-filename-xc 1 2))
                    compilation-error-regexp-alist-alist)
              (setq-local imenu-generic-expression
                          `((nil ,(nvp-ag/rg-imenu-function) 1))))))


;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-rgrep-dwim (root search &optional regex)
  (interactive (nvp-find--defaults current-prefix-arg "Rgrep search"))
  (require 'nvp-grep-config)
  (let ((default-directory root))
    (grep-compute-defaults)
    (rgrep (if regex search (format "\\b%s\\b" search)) "*.*" nil (nvp-prefix 16))))

;;;###autoload
(defun nvp-ag-dwim (root search &optional regex)
  "Run ag search."
  (interactive (nvp-find--defaults current-prefix-arg "Ag search"))
  (require 'nvp-ag-config)
  (unless (integerp current-prefix-arg) ; used as context=%d in ag/search
    (setq current-prefix-arg nil))
  (ag/search search root :regexp regex))

;;;###autoload
(defun nvp-ag-elisp-dwim (root search &optional regex)
  "Run ag search including `package-user-dir' from `user-emacs-directory'.
Ignore elpa directory by default, but with any prefix, prompt to include."
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (list nvp/emacs
           (nvp-find--search-term "Ag elisp" (eq 16 arg))
           (eq 16 arg))))
  (require 'nvp-ag-config)
  (let* ((elpa (nvp-path 'ds package-user-dir))
         (ag-ignore-list
          (nvp-prefix 4
            (if (y-or-n-p "Include elpa? ")
                (cl-callf2 cl-remove elpa ag-ignore-list :test #'string=)
              ag-ignore-list)
            :test '>=
            (cl-pushnew elpa ag-ignore-list :test #'string=))))
    (unless (integerp current-prefix-arg)
      (setq current-prefix-arg nil))
    (ag/search search root :regexp regex)))

;;;###autoload
(defun nvp-ag-dired (arg)
  "Call ag dired functions.
(0) `ag-project-dired-regexp'
(1) `ag-project-dired'
(2) `ag-dired-regexp'
(3) `ag-dired'" 
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (require 'nvp-ag-config)
  (call-interactively
   (pcase arg
     (`4  #'ag-project-dired)
     (`16 #'ag-dired-regexp)
     (`64 #'ag-dired)
     (_   #'ag-project-dired-regexp))))

(provide 'nvp-find)
;;; nvp-find.el ends here
