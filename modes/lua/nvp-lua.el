;;; nvp-lua.el --- lua extensions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lua-ts-mode nil t)

(nvp:decls :p (lua) :v (nvp-repl--display-action) :f (lua-funcname-at-point))
(defvar lua-documentation-url)
(defvar lua-documentation-function)

;;; REPL
(with-eval-after-load 'nvp-repl
  (require 'nvp-lua-repl))

;;; Fold

(defvar hs-special-modes-alist)
(with-eval-after-load 'hideshow
  (let ((rules '("\\(?:local \\)?function\\|if\\|do\\|while\\|for\\|{\\|\\[\\["
                 "end\\|}\\|\\]\\]" "--" nil)))
    (dolist (mode '(lua-ts-mode lua-mode))
      (if (assq mode hs-special-modes-alist)
          (setf (cdr (assq 'lua-ts-mode hs-special-modes-alist)) rules)
        (push `(,mode ,@rules) hs-special-modes-alist)))))

;;; Snippets

(nvp:decl nvp-yas-split-args yas-field-value)

;; return keys from 'key1 [= val1][, key_i [= val_i]]*'
(defun nvp-lua--keys (&optional str)
  (nvp-yas-split-args (or str yas-text) "[ \t]*=[^,]*,?[ \t]*"))

;; produce "-" when range is decreasing from INIT to LIMIT
(defun nvp-lua--step-sign (init limit)
  (let ((vinit (yas-field-value init))
        (vlim (yas-field-value limit)))
    (when (and vinit vlim (> (string-to-number vinit) (string-to-number vlim)))
      "-")))

;; -------------------------------------------------------------------
;;; Help

(defvar nvp-lua--rocks nil)
(defun nvp-lua--rocks (&optional recompute)
  (or (and (not recompute) nvp-lua--rocks)
      (setq nvp-lua--rocks
            (cl-loop for line in (process-lines "luarocks" "list" "--porcelain")
                     for parts = (string-split line)
                     collect (cons (car parts) (nth 3 parts))))))
(eval-when-compile
  (defsubst nvp:lua-read-rock (&optional recompute)
    (completing-read "Library: " (nvp-lua--rocks recompute) nil t)))

;; modules and locations for LIBRARY
(defun nvp-lua--modules (library)
  (cl-mapcan (lambda (line)
               (when (string-prefix-p "module" line)
                 (list (cdr (split-string line)))))
             (process-lines "luarocks" "show" "--porcelain" library)))

(defun nvp-lua-documentation (thing &optional rock)
  "Lookup THING using `lua-documentation-url'.
With \\[universal-argument] prefix arg ROCK show luarocks documentation for THING."
  (interactive (list (if current-prefix-arg (nvp:lua-read-rock t)
                       (read-string "Help: " (lua-funcname-at-point)))
                     current-prefix-arg))
  (if rock
      (let ((docs (process-lines "luarocks" "doc" thing "--list" "--porcelain")))
        (--if-let (--first (string-suffix-p "README.md" it t) docs)
            (find-file-other-window it)
          (user-error "No readme found for '%s'" thing)))
    (let ((url (concat lua-documentation-url "#pdf-" thing)))
      (funcall lua-documentation-function url))))

(defun nvp-lua-jump-to-library (library &optional homepage)
  "Jump to lua LIBRARY module source or HOMEPAGE with prefix arg."
  (interactive (list (nvp:lua-read-rock) current-prefix-arg))
  (if homepage (let ((home (car (process-lines "luarocks" "show" "--home" library))))
                 (if home (browse-url home)
                   (user-error "No home for '%s'" library)))
    (let* ((mods (nvp-lua--modules library))
           (mod (if (length= mods 1) (car mods)
                  (assoc-string (completing-read "Module: " mods nil t) mods)))
           (loc (and mod (cadr mod))))
      (unless (and loc (file-exists-p loc))
        (user-error "Library '%s' doesnt exist" library))
      (if (string= "so" (file-name-extension loc))
          (dired (file-name-directory loc))
        (find-file-other-window (cadr mod))))))

(provide 'nvp-lua)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua.el ends here
