;;; nvp-lua.el --- lua extensions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lua-ts-mode nil t)
(nvp:decls :p (lua) :v (nvp-repl--display-action))

;;; Repl

(define-advice lua-ts-inferior-lua (:around (orig-fn) "dont-be-dumb")
  (let ((display-buffer-overriding-action nvp-repl--display-action))
    (save-window-excursion
      (funcall orig-fn))))

;; Note: `lua-ts-mode' doesn't define a major mode for the REPL
(defvar lua-repl-mode-hook nil)

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(lua-mode lua-ts-mode)
    :name 'lua
    :modes '(lua-repl-mode)
    :send-region #'lua-ts-send-region
    :send-buffer #'lua-ts-send-buffer
    :send-file #'lua-ts-send-file
    :bufname (regexp-quote lua-ts-inferior-buffer)
    :cd-cmd "lfs=require 'lfs'; lfs.chdir(\"%s\")"
    :pwd-cmd "lfs=require 'lfs'; print(lfs.currentdir())"
    :help-cmd "_G"
    :history-file ".lua_history"
    :init (lambda (&optional _prefix)
            (save-window-excursion
              (let* ((display-buffer-overriding-action nvp-repl--display-action)
                     (proc (funcall-interactively #'lua-ts-inferior-lua)))
                (with-current-buffer (process-buffer proc)
                  (setq major-mode 'lua-repl-mode)
                  (run-hooks 'lua-repl-mode-hook))
                proc)))))

;;; Fold

(defvar hs-special-modes-alist)
(with-eval-after-load 'hideshow
  (setf (cdr (assq 'lua-ts-mode hs-special-modes-alist))
        '("\\(?:local \\)?function\\|if\\|do\\|while\\|for\\|{\\|\\[\\["
          "end\\|}\\|\\]\\]" "--" nil)))

;;; Help

(defvar nvp-lua--rocks nil)
(defun nvp-lua--rocks (&optional recompute)
  (or (and (not recompute) nvp-lua--rocks)
      (setq nvp-lua--rocks
            (cl-loop for line in (process-lines "luarocks" "list" "--porcelain")
                     for parts = (string-split line)
                     collect (cons (car parts) (nth 3 parts))))))

;; modules and locations for LIBRARY
(defun nvp-lua--modules (library)
  (cl-mapcan (lambda (line)
               (when (string-prefix-p "module" line)
                 (list (cdr (split-string line)))))
             (process-lines "luarocks" "show" "--porcelain" library)))

(defun nvp-lua-documentation (thing &optional rock)
  "Lookup THING using `lua-documentation-url'.
With prefix arg ROCK show luarocks documentation for THING."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Rock: " (nvp-lua--rocks) nil t)
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
  (interactive
   (list (completing-read "Library: " (nvp-lua--rocks) nil t) current-prefix-arg))
  (if homepage
      (let ((home (car (process-lines "luarocks" "show" "--home" library))))
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
