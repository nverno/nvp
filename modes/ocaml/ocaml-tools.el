;;; ocaml-tools.el ---  -*- lexical-binding: t; -*-

;; Last modified: <2019-03-08 06:33:06>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  4 November 2016

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'nvp-complete)
  (defvar utop-buffer-name)
  (defvar opam-share))
(declare-function utop "utop")
(declare-function ocaml-module-alist "caml-help")
(declare-function smie-forward-sexp "smie")
(nvp-declare "tuareg" tuareg-opam-current-compiler tuareg-beginning-of-defun)
(autoload 'nvp-ext-run-script "nvp-ext")
(autoload 'nvp-process-buffer "nvp")
(autoload 'tuareg-opam-update-env "tuareg")
(autoload 'expand-abbrev-hook "expand")

(nvp-package-define-root)

;; -------------------------------------------------------------------
;;; Install 
;; opam, ocp-indent, utop, merlin

(nvp-with-gnu
  ;; FIXME: install aspcud external solver
  ;; https://opam.ocaml.org/doc/Install.html#GettingtheSources
  (defun ocaml-tools-install (what)
    (interactive
     (list (ido-completing-read
            "Install: " '("opam" "packages" "src" "info"))))
    (let ((script (expand-file-name "tools/install.sh" (nvp-package-root))))
     (pcase what
       (`"opam"
        (set-process-sentinel
         (nvp-ext-run-script script '("install_opam") 'sudo)
         #'(lambda (p _m)
             (when (zerop (process-exit-status p))
               ;; run non-sudoed
               (nvp-ext-run-script '("install_opam_init"))))))
       (`"packages"
        (nvp-ext-run-script script '("install_ocaml_pkgs")))
       (`"src"
        (nvp-ext-run-script script '("install_ocaml_src")))
       (`"info"
        (nvp-ext-run-script script '("install_ocaml_info") 'sudo))
       (_ ())))))

;; -------------------------------------------------------------------
;;; Utils 

;; read input in various ways
(defmacro ocaml-tools-read (prompt &optional thing &rest args)
  (declare (indent defun) (debug t))
  (pcase thing
    ((pred stringp)
     `(read-from-minibuffer ,prompt ,thing))
    (`(quote ,sym)
     (cond
      ((consp sym)
       `(ido-completing-read ,prompt ,thing))
      ((symbol-function sym)
       (if args
           `(funcall-interactively ',sym ,@args)
         `(call-interactively ',sym)))
      ((eq sym 'module)
       `(completing-read ,prompt (ocaml-module-alist)))
      (t `(ido-completing-read ,prompt ,sym))))
    ((pred symbolp)
     (if (string= ":module" (symbol-name thing))
         `(completing-read ,prompt (ocaml-module-alist))
       `(ido-completing-read ,prompt ,thing)))
    ((pred consp)
     `(ido-completing-read ,prompt ,thing))
    (_ `(read-from-minibuffer ,prompt))))

;; get version
(defun ocaml-tools-compiler-version ()
  (let ((ver (shell-command-to-string "opam switch show -s")))
    (and ver (string-match "\\([0-9.]+\\)" ver)
         (match-string 1 ver))))

;; check tools, return those that are missing
(defun ocaml-tools-missing-tools (&optional tools)
  (let ((tools (or tools '("merlin" "utop" "ocp-indent"))))
    (delq
     nil
     (mapcar
      #'(lambda (tool)
          (let ((res
                 (shell-command-to-string
                  (concat
                   (eval-when-compile
                     (concat "opam list --installed --short "
                             "--safe --color=never "))
                   tool))))
            (and (string= "" res) tool)))
      tools))))

;;; Environment

;; return list of opam environment variables 
;; (defun ocaml-tools--opam-env ()
;;   (let ((env (shell-command-to-string
;;               "opam config env --safe --sexp")))
;;     (and env (not (string= env ""))
;;          (car (read-from-string env)))))

;; sync emacs environment with opam
;; (defun ocaml-tools-opam-setenv ()
;;   (let ((env (ocaml-tools--opam-env)))
;;     (when env
;;       (dolist (var env)
;;         (setenv (car var) (cadr var))
;;         (when (string= (car var) "PATH")
;;           (setq exec-path
;;                 (split-string (cadr var) path-separator)))))))

;; opam-share
(defun ocaml-tools-opam-share ()
  (let ((reply
         (shell-command-to-string "opam config var share --safe")))
    (and reply
         (substring reply 0 -1))))

;; check / set variables
(defun ocaml-tools-setenv ()
  ;; make sure opam binaries are on path
  (and (not (executable-find "ocamlbuild"))
       (tuareg-opam-update-env
        (tuareg-opam-current-compiler)))
  ;; set opam-share and add to load-path
  ;; (and (not (bound-and-true-p opam-share))
  ;;      (setq opam-share (ocaml-tools-opam-share)))
  ;; (add-to-list
  ;;  'load-path (expand-file-name "emacs/site-lisp" opam-share))
  )

(defsubst ocaml-tools-match-p ()
  (save-excursion
    (beginning-of-line)
    (or (looking-at-p "[ \t]*\\(?:|\\|match\\)")
        (progn
          (forward-line -1)
          (looking-at-p "[ \t]*\\(?:match\\)")))))

(defsubst ocaml-tools-record-p ()
  (save-excursion
    (up-list -1 t t)
    (looking-at-p "{")))

(defsubst ocaml-tools-align-fields ()
  (let (start end)
    (save-excursion
      (up-list -1 t t)
      (setq start (point))
      (forward-sexp)
      (setq end (point)))
    (align-regexp start end "\\(\\s-*\\)\\(?::\\|=\\)")))

;; -------------------------------------------------------------------
;;; Insert / Toggle

(defun ocaml-tools-toggle-rec ()
  (interactive)
  (save-excursion
    (end-of-line)
    (tuareg-beginning-of-defun)
    (forward-word-strictly)
    (if (looking-at-p "[ \t]*rec\\_>")
        (kill-word 1)
      (delete-horizontal-space)
      (insert " rec "))))

;; -------------------------------------------------------------------
;;; Interactive

;;; Movement

(defun ocaml-tools-previous-defun ()
  (interactive)
  (tuareg-beginning-of-defun))

;; goto beginning of next defun
(defvar tuareg-starters-syms)
(defun ocaml-tools-next-defun ()
  (interactive)
  (when (ocaml-tools-find-matching-starter tuareg-starters-syms)
    (beginning-of-line)))

;; modified from `tuareg-find-matching-starter' to use
;; `smie-forward-sexp'
(defun ocaml-tools-find-matching-starter (starters)
  (let (tok)
    (while
        (let ((td (smie-forward-sexp 'halfsexp)))
          (cond
           ;; stop infinite loop when at end of buffer
           ((eobp) nil)
           ((and (car td)
                 (member (nth 2 td) starters))
            (goto-char (nth 1 td)) (setq tok (nth 2 td)) nil)
           ((and (car td) (not (numberp (car td))))
            (unless (bobp) (goto-char (nth 1 td)) t))
           (t t))))
    tok))

;;; Newlines

(nvp-newline ocaml-tools-newline-dwim
  "Newline dwim for ocaml."
  :pairs (("{" "}"))
  :comment-re (" *\\(?:(\\*\\|\\*\\)" . "\\*) *")
  :comment-start "* ")

(defun ocaml-tools-newline-or-insert ()
  (interactive)
  (cond
   ((ocaml-tools-match-p)
    (beginning-of-line)
    (if (looking-at-p "[ \t]*$")
        (indent-according-to-mode)
      (let ((l1 (looking-at-p "[ \t]*\\(?:match\\)")))
        (end-of-line)
        (if l1 (newline-and-indent)
          (insert "\n| ")
          (indent-according-to-mode)))))
   ((ocaml-tools-record-p)
    (ocaml-tools-align-fields)
    (end-of-line)
    (delete-horizontal-space)
    (if (eq (char-before) ?\})
        (insert ";;")
      (unless (eq (char-before) ?\;)
        (insert ";")))
    (newline-and-indent))
   (t
    (end-of-line)
    (unless (looking-back ";;" 1)
      (insert ";;"))
    (newline-and-indent))))

;;; .merlin

(defun ocaml-tools-merlin-init ()
  "Create .merlin with all ocamlfind packages / .opam sources."
  (interactive)
  (let ((script (expand-file-name "tools/merlin-init.sh" (nvp-package-root))))
    (nvp-ext-run-script script)))

;;; inf shell

;; switch b/w inferior and source buffers
(defvar ocaml-tools--source-buffer nil)
(defun ocaml-tools-switch-to-inf ()
  (interactive)
  (if (eq major-mode 'utop-mode)
      (and ocaml-tools--source-buffer
           (pop-to-buffer ocaml-tools--source-buffer))
    (setq ocaml-tools--source-buffer (current-buffer))
    (if (and (bound-and-true-p utop-buffer-name)
             (buffer-live-p utop-buffer-name))
        (pop-to-buffer utop-buffer-name)
      (utop))))

(defun ocaml-tools-utop-return ()
  (interactive)
  (goto-char (line-end-position))
  (insert ";;")
  (call-interactively 'utop-eval-input-or-newline))

;;; Tag

(defun ocaml-tools-tag-source ()
  (interactive)
  ;; (let* ((dir (expand-file-name ""))))
  )

;; -------------------------------------------------------------------
;;; Compile 

;; display current compiler
(defun ocaml-tools-compiler ()
  (interactive)
  (message "%s" (ocaml-tools-compiler-version)))

;; Run compile, with prefix offers completing read for command line
;; switches to ocamlc
(defun ocaml-tools-compile (&optional args)
  (interactive)
  (nvp-complete-compile "ocamlc"
    (if args
        (concat "ocamlc " (mapconcat 'identity args " ") " %s")
      "ocamlc -g %s")))

;; -------------------------------------------------------------------
;;; Symbols 
;; remove annoying stuff
(defvar tuareg-font-lock-symbols-alist)
(setq tuareg-font-lock-symbols-alist
  (cond ((fboundp 'decode-char) ;; or a unicode font.
         `(("fun" . ,(decode-char 'ucs 955))
           ;; ("sqrt" . ,(decode-char 'ucs 8730))
           ;; ("not" . ,(decode-char 'ucs 172))
           ;; ("&&" . ,(decode-char 'ucs 8743)); 'LOGICAL AND' (U+2227)
           ;; ("or" . ,(decode-char 'ucs 8744)); 'LOGICAL OR' (U+2228)
           ;; ("||" . ,(decode-char 'ucs 8744))
           ;; ("[|" . ,(decode-char 'ucs 12314)) ;; 〚
           ;; ("|]" . ,(decode-char 'ucs 12315)) ;; 〛
           ("*." . ,(decode-char 'ucs 215))
           ("/." . ,(decode-char 'ucs 247))
           ("->" . ,(decode-char 'ucs 8594))
           ("<-" . ,(decode-char 'ucs 8592))
           ("<=" . ,(decode-char 'ucs 8804))
           (">=" . ,(decode-char 'ucs 8805))
           ("<>" . ,(decode-char 'ucs 8800))
           ;; ("==" . ,(decode-char 'ucs 8801))
           ;; ("!=" . ,(decode-char 'ucs 8802))
           ("<=>" . ,(decode-char 'ucs 8660))
           (":=" . ,(decode-char 'ucs 8656))
           ("infinity" . ,(decode-char 'ucs 8734))
           ;; Some greek letters for type parameters.
           ("'a" . ,(decode-char 'ucs 945))
           ("'b" . ,(decode-char 'ucs 946))
           ("'c" . ,(decode-char 'ucs 947))
           ("'d" . ,(decode-char 'ucs 948))
           ("'e" . ,(decode-char 'ucs 949))
           ("'f" . ,(decode-char 'ucs 966))
           ("'i" . ,(decode-char 'ucs 953))
           ("'k" . ,(decode-char 'ucs 954))
           ("'m" . ,(decode-char 'ucs 956))
           ("'n" . ,(decode-char 'ucs 957))
           ("'o" . ,(decode-char 'ucs 969))
           ("'p" . ,(decode-char 'ucs 960))
           ("'r" . ,(decode-char 'ucs 961))
           ("'s" . ,(decode-char 'ucs 963))
           ("'t" . ,(decode-char 'ucs 964))
           ("'x" . ,(decode-char 'ucs 958))))
        ((and (fboundp 'make-char) (fboundp 'charsetp) (charsetp 'symbol))
         `(("fun" . ,(make-char 'symbol 108))
           ("sqrt" . ,(make-char 'symbol 214))
           ("not" . ,(make-char 'symbol 216))
           ("&&" . ,(make-char 'symbol 217))
           ("or" . ,(make-char 'symbol 218))
           ("||" . ,(make-char 'symbol 218))
           ("*." . ,(make-char 'symbol 183))
           ("/." . ,(make-char 'symbol 184))
           ("<=" . ,(make-char 'symbol 163))
           ("<-" . ,(make-char 'symbol 172))
           ("->" . ,(make-char 'symbol 174))
           (">=" . ,(make-char 'symbol 179))
           ("<>" . ,(make-char 'symbol 185))
           ("==" . ,(make-char 'symbol 186))
           ("<=>" . ,(make-char 'symbol 219))
           (":=" . ,(make-char 'symbol 220))
           ("=>" . ,(make-char 'symbol 222))
           ("infinity" . ,(make-char 'symbol 165))
           ;; Some greek letters for type parameters.
           ("'a" . ,(make-char 'symbol 97))
           ("'b" . ,(make-char 'symbol 98))
           ("'c" . ,(make-char 'symbol 103)) ; sic! 99 is chi, 103 is gamma
           ("'d" . ,(make-char 'symbol 100))
           ("'e" . ,(make-char 'symbol 101))
           ("'f" . ,(make-char 'symbol 102))
           ("'i" . ,(make-char 'symbol 105))
           ("'k" . ,(make-char 'symbol 107))
           ("'m" . ,(make-char 'symbol 109))
           ("'n" . ,(make-char 'symbol 110))
           ("'o" . ,(make-char 'symbol 111))
           ("'p" . ,(make-char 'symbol 112))
           ("'r" . ,(make-char 'symbol 114))
           ("'s" . ,(make-char 'symbol 115))
           ("'t" . ,(make-char 'symbol 116))
           ("'x" . ,(make-char 'symbol 120))))))

(provide 'ocaml-tools)
;;; ocaml-tools.el ends here
