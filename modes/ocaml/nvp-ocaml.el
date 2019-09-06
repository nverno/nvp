;;; nvp-ocaml.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-complete)
  (defvar utop-buffer-name)
  (defvar opam-share))
(require 'nvp)
(declare-function utop "utop")
(declare-function ocaml-module-alist "caml-help")
(declare-function smie-forward-sexp "smie")
(nvp-decl string-trim-right tuareg-opam-current-compiler tuareg-beginning-of-defun
  tuareg-opam-update-env)


(defun nvp-ocaml-library-path ()
  (when (executable-find "opam")
    (--when-let
        (shell-command-to-string
         "eval $(opam config env); opam config exec -- echo $CAML_LD_LIBRARY_PATH")
      (string-trim-right it))))

;; get version
(defun nvp-ocaml-compiler-version ()
  (let ((ver (shell-command-to-string "opam switch show -s")))
    (and ver (string-match "\\([0-9.]+\\)" ver)
         (match-string 1 ver))))

;; check tools, return those that are installed
(defun nvp-ocaml-installed-tools (&optional tools)
  (let ((tools (or tools '("merlin" "utop" "ocp-indent"))))
    (shell-command-to-string
     (format "opam list --installed --short --safe --color=never %s"
             (mapconcat 'identity tools " ")))))

;; read input in various ways
(defmacro nvp-ocaml-read (prompt &optional thing &rest args)
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

;;; Environment

;; return list of opam environment variables 
;; (defun nvp-ocaml--opam-env ()
;;   (let ((env (shell-command-to-string
;;               "opam config env --safe --sexp")))
;;     (and env (not (string= env ""))
;;          (car (read-from-string env)))))

;; sync emacs environment with opam
;; (defun nvp-ocaml-opam-setenv ()
;;   (let ((env (nvp-ocaml--opam-env)))
;;     (when env
;;       (dolist (var env)
;;         (setenv (car var) (cadr var))
;;         (when (string= (car var) "PATH")
;;           (setq exec-path
;;                 (split-string (cadr var) path-separator)))))))

;; opam-share
(defsubst nvp-ocaml-opam-share ()
  (let ((reply (shell-command-to-string "opam config var share --safe")))
    (and reply (substring reply 0 -1))))

;; check / set variables
(defsubst nvp-ocaml-setenv ()
  ;; make sure opam binaries are on path
  (and (not (executable-find "ocamlbuild"))
       (tuareg-opam-update-env
        (tuareg-opam-current-compiler)))
  ;; set opam-share and add to load-path
  ;; (and (not (bound-and-true-p opam-share))
  ;;      (setq opam-share (nvp-ocaml-opam-share)))
  ;; (add-to-list
  ;;  'load-path (expand-file-name "emacs/site-lisp" opam-share))
  )

(defsubst nvp-ocaml-match-p ()
  (save-excursion
    (beginning-of-line)
    (or (looking-at-p "[ \t]*\\(?:|\\|match\\)")
        (progn
          (forward-line -1)
          (looking-at-p "[ \t]*\\(?:match\\)")))))

(defsubst nvp-ocaml-record-p ()
  (save-excursion
    (up-list -1 t t)
    (looking-at-p "{")))

(defsubst nvp-ocaml-align-fields ()
  (let (start end)
    (save-excursion
      (up-list -1 t t)
      (setq start (point))
      (forward-sexp)
      (setq end (point)))
    (align-regexp start end "\\(\\s-*\\)\\(?::\\|=\\)")))

;; -------------------------------------------------------------------
;;; Insert / Toggle

(defun nvp-ocaml-toggle-rec ()
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
(nvp-decl tuareg-beginning-of-defun)

;;-- Movement
(defun nvp-ocaml-previous-defun ()
  (interactive)
  (tuareg-beginning-of-defun))

;; goto beginning of next defun
(defvar tuareg-starters-syms)
(defun nvp-ocaml-next-defun ()
  (interactive)
  (when (nvp-ocaml-find-matching-starter tuareg-starters-syms)
    (beginning-of-line)))

;; modified from `tuareg-find-matching-starter' to use
;; `smie-forward-sexp'
(defun nvp-ocaml-find-matching-starter (starters)
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

(cl-defmethod nvp-newline-dwim-comment
  (&context (major-mode tuareg-mode) &optional syntax arg _cont)
  (nvp-newline-dwim--comment syntax arg))

(defun nvp-ocaml-newline-or-insert ()
  (interactive)
  (cond
   ((nvp-ocaml-match-p)
    (beginning-of-line)
    (if (looking-at-p "[ \t]*$")
        (indent-according-to-mode)
      (let ((l1 (looking-at-p "[ \t]*\\(?:match\\)")))
        (end-of-line)
        (if l1 (newline-and-indent)
          (insert "\n| ")
          (indent-according-to-mode)))))
   ((nvp-ocaml-record-p)
    (nvp-ocaml-align-fields)
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

(defun nvp-ocaml-merlin-init ()
  "Create .merlin with all ocamlfind packages / .opam sources."
  (interactive)
  (-when-let (script (expand-file-name "emacs/merlin-init" nvp/bin))
    (start-process-shell-command "bash" nil (concat "bash -l " script))))

;;; inf shell

;; switch b/w inferior and source buffers
(defvar nvp-ocaml--source-buffer nil)
(defun nvp-ocaml-switch-to-inf ()
  (interactive)
  (if (eq major-mode 'utop-mode)
      (and nvp-ocaml--source-buffer
           (pop-to-buffer nvp-ocaml--source-buffer))
    (setq nvp-ocaml--source-buffer (current-buffer))
    (if (and (bound-and-true-p utop-buffer-name)
             (buffer-live-p utop-buffer-name))
        (pop-to-buffer utop-buffer-name)
      (utop))))

(defun nvp-ocaml-utop-return ()
  (interactive)
  (goto-char (line-end-position))
  (insert ";;")
  (call-interactively 'utop-eval-input-or-newline))

;;; Tag

(defun nvp-ocaml-tag-source ()
  (interactive)
  ;; (let* ((dir (expand-file-name ""))))
  )

;; -------------------------------------------------------------------
;;; Compile 

;; display current compiler
(defun nvp-ocaml-compiler ()
  (interactive)
  (message "%s" (nvp-ocaml-compiler-version)))

;; Run compile, with prefix offers completing read for command line
;; switches to ocamlc
(defun nvp-ocaml-compile (&optional args)
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

(provide 'nvp-ocaml)
;;; nvp-ocaml.el ends here
