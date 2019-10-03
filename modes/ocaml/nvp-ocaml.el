;;; nvp-ocaml.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;; opam:
;; - dump lisp-readable env.: opam config env --safe --sexp
;; - share directory: opam config var --safe
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-complete))
(require 'nvp)
(require 'tuareg)
(nvp-decls :f (string-trim-right nvp-async-shell-command-to-string))
(nvp-package-define-root :name nvp-ocaml :dirs ("etc"))

;;; Config

(defun nvp-ocaml-merlin-init ()
  "Create .merlin with all ocamlfind packages / .opam sources."
  (interactive)
  (nvp-async-shell-command-to-string
   (concat "opam config exec -- bash -c "
           (expand-file-name "emacs/merlin-init" nvp/bin))))

(defun nvp-ocaml-init ()
  "Create local .ocamlinit."
  (interactive)
  (nvp-async-shell-command-to-string
   (expand-file-name "emacs/ocaml-init" nvp/bin)))

(defun nvp-ocaml-library-path ()
  (when (executable-find "opam")
    (--when-let
        (shell-command-to-string "eval $(opam config env); opam config var lib")
      (string-trim-right it))))


;;; Newlines

;; default newline-dwim + comment continuation in nested comments
(cl-defmethod nvp-newline-dwim-comment
  (&context (major-mode tuareg-mode) &optional syntax arg _cont)
  (nvp-newline-dwim--comment syntax arg))

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

(defsubst nvp-ocaml-line-closed-p ()
  (save-excursion
    (end-of-line)
    (or (save-excursion (equal ";;" (smie-default-forward-token)))
        (equal ";;" (smie-default-backward-token)))))

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
    (unless (nvp-ocaml-line-closed-p)
      (insert ";;"))
    (newline-and-indent))))

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

;;; Move

(defsubst nvp-ocaml--beginning-of-comment ()
  (let ((ppss (parse-partial-sexp (point-min) (point))))
    (and (nth 8 ppss)
         (goto-char (nth 8 ppss)))))

(defun nvp-ocaml-previous-defun ()
  (interactive)
  (let ((bl (point-at-bol)))
    (nvp-ocaml--beginning-of-comment)
    (forward-comment (- (point-max)))
    (tuareg-beginning-of-defun)
    (when (<= bl (point))
      ;; gets stuck here on cases like this w/o beginning-of-line, cursor at '|'
      ;;> blah ;; |blah ;;
      (beginning-of-line)
      (forward-comment (- (point-max)))
      (tuareg-beginning-of-defun))))

(defun nvp-ocaml-next-defun ()
  (interactive)
  (nvp-ocaml--beginning-of-comment)
  (tuareg-beginning-of-defun -1))

;;; Tag

(defun nvp-ocaml-tag-source ()
  (interactive)
  ;; (let* ((dir (expand-file-name ""))))
  )

;;; Compile 

;; Run compile, with prefix offers completing read for command line
;; switches to ocamlc
(defun nvp-ocaml-compile (&optional args)
  (interactive)
  (nvp-complete-compile "ocamlc"
    (if args
        (concat "ocamlc " (mapconcat 'identity args " ") " %s")
      "ocamlc -g %s")))

(provide 'nvp-ocaml)
;;; nvp-ocaml.el ends here
