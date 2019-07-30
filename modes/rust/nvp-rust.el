;;; nvp-rust.el --- rustls -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar toml-mode-map))
(require 'tag-utils)                    ;remove
(require 'rust-mode)
;; FIXME: declare functions for s/f -- dont want these
(require 'racer)
(nvp-declare "" nvp-setup-program)

;; TODO:
;; - macroexp: rustc --pretty expanded, or rustc --pretty expanded,hygiene
;;   https://doc.rust-lang.org/book/macros.html#debugging-macro-code
;; - deps

(nvp-package-define-root)

(defvar nvp-rust-src-repo "https://github.com/rust-lang/rust")
(defvar nvp-rust-rust-home (or (getenv "RUST_SRC_PATH")
                               (expand-file-name "rust/src" (getenv "DEVEL"))))

;; -------------------------------------------------------------------
;;; Tag

;; clone/update source repo, tag it, set RUST_SRC_PATH
(defun nvp-rust-tag-source (arg &optional noretry)
  (interactive "P")
  (let ((have-src (file-exists-p nvp-rust-rust-home))
        (tags (expand-file-name "TAGS" nvp-rust-rust-home)))
    (if (and (not noretry)
             (or (not have-src) (eq '(16) arg)))
        ;; clone/pull first
        (set-process-sentinel
         (apply 'start-process "nvp-rust" "*nvp-install*" "git"
                (delq nil
                      `(,@(if (not have-src)
                              `("clone" ,nvp-rust-src-repo)
                            '("pull" "origin" "master"
                              "--allow-unrelated-histories"))
                        "--depth=10"
                        ,(when (not have-src)
                           (directory-file-name
                            (file-name-directory
                             nvp-rust-rust-home))))))
         (lambda (p _m)
           (when (zerop (process-exit-status p))
             ;; set RUST_SRC_PATH
             (nvp-with-w32
               (nvp-rust-w32-setenv! "RUST_SRC_PATH"
                                       nvp-rust-rust-home))
             (nvp-rust-tag-source nil t))))
      ;; tag source / load tags table when finished
      (when have-src
        (if (and (not arg) (file-exists-p tags))
            (visit-tags-table tags)
         (tag-utils-tag-dir nvp-rust-rust-home))))))

;; -------------------------------------------------------------------
;;; Cargo

(defun nvp-rust-cargo-help ()
  (interactive)
  (browse-url "http://doc.crates.io/guide.html"))

(defun nvp-rust-locate-cargo ()
  (or (and buffer-file-name
           (expand-file-name
            "Cargo.toml"
            (locate-dominating-file buffer-file-name "Cargo.toml")))
      (user-error "No Cargo.toml found.")))

(defun nvp-rust-open-cargo ()
  (interactive)
  (when-let ((cargo (nvp-rust-locate-cargo)))
    (find-file cargo)))

;; -------------------------------------------------------------------
;;; Insert / Toggle

;; rust mode functions:
;; rust-in-comment-paragraph
;; rust-in-macro
;; rust-in-str-or-cmnt
;; rust-beginning-or-defun
;; rust-end-of-defun

;; toggle public visibility of function at point
(defun nvp-rust-toggle-pub ()
  (interactive)
  (save-excursion
    (rust-beginning-of-defun)
    (if (and (looking-at-p "pub ")
             (message "pub off"))
        (delete-char 4)
      (insert "pub ")
      (message "pub on"))))

;; toggle mutability of variable at point
(defun nvp-rust-toggle-mut ()
  (interactive)
  (save-excursion
    (racer-find-definition)
    (back-to-indentation)
    (forward-char 4)
    (if (and (looking-at-p "mut ")
             (message "mutability off"))
        (delete-char 4)
      (insert "mut ")
      (message "mutability on"))))

;; convert vector expression at point to slice
;; foo -> &foo[..]
(defun nvp-rust-slice ()
  (interactive)
  (insert "&")
  (forward-symbol 1)
  (insert "[..]"))

;;; Enums / Struct

(eval-when-compile
  ;; do body business at item location
  (defmacro nvp-rust-at-definition-of (item &rest body)
    (declare (indent defun))
    `(save-excursion
       (with-current-buffer (find-file-noselect
                             (get-text-property 0 'file ,item))
         (goto-char (point-min))
         (forward-line (1- (get-text-property 0 'line ,item)))
         (forward-char (get-text-property 0 'col ,item))
         ,@body)))

  ;; collect stuff between beginning/end of rust def at point
  (defmacro nvp-rust-collect-fields (regexp)
    (declare (indent defun))
    `(save-excursion
       (rust-beginning-of-defun)
       (let ((end (save-excursion
                    (rust-end-of-defun)
                    (point)))
             (case-fold-search)
             opts)
         (while (< (point) end)
           (and (looking-at ,regexp)
                (push (match-string-no-properties 1) opts))
           (forward-line 1))
         opts))))

;; find type in let expression
(defun nvp-rust-let-type ()
  (let (case-fold-search)
    ;; "\\_<\\([[:upper:]][_[:digit:][:nonascii:][:word:]]*\\)\\_>"
    (when (re-search-forward
           rust-re-type-or-constructor (line-end-position) t)
      (match-string 1))))

;; Get cases of thing at point (|):
;;
;; Option| ==> (:enum "Some" "None")
;;
;; fn blah(t: &Option<&Thing>) -> Thing {
;;   match *t| ==> (:enum "Some" "None")
;;
;; let x = Some("value");
;; x| ==> (:enum "Some" "None")
;;
;; let x: Option<u32> = ...
;; x| ==> (:enum "Some" "None")
;;
;;; struct
;;
;; struct Point {
;;   x: i32,
;;   y: i32,
;; }
;; let point = Point| ==> (:struct "x" "y")
;;
;; FIXME: fix reading fields, matches by line ...

(defun nvp-rust-cases ()
  (-when-let* ((sym (symbol-at-point))
               (sym (cl-find-if
                     (lambda (s) (string= sym s)) (racer-complete))))
    (pcase (get-text-property 0 'matchtype sym)
      ((or "Let" "FnArg")
       (nvp-rust-at-definition-of sym
         (when (nvp-rust-let-type)
           (nvp-rust-cases))))
      ((or "Enum" "EnumVariant")
       (nvp-rust-at-definition-of sym
         (cons :enum (nvp-rust-enum-fields))))
      ("Struct"
       (nvp-rust-at-definition-of sym
         (cons :struct (nvp-rust-struct-fields))))
      (_ sym))))

;; get fields of enum at point
(defun nvp-rust-enum-fields ()
  (nvp-rust-collect-fields
    (concat "^\\s-*" rust-re-type-or-constructor)))

;; struct fields
(defun nvp-rust-struct-fields ()
  (nvp-rust-collect-fields "^\\s-*\\([_a-z0-9]+\\):"))

;; get fields for struct / enum we be at
(defun nvp-rust-match ()
  (interactive)
  (let ((_meta (racer--call-at-point "find-definition")))
    ;; FIXME:
    ))

;; -------------------------------------------------------------------
;;; Interactive

(nvp-newline nvp-rust-newline-dwim nil
  :pairs (("{" "}"))
  :comment-re (" *\\(?:/\\*\\|\\*\\)" . "\\*/ *")
  :comment-start " * ")

;; -------------------------------------------------------------------
;;; Minor Mode

(defvar nvp-rust-menu
  '("RU"
    ["Tag Source" nvp-rust-tag-source
     :help "C-u C-u update source, C-u retag"]
    ["Open Cargo.toml" nvp-rust-open-cargo]
    "--"
    ["Install/Upgrade" nvp-rust-install
     :help "Choco upgrade or install with prefix."]))

(defvar nvp-rust-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil nvp-rust-menu)
    (define-key km (kbd "<f2> m t") #'nvp-rust-tag-source)
    (define-key km (kbd "<f2> m .") #'xref-find-definitions)
    (define-key km (kbd "<f2> M-p") #'nvp-rust-toggle-pub)
    (define-key km (kbd "<f2> M-m") #'nvp-rust-toggle-mut)
    (define-key km (kbd "<f2> M-v") #'nvp-rust-slice)
    (define-key km (kbd "<f2> m c") #'nvp-rust-open-cargo)
    km))

;;;###autoload
(define-minor-mode nvp-rust-mode
  "Rust utilities."
  nil
  :keymap 'nvp-rust-mode-map
  :lighter " RU"
  (nvp-rust-tag-source nil))

(defun nvp-rust-toml-hook ()
  (define-key toml-mode-map (kbd "M-?") #'nvp-rust-cargo-help))

;;;###autoload (add-hook 'toml-mode-hook 'nvp-rust-toml-hook)
;;;###autoload (add-hook 'rust-mode-hook 'nvp-rust-mode)

(provide 'nvp-rust)
;;; nvp-rust.el ends here
