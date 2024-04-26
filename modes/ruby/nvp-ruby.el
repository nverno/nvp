;;; nvp-ruby.el --- ruby stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (yari nvp-hap inf-ruby))

;;; REPL
(with-eval-after-load 'nvp-repl
  (require 'nvp-ruby-repl))

;;; Tree-sitter
(defvar nvp-ruby-ts-font-settings
  (when (treesit-available-p)
    (treesit-font-lock-rules
     :language 'ruby
     :feature 'nvp
     '((assignment
        left: (element_reference
               object: (identifier) @font-lock-variable-name-face))
       (identifier) @font-lock-variable-use-face))))

(define-advice ruby-ts--font-lock-settings
    (:around (orig-fn lang &rest args) "add-fonts")
  (if (eq 'ruby lang)
      (append (apply orig-fn lang args) nvp-ruby-ts-font-settings)
    (apply orig-fn lang args)))

(nvp:treesit-add-rules ruby-ts-mode
  :extra-features '(nvp))

;;; Snippets
;; create arg initializtion from yas-text
;; (i,j) => @i = i\n@j = j or (io=$stdin, ...) => @io = io\n...
(defsubst nvp-ruby-yas-init-args ()
  (mapconcat (lambda (s) (concat "@" s " = " s))
             (--map (car (split-string it "=" t " "))
                    (split-string yas-text "[() ,]+" t " "))
             "\n"))

;;; Movement
(declare-function ruby-end-of-block "ruby-mode")
(defun nvp-ruby-beginning-of-block ()
  (interactive)
  (ruby-end-of-block -1))

;;; Compile
(declare-function ruby-compilation-this-buffer "ruby-compilation")
(defun nvp-ruby-compile ()
  (interactive)
  (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter)
  (ruby-compilation-this-buffer)
  (other-window 1)
  (add-hook 'kill-buffer-hook
            (lambda () (remove-hook 'compilation-filter-hook #'inf-ruby-auto-enter))
            nil t))

;;; Types
(defun nvp-ruby-yardocify-types (start end)
  "Fix types between START and END to conform to yardoc specs."
  (interactive
   (if (region-active-p) (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-mark-and-excursion
    (goto-char start)
    (while (re-search-forward "{\\([^[\n}]+\\)\\(\\(?:\\[\\]\\)*\\)\?}" end t)
      (when (nvp:ppss 'cmt)
        (let* ((cnt (when (match-string 2)
                      (goto-char (match-beginning 2))
                      (save-match-data
                        (count-matches "\\(\\[\\]\\)" (point) (line-end-position)))))
               (rep-str
                (if cnt
                    (concat "[" (s-repeat cnt "Array<") "\\1" (s-repeat cnt ">") "]")
                  "[\\1]")))
          (replace-match rep-str t nil nil 0))))))

;; -------------------------------------------------------------------
;;; Fold / Align

;; ruby-hacks.el
;; setup align for ruby-mode
(defconst align-ruby-modes '(ruby-mode ruby-ts-mode)
  "align-perl-modes is a variable defined in `align.el'.")

(defvar hs-special-modes-alist)
(with-eval-after-load 'hideshow
  (dolist (mode align-ruby-modes)
    (unless (assoc mode hs-special-modes-alist)
      (push (list mode "\\(def\\|do\\)" "end" "#") hs-special-modes-alist))))

(defconst ruby-align-rules-list
  '((ruby-comma-delimiter
     (regexp . ",\\(\\s-*\\)[^/ \t\n]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-string-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-symbol-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
     (modes  . align-ruby-modes)))
  "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")

(with-no-warnings
  (with-eval-after-load 'align
    (dolist (mode align-ruby-modes)
      (dolist (lst '(align-perl-modes
                     align-dq-string-modes
                     align-sq-string-modes
                     align-open-comment-modes))
        (add-to-list lst mode)))
    (dolist (it ruby-align-rules-list)
      (add-to-list 'align-rules-list it))))

;; (defun nvp-ruby--buffer-requires ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let (res)
;;       (while
;;           (re-search-forward "^require +\\(?:['\"]\\)\\([^\n\"']+\\)" nil 'move)
;;         (push (match-string 1) res))
;;       res)))

;; (defun nvp-ruby-require (lib)
;;   (unless (cl-member lib (nvp-ruby--buffer-requires) :test 'string=)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (forward-comment (point-max))
;;       (skip-syntax-backward " >")
;;       (insert (concat "\nrequire \"" lib "\"")))))

(provide 'nvp-ruby)
;;; nvp-ruby.el ends here
