;;; nvp-cmake.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'cmake-mode nil t)
(nvp:decls :p "cmake-mode")

(nvp:package-define-root :name nvp-cmake)

(defun nvp-cmake-current-function ()
  (let ((ppss (parse-partial-sexp (point-min) (point))))
    (when (nth 0 ppss)
      (save-excursion
        ;; skip backward up-lists, passing over variables and out of strings
        (while (and (nvp:goto 'bul)
                    (memq (char-after) '(?\{ ?$ ?\")))
          (skip-chars-backward "{$\"" (line-beginning-position)))
        (let ((end (progn
                     (skip-syntax-backward " " (line-beginning-position))
                     (point)))
              (beg (progn
                     (skip-syntax-backward "w_" (line-beginning-position))
                     (point))))
          (buffer-substring-no-properties beg end))))))

;;; Eldoc

(defvar nvp-cmake-eldoc-cache (make-hash-table :test 'equal))

;; single line doc for CMD
(defun nvp-cmake--get-eldoc-string (cmd type)
  (or (gethash cmd nvp-cmake-eldoc-cache)
      (let ((res
             (string-trim-right
              (shell-command-to-string
               (concat "cmake --help-" (symbol-name type)
                       " " (shell-quote-argument cmd)
                       " | awk 'FNR==1 && $1 != \"Argument\" {printf \"%s: \", $1}"
                       " FNR==4'")))))
        (and (not (equal "" res))
             (puthash cmd res nvp-cmake-eldoc-cache)))))

(defun nvp-cmake-eldoc-function ()
  "Return eldoc string for cmake variables/commands."
  (let ((case-fold-search nil)
        (sym (thing-at-point 'symbol)))
    (or (when sym
          (or (and (string-match-p "^[A-Z]+" sym) ; variable/property
                   (or (nvp-cmake--get-eldoc-string sym 'variable)
                       (nvp-cmake--get-eldoc-string sym 'property)))
              (nvp-cmake--get-eldoc-string sym 'command)))
        (--when-let (nvp-cmake-current-function)
          (nvp-cmake--get-eldoc-string it 'command)))))

;;; Abbrevs

(defvar nvp-cmake-abbrev-syntax-table)
(with-eval-after-load 'cmake-mode
  (setq nvp-cmake-abbrev-syntax-table
        (let ((tab (copy-syntax-table cmake-mode-syntax-table)))
          (modify-syntax-entry ?_ "w" tab)
          (modify-syntax-entry ?- "w" tab)
          tab)))

(defun nvp-cmake-expand-abbrev ()
  (nvp:with-letf 'forward-word 'forward-word-strictly
    (c-with-syntax-table nvp-cmake-abbrev-syntax-table
      (abbrev--default-expand))))

(defun nvp-cmake-add-abbrevs (&optional prefix)
  (interactive "P")
  (let ((prog (expand-file-name "bin/dump-vars.awk" (nvp:package-root nvp-cmake))))
    (pcase prefix
      ('(4) (nvp:with-results-buffer :title "Cmake properties/variables"
              (call-process-shell-command prog nil (current-buffer))))
      (_ (mapc
          (lambda (a) (define-abbrev cmake-mode-abbrev-table (downcase a) a))
          (process-lines prog))))))

(provide 'nvp-cmake)
;;; nvp-cmake.el ends here
