;;; nvp-cmake.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))

(defun nvp-cmake-current-function ()
  (let ((ppss (parse-partial-sexp (point-min) (point))))
    (when (nth 0 ppss)
      (save-excursion
        ;; skip backward up-lists, passing over variables and out of strings
        (while (and (nvp-goto 'bul)
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
          (or (and (string-match-p "^[A-Z]+" sym) ; variable
                   (nvp-cmake--get-eldoc-string sym 'variable))
              (nvp-cmake--get-eldoc-string sym 'command)))
        (--when-let (nvp-cmake-current-function)
          (nvp-cmake--get-eldoc-string it 'command)))))


(provide 'nvp-cmake)
;;; nvp-cmake.el ends here
