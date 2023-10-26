;;; nvp-cmake-eldoc.el --- Cmake eldoc -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-cmake)
(nvp:decls)

(defvar nvp-cmake-eldoc-cache (make-hash-table :test 'equal))

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

;; single line doc for CMD
(defun nvp-cmake--get-eldoc-string (cmd type)
  (or (gethash cmd nvp-cmake-eldoc-cache)
      (let ((res
             (string-trim-right
              (shell-command-to-string
               (concat
                nvp-cmake-executable " --help-" (symbol-name type)
                " " (shell-quote-argument cmd) " | awk 'FNR==4'")))))
        (and (not (equal "" res))
             (puthash cmd res nvp-cmake-eldoc-cache)))))

;;;###autoload
(defun nvp-cmake-eldoc-function (callback &rest _ignored)
  "Return eldoc string for cmake variables/commands."
  (let ((case-fold-search nil)
        (sym (thing-at-point 'symbol))
        (face 'font-lock-variable-name-face))
    (--when-let
        (or (when sym
              (or (and (string-match-p "^[A-Z]+" sym) ; variable/property
                       (or (nvp-cmake--get-eldoc-string sym 'variable)
                           (nvp-cmake--get-eldoc-string sym 'property)))
                  (--when-let (nvp-cmake--get-eldoc-string sym 'command)
                    (prog1 it (setq face 'font-lock-function-name-face)))))
            (--when-let (nvp-cmake-current-function)
              (setq face 'font-lock-function-name-face
                    sym it)
              (nvp-cmake--get-eldoc-string it 'command)))
      (funcall callback it
               :thing sym
               :face face))))

(provide 'nvp-cmake-eldoc)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cmake-eldoc.el ends here
