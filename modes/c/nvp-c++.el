;;; nvp-c++.el --- C++ -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-c)
(nvp:decls)


(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (major-mode c++-mode c++-ts-mode)
  (nvp-newline-dwim--comment syntax arg " * "))

(defface font-lock-doxygen-face
  '((t (:foreground "#dd8920")))
  "Special face to highlight doxygen tags such as <tt>...</tt>
and <code>...</code>."
  :group 'font-lock-highlighting-faces)

(defvar-local nvp-c++--add-font t)
(defun nvp-c++-doxygen ()
  "Toggle font-locking for doxygen."
  (interactive)
  (if (setq nvp-c++--add-font (not nvp-c++--add-font))
      (font-lock-refresh-defaults)
    (font-lock-add-keywords
     ;; 'c++-mode
     nil
     '(("\\(<\\(?:code\\|tt\\)>\"?\\)\\([^<]*?\\)\\(\"?</\\(?:code\\|tt\\)>\\)"
        (0 (prog1 ()
             (let* ((expr (match-string-no-properties 2))
                    (expr-len (length expr)))
               (if (eq 1 expr-len)
                   (compose-region (match-beginning 0)
                                   (match-end 0)
                                   (aref expr 0))
                 (compose-region (match-beginning 1)
                                 (1+ (match-end 1))
                                 (aref expr 0))
                 (compose-region (1- (match-beginning 3))
                                 (match-end 3)
                                 (aref expr (1- expr-len)))))))
        (0 'font-lock-doxygen-face t))))
    (font-lock-flush)
    (font-lock-ensure)))

(provide 'nvp-c++)
;;; nvp-c++.el ends here
