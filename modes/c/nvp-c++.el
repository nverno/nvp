;;; nvp-c++.el --- C++ helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-compile))
(require 'nvp-c)
(declare-function nvp-c-out-file "c-tools")
(nvp-declare "" nvp-compile nvp-compile-cmake)

;; -------------------------------------------------------------------
;;; Commands 

;;; Compile
;; pretty much same as c-tools-compile
(nvp-make-or-compile-fn nvp-c++-compile
  (:default-prompt (read-from-minibuffer "Compiler flags: "))
  (let* ((flags (or args "-Wall -Werror -O2 -g -std=c++14"))
         (file (file-name-nondirectory buffer-file-name))
         (out (file-name-sans-extension file))
         (compile-command
          (format "%s %s -o %s%s %s" (nvp-program "g++")
                  flags out (nvp-with-gnu/w32 ".out" ".exe") file)))
    (nvp-compile)))

(defun nvp-c++-compile-and-run (keep)
  (interactive "P")
  (funcall-interactively
   #'nvp-c-compile-and-run keep (nvp-program "g++") "-std=c++14 -O3 -s"))

;; -------------------------------------------------------------------
;;; Font-lock

(defface font-lock-doxygen-face
  '((nil (:foreground "SaddleBrown" :background "#f7f7f7")))
  "Special face to highlight doxygen tags such as <tt>...</tt>
and <code>...</code>."
  :group 'font-lock-highlighting-faces)

;; toggle font-locking for doxygen
(defvar-local nvp-c++--add-font t)
(defun nvp-c++-doxygen ()
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
