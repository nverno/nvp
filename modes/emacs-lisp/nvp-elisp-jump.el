;;; nvp-elisp-jump.el --- jump to locations -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/elisp-utils
;; Last modified: <2019-03-07 13:02:56>
;; Created:  2 December 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(autoload 'lm-header "lisp-mnt")
(autoload 'find-function-library "find-func")
(autoload 'find-library-name "find-func")

;;;###autoload
(defun nvp-elisp-jump-to-library (library &optional arg)
  "Jump to LIBRARY file."
  (interactive
   (list (call-interactively 'locate-library) current-prefix-arg))
  (when library
    (let* ((el (concat (file-name-sans-extension library) ".el"))
           (elgz (concat el ".gz")))
      (pcase arg
        (`(4) (dired (file-name-directory library)))
        (`(16) (let ((scripts (nvp-install--script (file-name-directory library))))
                 (and (file-exists-p scripts)
                      (find-file-other-window scripts))))
        (_ (find-file-other-window (if (file-exists-p el) el elgz)))))))

(defun nvp-elisp-get-library-file (&optional lisp-only)
  "Get defining file for current symbol or prompt for library.
Optionally, search LISP-ONLY files (no C sources)."
  (let* ((sym (or (symbol-at-point)
                  (intern (ido-completing-read
                           "Feature: "
                           (mapcar #'symbol-name features) nil t))))
         (lib (if (not (memq sym features))
                  (cdr (find-function-library sym lisp-only))
                (find-library-name (symbol-name sym)))))
    lib))

;;;###autoload
(defun nvp-elisp-jump-to-library-url (&optional choose)
  "Browse URL of either file defining symbol at point or prompt for library.
If CHOOSE is non-nil, prompt for library."
  (interactive "P")
  (let (url)
    (if (and (not choose) (setq url (save-excursion (lm-header "URL"))))
        (browse-url url)                ;found URL in current buffer!!
       (let ((lib (nvp-elisp-get-library-file 'lisp-only)))
         (if (and lib                   ;no URL in emacs sources
                  (member (file-name-extension lib) '("el" "elc")))
             (let ((file (concat (file-name-sans-extension lib) ".el")))
               (if (not (file-exists-p file))
                   (user-error "Emacs source library - no URL: %s" lib)
                 (with-temp-buffer
                   (insert-file-contents file)
                   (if (setq url (lm-header "URL"))
                       (browse-url url)
                     (user-error "Library %s has no URL header" lib)))))
           (user-error "Library %s isn't elisp." lib))))))

;;;###autoload
(defun nvp-elisp-jump-to-cask (&optional this-window)
  "Jump to the closest Cask file."
  (interactive "P")
  (unless (buffer-file-name)
    (user-error "The buffer has no file"))
  (let ((dir (locate-dominating-file (buffer-file-name) "Cask")))
    (unless dir
      (user-error "No Cask file found for this file"))
    (if this-window (find-file (expand-file-name "Cask" dir))
      (find-file-other-window (expand-file-name "Cask" dir)))))

(provide 'nvp-elisp-jump)
;;; nvp-elisp-jump.el ends here
