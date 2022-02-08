;;; nvp-search.el --- search-map; search/replace -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; - occur
;; - highlight regexps
;; - hi-lock
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :f (occur-read-primary-args consult-multi-occur))
(nvp:auto "nvp-buffer" 'nvp-buffer-matching-mode)
(nvp:auto "replace" 'multi-occur)

(defvar nvp-multi-occur-function #'consult-multi-occur)

;; -------------------------------------------------------------------
;;; Occur 

;;;###autoload
(defun nvp-occur-dwim (&optional arg)
  "Call `occur' with either region or symbol-at-point.
With prefix ARG multi-occur in buffers of the same mode."
  (interactive "P")
  (when-let* ((str (nvp:tap 'dwim)))
    (push (if (stringp str) (regexp-quote str) str) regexp-history))
  (pcase arg
    ('nil (call-interactively #'occur))
    (`(4) (nvp-multi-occur-in-this-mode))
    (`(16) (funcall-interactively nvp-multi-occur-function))
    (_ (call-interactively #'occur)))
  (if (get-buffer "*Occur*")
      (switch-to-buffer-other-window "*Occur*")))

;; https://www.masteringemacs.org/article/searching-buffers-occur-mode
;; Show all lines matching REGEXP in buffers withe the same 
;; `major-mode'.
;;;###autoload
(defun nvp-multi-occur-in-this-mode ()
  (interactive)
  (multi-occur
   (nvp-buffer-matching-mode major-mode) (car (occur-read-primary-args))))

;; -------------------------------------------------------------------
;;; Hi-lock
(eval-when-compile (defvar hi-lock-file-patterns))

;;;###autoload
(defun nvp-hi-lock-forward ()
  "Jump between hi-lock matches."
  (interactive)
  (goto-char
   (apply
    #'min
    (mapcar
     (lambda (pattern)
       (save-excursion
         (re-search-forward (car pattern) nil 'noerror)
         (point)))
     hi-lock-file-patterns))))

(provide 'nvp-search)
;;; nvp-search.el ends here
