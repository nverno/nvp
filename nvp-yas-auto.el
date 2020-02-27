;;; nvp-yas-auto.el --- yas helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'yasnippet)
(require 'nvp)

;; TODO: optionally remove dirs of modes with no live buffers
;;;###autoload
(defun nvp-yas-reload-all ()
  "Reload modes' snippet tables, removing any that no longer exist."
  (interactive)
  (when (and nvp-mode-snippet-dir (not (member nvp-mode-snippet-dir yas-snippet-dirs)))
    (push nvp-mode-snippet-dir yas-snippet-dirs))
  (setq yas-snippet-dirs (cl-remove-if-not #'file-exists-p yas-snippet-dirs))
  (mapc #'yas-load-directory yas-snippet-dirs))

;; compile snippets, optionally compile all snippet subdirs in site-lisp addons
;;;###autoload
(defun nvp-yas-compile (&optional all)
  "Compile snippets in default location.
Optionally, compile ALL snippets including subdirs in site-lisp packages."
  (interactive "P")
  (let ((yas-snippet-dirs
         (cons nvp/snippet
               (and all (directory-files-recursively nvp/site "snippets" 'dirs)))))
    (mapc #'yas-recompile-all yas-snippet-dirs)))

;; -------------------------------------------------------------------
;;; Active expansion

;;;###autoload
(defun nvp-yas-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
	 (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

;;;###autoload
(defun nvp-yas-start-of-active-field ()
  (interactive)
  (let* ((snippet (car-safe (yas-active-snippets)))
	 (position (yas--field-start
                    (yas--snippet-active-field snippet))))
    (if (= (point) position)
	(move-beginning-of-line 1)
      (goto-char position))))

(provide 'nvp-yas-auto)
;;; nvp-yas-auto.el ends here
