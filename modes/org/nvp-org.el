;;; nvp-org.el --- org helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'org)
(nvp:decls :p (outline org)
           :f (org-tempo-setup org-element-map org-element-parse-buffer))


;; Register custom link format
(nvp:auto "nvp-org-link"
  nvp-org-nvp-export nvp-org-nvp-store-link nvp-org-nvp-open)

(with-eval-after-load 'ol
  (org-link-set-parameters "nvp"
    :store  #'nvp-org-nvp-store-link
    :follow #'nvp-org-nvp-open
    :export #'nvp-org-nvp-export))
(put 'org-link-set-parameters 'lisp-indent-function 1)

;;;###autoload
(defun nvp-org-tags-view (&optional directory)
  "Display org tags with `org-tags-view'.
When call from an `org-mode' buffer with \\[universal-argument], limit tags
to those in current buffer.
Elsewhere, or with prefix >= 4, prompt for DIRECTORY to search within."
  (interactive
   (list (let ((raw (prefix-numeric-value current-prefix-arg)))
           (cond ((and (= raw 4)
                       (derived-mode-p 'org-mode))
                  (buffer-file-name))
                 ((>= raw 4) (completing-read-multiple
                              "Directory: " #'completion-file-name-table))
                 (t nil)))))
  (let ((org-agenda-files (if directory (nvp:as-list directory)
                            org-agenda-files)))
    (nvp:prefix-shift -1)
    (call-interactively #'org-tags-view)))


(defun nvp-org-forward-element ()
  (interactive)
  (nvp:push-mark 'nvp-org-forward-element)
  (cond ((org-at-item-p) (condition-case nil
                             (org-next-item)
                           (error (org-forward-element))))
        (t (org-forward-element))))

(defun nvp-org-backward-element ()
  (interactive)
  (let ((pushed (not (eq t (nvp:push-mark 'nvp-org-backward-element)))))
    (condition-case err
        (org-backward-element)
      (error (and pushed (pop-mark))
             (error (error-message-string err))))))

(nvp:def-keymap nvp-repeat-org-move
  :repeat (:enter (nvp-org-forward-element org-backward-element))
  "TAB" #'org-cycle
  "n"   #'nvp-org-forward-element
  "j"   #'nvp-org-forward-element
  "p"   #'nvp-org-backward-element
  "k"   #'nvp-org-backward-element
  "l"   #'org-down-element
  "h"   #'org-up-element)

;; Ensure point is at end-of-line so text doesn't get carried to next todo
(define-advice org-insert-todo-heading (:before (&rest _args) "move-eol")
  (end-of-line))

;;;###autoload
(defun nvp-org-capture (&optional goto keys)
  (interactive)
  (require 'nvp-vars)
  (org-capture goto keys))

(defun nvp-org-toggle-parent-checkbox ()
  "Add checkbox to parent header if not there and update block statistics."
  (interactive)
  (when (not (eq 0 (org-outline-level)))
    (save-excursion
      (org-back-to-heading t)
      (end-of-line)
      (unless (looking-back "\\[[0-9]*/[0-9]*\\][ \t]*" (line-beginning-position))
        (delete-horizontal-space)
        (insert " [/]"))
      (org-update-checkbox-count))))

;; XXX: it would be nice to get completion like the old hydra
(defun nvp-org-src-maybe ()
  "When '<' is entered at the beginning of a line, load org-tempo.el."
  (interactive)
  (when (and (looking-back "^\\s-*" (line-beginning-position))
             (require 'org-tempo))    ; load shortcuts
    (org-tempo-setup)
    (define-key org-mode-map "<" #'self-insert-command))
  (self-insert-command 1))

(provide 'nvp-org)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-org.el ends here
