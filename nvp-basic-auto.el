;;; nvp-basic-auto.el --- basic autoloads -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-31 04:50:15>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  2 February 2019

;;; Commentary:
;; autos that will get called
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro)
  (require 'hydra))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Hydras

;;;###autoload(autoload 'nvp-hydra-goto-line/goto-line "nvp-basic-auto")
(nvp-hydra-set-property 'nvp-hydra-goto-line)
(defhydra nvp-hydra-goto-line (goto-map) "line"
  ("g" goto-line "go")
  ("b" (push-mark (car mark-ring) nil 'activate) "mark to start")
  ("m" set-mark-command "mark" :bind nil)
  ("p" (set-mark-command 1) "pop" :bind nil)
  ("e" exchange-point-and-mark "exchange")
  ("q" nil "quit"))

;;; Yank / Pop
(declare-function helm-show-kill-ring "")
;;;###autoload(autoload 'nvp-hydra-yank-pop/yank "nvp-basic-auto")
;;;###autoload(autoload 'nvp-hydra-yank-pop/yank-pop "nvp-basic-auto")
(defhydra nvp-hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev"))
  ;; ("l" helm-show-kill-ring "list" :color blue)

;; -------------------------------------------------------------------
;;; Bindings

;;;###autoload
(defun nvp-buffer-local-set-key (key cmd)
  "Like `local-set-key', but don't modify KEY in other buffers of same mode."
  (let ((lmap (make-sparse-keymap)))
    (set-keymap-parent lmap (current-local-map))
    (define-key lmap key cmd)
    (use-local-map lmap)))

;; https://stackoverflow.com/a/14769115/2415684
;;;###autoload
(defun nvp-local-set-minor-mode-key (mode key def)
  "Override a minor mode keybinding for the local buffer only using \
`minor-mode-overriding-map-alist'."
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist)))))
    (define-key newmap key def)))

;; -------------------------------------------------------------------
;;; Assorted

(nvp-declare "" nvp-move-previous-heading nvp-move-forward-heading
  nvp-mode-header-regex)

;;;###autoload
(defun nvp-mark-header-region ()
  "Mark current header region."
  (interactive)
  (condition-case nil
      ;; mark successive header
      (if (use-region-p)
          (set-mark
           (save-excursion
             (goto-char (mark))
             (nvp-move-forward-heading)
             (point)))
        ;; first marking
        (forward-line 0)
        (or (looking-at (nvp-mode-header-regex))
            (nvp-move-previous-heading 'error))
        ;; headers are known at this point
        (save-excursion
         (or (ignore-errors (nvp-move-forward-heading))
             (prog1 (goto-char (point-max))
               (message "Marked to end of buffer (no more headings)")))
         (push-mark (point) nil 'activate)))
    (error (user-error "Can't find header region to mark."))))

;;;###autoload
(defun nvp-kill-emacs ()
  (interactive)
  (save-some-buffers 'no-ask)
  (kill-emacs))

(provide 'nvp-basic-auto)
;;; nvp-basic-auto.el ends here
