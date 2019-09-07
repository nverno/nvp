;;; nvp-ocaml-inf.el --- ocaml REPL -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'utop)
(nvp-decls)

;; default syntax breaks motions
(let ((tab utop-mode-syntax-table))
  (dolist (c (string-to-list ";.,_-^:@#%!~"))
    (modify-syntax-entry c "_" tab)))

(nvp-bindings nvp-utop-fast-map nil
  :create t :repeat t :indicate t
  ("n" . nvp-utop-next-prompt)
  ("p" . nvp-utop-previous-prompt))

(defvar nvp-utop-prompt-regexp "utop[\\[0-9\\]+]> ")

;; drop-in replacements for `comint-next-prompt'/...
(defun nvp-utop-next-prompt (n)
  (interactive "p")
  (let ((paragraph-start nvp-utop-prompt-regexp))
    (end-of-line (if (> n 0) 1 0))
    (forward-paragraph n)
    (if (and (looking-at nvp-utop-promp-regexp)
             (<= (match-end 0) (line-end-position)))
        (goto-char (match-end 0)))))

(defun nvp-utop-previous-prompt (n)
  (interactive "p")
  (nvp-utop-next-prompt (- n)))

;; switch b/w inferior and source buffers
;;;###autoload
(defun nvp-utop-switch-buffers ()
  (interactive)
  (switch-to-buffer-other-window
   (if (eq major-mode 'utop-mode)
       (-if-let (proc (nvp-buffer-process))
           (let ((src-buf (process-get proc :src-buffer)))
             (or (and (buffer-live-p src-buf) src-buf)
                 (other-buffer (current-buffer) 'visible)))
         (other-buffer (current-buffer) 'visible))
     (let ((src (current-buffer)))
       (utop-prepare-for-eval)
       (with-current-buffer (get-buffer utop-buffer-name)
         (process-put (nvp-buffer-process) :src-buffer src)
         (current-buffer))))))

(defun nvp-utop-return ()
  (interactive)
  (goto-char (line-end-position))
  (insert ";;")
  (call-interactively 'utop-eval-input-or-newline))

;; (9/7/19) bug in utop-eval-phrase
(defun nvp-utop-eval-phrase (&optional arg)
  (interactive "P")
  (if arg (tuareg-eval-phrase)
    (utop-prepare-for-eval)
    (let (end)
      (save-excursion
        (-let (((beg . end) (funcall utop-discover-phrase)))
          (nvp-indicate-pulse-region-or-line beg end)
          (utop-eval beg end))))))

(provide 'nvp-ocaml-inf)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ocaml-inf.el ends here
