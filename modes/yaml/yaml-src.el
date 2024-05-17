;;; yaml-src.el --- formatting code sections in yaml -*- lexical-binding: t; -*-

;;; Commentary:

;; Edit code sections in separate buffer in the corresponding
;; major-mode.

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar yaml-src--prev-buffer))
(nvp:auto "nvp-read" 'nvp-read-mode)

;; amount to indent source blocks in addition to
;; baseline yaml indent at location
(defvar yaml-src-indent-offset 4)

;; move point back to previous item: - [lang]:
(defun yaml-src--back-to-item ()
  (beginning-of-line)
  (while (not (or (bobp) (looking-at-p "^\\s-*-")))
    (forward-line -1)))

;; find what type of code this should be
(defun yaml-src--lang ()
  (save-excursion
    (yaml-src--back-to-item)
    (let ((lang (and (looking-at "^\\s-*-\\s-*\\([^: \n\t]+\\)")
                     (match-string-no-properties 1))))
      (pcase lang
        ("ps" 'powershell-mode)
        ("cmd" 'cmd-mode)
        ("el" 'emacs-lisp-mode)
        ("perl" 'cperl-mode)
        ("ruby" 'ruby-mode)
        ("py" 'python-mode)
        ("sh" 'sh-mode)
        ("cl" 'lisp-mode)
        ("rs" 'rust-mode)
        ("go" 'go-mode)
        ("sed" 'sed-mode)
        ("awk" 'awk-mode)
        ("m" 'octave-mode)
        ("R" 'ess-mode)
        ("jl" 'julia-mode)
        ("hs" 'haskell-mode)
        ("sql" 'sql-mode)
        ("js" 'js2-mode)
        (_ (intern (nvp-read-mode)))))))

;; find the baseline indentation, add OFFSET or 4 by default
(defun yaml-src--indent ()
  (save-excursion
    (yaml-src--back-to-item)
    (back-to-indentation)
    (+ yaml-src-indent-offset (current-column))))

;; make a buffer for editing source code in `mode' and pop to it
(defun yaml-src--buffer (mode &optional code)
  (let ((prev (current-buffer))
        (buff (get-buffer-create
               (concat "*yaml-src [" (symbol-name mode) "]*"))))
    (with-current-buffer buff
      (kill-all-local-variables)
      (funcall mode)
      (yaml-src-mode)
      (setq-local yaml-src--prev-buffer prev)
      (when code
        (insert code))
      (pop-to-buffer (current-buffer)))))

;; edit source code at point: copy to another buffer in the proper
;; mode, edit there, then insert back into buffer
;;;###autoload
(defun yaml-src-edit-src (&optional start end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))))
  (let ((lang (yaml-src--lang))
        (code (if (and start end)
                  (buffer-substring-no-properties start end))))
    (if (not lang)
        (user-error "Language not recognized.")
      (when code
        (delete-region start end))
      (yaml-src--buffer lang code))))

;; ------------------------------------------------------------
;;; Minor mode

(defvar yaml-src-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-x C-s") 'yaml-src-edit-exit)
    km))

(define-minor-mode yaml-src-mode
  "Yaml code minor mode."
  :lighter " YamlCode")

;; kill editing buffer and insert code back into OG
(defun yaml-src-edit-exit ()
  (interactive)
  (let ((code (buffer-string))
        (prev (bound-and-true-p yaml-src--prev-buffer)))
    (kill-buffer (current-buffer))
    (with-current-buffer prev
      (yaml-src-insert code)
      (pop-to-buffer (current-buffer)))))

;; insert code back into yaml preserving indent
(defun yaml-src-insert (code)
  (let ((base (yaml-src--indent))
        (start (point))
        (_ (insert code))
        (end (line-number-at-pos)))
    (goto-char start)
    (while (not (or (eobp)
                    (> (line-number-at-pos) end)))
      (indent-to base)
      (forward-line 1))))

(provide 'yaml-src)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; yaml-src.el ends here
