;;; test helper functions/macros
(require 'nvp)
(require 'ert)
(require 'cl-lib)

(defun nvp-test-buffer-setup (&optional mode)
  (if mode (funcall mode) (emacs-lisp-mode))
  (setq-local indent-tabs-mode nil)
  (setq-local comment-column 40)
  (setq-local show-trailing-whitespace nil)
  (setq-local use-hard-newlines nil)
  (electric-indent-local-mode))

(defun nvp-test-set-mark-and-point ()
  "Replace '_' and set mark there. Then replace '|' and leave point there."
  (let (end-char)
    (when (search-forward "_" nil 'move)
      (delete-char -1)
      (set-mark (point)))
    (goto-char (point-min))
    (when (search-forward "|")
      (delete-char -1)
      (setq end-char ?|))
    end-char))

(defmacro nvp--buffer-should-change (from to &optional mode &rest body)
  "Buffer contents should change FROM to TO.
If '_' is present in FROM text, the mark is set there.
If '|' is present in TO text, the point is expected to end there."
  (declare (indent defun))
  `(with-temp-buffer
     (nvp-test-buffer-setup ,mode)
     (save-excursion (insert ,from))
     (let ((end-char (nvp-test-set-mark-and-point)))
       ,@body
       (and end-char (insert end-char)))
     (should (string= (buffer-string) ,to))))

(provide 'nvp-test-helpers)
