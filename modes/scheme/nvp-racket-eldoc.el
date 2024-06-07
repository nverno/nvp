;;; nvp-racket-eldoc.el --- racket eldoc -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'racket-eldoc nil t)
(nvp:decls :p (racket))


;; `racket--do-eldoc': returns symbol in addition to docstring
(defun nvp-racket--do-eldoc (how repl-session-id)
  (and (racket--cmd-open-p)
       (> (point) (point-min))
       (save-excursion
         (condition-case _
             ;; The char-before and looking-at-p checks below are to
             ;; skip when the sexp is quoted or when its first elem
             ;; couldn't be a Racket function name.
             (let* ((beg (progn
                           (backward-up-list)
                           (and (not (memq (char-before) '(?` ?' ?,)))
                                (progn (forward-char 1) (point)))))
                    (beg (and beg (looking-at-p "[^0-9#'`,\"]") beg))
                    (end (and beg (progn (forward-sexp) (point))))
                    (end (and end
                              (char-after (point))
                              (eq ?\s (char-syntax (char-after (point))))
                              end))
                    (sym (and beg end (buffer-substring-no-properties beg end)))
                    (how (racket-how-front-to-back how))
                    (str (and sym (racket--cmd/await repl-session-id
                                                     `(type ,how ,sym)))))
               (cons sym str))
           (scan-error nil)))))


(defvar nvp-racket--eldoc-cache (make-hash-table :test #'equal))

(defun nvp-racket--format-param-type (par-info)
  "Format param for eldoc with type info and/or defaults from PAR-INFO."
  (let* ((parts (string-split par-info "=" t " "))
         (par-type (string-split (pop parts) " :" t  " "))
         (default (pop parts))
         (par-name (pop par-type))
         (par-str (concat
                   (propertize par-name 'face 'font-lock-variable-name-face)
                   ": " (propertize
                         (string-join par-type " ") 'face 'font-lock-type-face)
                   (when default
                     (concat (propertize " = " 'face 'font-lock-operator-face)
                             default)))))
    (cons par-name par-str)))

(defun nvp-racket--format-signature (fn-sig &optional rtype par-types)
  "Format func signature FN-SIG with return type RTYPE and param types PAR-TYPES."
  (cl-loop for (par . psig) in par-types
           do (setq fn-sig (replace-regexp-in-string
                            (concat "\\_<" (regexp-quote par) "\\_>")
                            psig fn-sig)))
  (if rtype
      (concat fn-sig (propertize " -> " 'face 'font-lock-operator-face) rtype)
    fn-sig))

(defun nvp-racket--parse-signature ()
  "Parse single signature, return type, and param types."
  (let ((end (progn (forward-sexp)
                    (point)))
        rtype par-types)
    ;; Return type
    (when (looking-at "\\s-*->\\s-*")
      (goto-char (match-end 0))
      (setq rtype (propertize
                   (string-trim (buffer-substring (point) (line-end-position)))
                   'face 'font-lock-type-face))
      (forward-line 1))
    (and (looking-at-p "\\s-*$")
         (forward-line 1))
    ;; Parameter types
    (while (not (or (eobp)
                    ;; alternative signature
                    (looking-at-p "\\s-*(")))
      ;; XXX: Ignoring ebnf alternatives for now
      (if (looking-at-p "^\\s-*\\(?:|\\|[[:alnum:]-]+\\s-*=\\|$\\)")
          (forward-line 1)
        (let ((beg (point)))
          (forward-line 1)
          ;; Default value
          (when (looking-at "^\\s-*= ")
            (goto-char (match-end 0))
            (forward-sexp))
          (push (nvp-racket--format-param-type
                 (buffer-substring beg (1- (point))))
                par-types))))
    (let ((sig-str (buffer-substring (point-min) end)))
      (delete-region (point-min) (point))
      (nvp-racket--format-signature sig-str rtype par-types))))

(defun nvp-racket--eldoc-format (sym docstring)
  "Return formatted eldoc DOCSTRING for SYM."
  (or (gethash sym nvp-racket--eldoc-cache)
      (let ((sigs (with-temp-buffer
                    (insert docstring)
                    (racket-mode)
                    (font-lock-fontify-region (point-min) (point-max))
                    (goto-char (point-min))
                    (let (sig sigs)
                      (while (and (not (eobp))
                                  (setq sig (nvp-racket--parse-signature)))
                        (push sig sigs))
                      sigs))))
        (setf (gethash sym nvp-racket--eldoc-cache) (nreverse sigs)))))

;;;###autoload
(defun nvp-racket-eldoc-function (callback &rest _ignored)
  "Eldoc for `eldoc-documentation-functions' in `racket-mode'."
  (pcase-let ((`(,sym . ,doc) (nvp-racket--do-eldoc
                               (racket--buffer-file-name) nil)))
    (when (and sym doc)
      (let ((sigs (nvp-racket--eldoc-format sym doc)))
        (funcall callback
                 (if (length> sigs 1)
                     ;; Align alternative signatures
                     (mapconcat 'identity sigs
                                (concat "\n" (make-string (+ 2 (length sym)) ? )))
                   (car sigs))
                 :thing sym
                 :face 'font-lock-function-name-face)))))

(provide 'nvp-racket-eldoc)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-racket-eldoc.el ends here
