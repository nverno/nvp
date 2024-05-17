;;; nvp-man.el --- help utils -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Utility functions to parse help output
;; - parse man output
;; - Currently used in sh-help
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'man)

;; -------------------------------------------------------------------
;;; Parsing Man output

;; regex to match man subentry
(defconst nvp-help--man-subentry-re
  "\\([^ \t]\\(?:[^ \t\n\r]\\| [^ \t\n\r]\\)+\\)")

(eval-when-compile
  ;; make indentation based regexp
  (defsubst nvp-help--man-indent ()
    (buffer-substring (point) (+ (point) (current-indentation))))

  (defsubst nvp-help--man-indent-re ()
    (concat "^\\(?:[ \t]*$\\|" (nvp-help--man-indent) "\\)"))

  (defsubst nvp-help--man-section-re ()
    (concat "^" (nvp-help--man-indent) "[^ \t\n\r]")))

;; return section from man doc
(defun nvp-help-man-string (section-re)
  (goto-char (point-min))
  (when (re-search-forward section-re nil 'move)
    ;; (forward-line)
    (beginning-of-line)
    (let* ((start (point))
           (section-re (nvp-help--man-section-re)))
      (forward-line)
      (while (not (or (eobp) (looking-at-p section-re)))
        (forward-line))
      (buffer-substring start (1- (point))))))

;; get switches from man section from START-RE to END-RE
;; default to start of SECTION-RE to beginning of next section
(defun nvp-help-man-switches (&optional section-re start-re end-re)
  (or section-re (setq section-re "^OPTIONS"))
  (or start-re (setq start-re "^\\s-+-"))
  (or end-re (setq end-re Man-heading-regexp))
  (goto-char (point-min))
  (when (re-search-forward section-re)
    (forward-line)
    (let* ((indent-re (nvp-help--man-indent-re))
           (flag-re (concat indent-re nvp-help--man-subentry-re))
           (cont-re "\t[ \t]*\\|^$")
           res key start)
      (when (re-search-forward start-re)
        (beginning-of-line)
        (while (not (looking-at-p end-re))
          (if (not (looking-at flag-re))
              (forward-line)
            (setq key (match-string-no-properties 1))
            ;; get description for key
            (setq start (match-end 0))
            (forward-line)
            (while (looking-at-p cont-re)
              (forward-line))
            (push
             (cons key
                   (replace-regexp-in-string
                    "^\\s-+\\|\t\\|\n$" ""
                    (buffer-substring-no-properties start (1- (point)))))
             res))))
      (nreverse res))))

;; -------------------------------------------------------------------
;;; Command line flags

(defvar nvp-man-flag-re
  (concat "\\(\\(?:--\\|\\+\\+\\)+\\(?:[^,][^,]\\)+\\)"
          ;; optional trailing: [ '[' .* ']' | ' ' arg ] [,]
          "\\(?:\\[[^\\]\n\r]+\\]\\| [^-,\n\r]+\\)?\\(?:,,\\)?"))
(defvar nvp-man--indent-re "^ \\{,16\\}")

(defvar nvp-man--flags (make-hash-table :test #'equal))

(defun nvp-man-flags (topic)
  "Find command-line flags and their locations in the manpage for TOPIC.
Returns list of \\='(flag . line-number)."
  (let ((man-args (Man-translate-references topic)))
    (or (gethash man-args nvp-man--flags)
        (Man-start-calling
         (with-temp-buffer
           (let ((exit-status
                  (call-process shell-file-name nil (list (current-buffer) nil) nil
                                shell-command-switch
                                (format (Man-build-man-command) man-args)))
                 (msg))
             (or (and (numberp exit-status) (= exit-status 0))
                 (and (numberp exit-status)
                      (setq msg (format "exited abnormally with code %d" exit-status)))
                 (setq msg exit-status))
             (if msg (user-error msg)
               (let (flags)
                 (goto-char (point-min))
                 (while (re-search-forward
                         (concat nvp-man--indent-re nvp-man-flag-re) nil t)
                   (let ((line (line-number-at-pos (match-beginning 1))))
                     (push (cons (replace-regexp-in-string "." "" (match-string 1))
                                 line)
                           flags)
                     (while (looking-at (concat ",?\\s-*" nvp-man-flag-re))
                       (push (cons (replace-regexp-in-string "." "" (match-string 1))
                                   line)
                             flags)
                       (goto-char (match-end 0))))
                   ;; (beginning-of-line 2)
                   (forward-line 1))
                 (puthash man-args flags nvp-man--flags)))))))))

;;;###autoload
(defun nvp-man-jump-to-flag ()
  "Jump to flag definition in `Man-mode' buffers."
  (interactive nil Man-mode)
  (--when-let (nvp-man-flags Man-arguments)
    (let ((line (cdr (assoc-string (completing-read "Flag: " it nil t) it))))
      (push-mark)
      (funcall-interactively #'goto-line line))))

(provide 'nvp-man)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-man.el ends here
