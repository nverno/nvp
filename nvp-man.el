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

(provide 'nvp-man)
;;; nvp-man.el ends here
