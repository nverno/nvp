;;; nvp-scheme-chicken.el --- chicken scheme -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'scheme)
(require 'nvp)

;; From: https://www.reddit.com/r/emacs/comments/br7q0y/weekly_tipstricketc_thread/
;; extend fontification for multiline comments in chicken scheme
(defun nvp-scheme-region-extend-function ()
  (when (not (get-text-property (point) 'font-lock-multiline))
    (let* ((heredoc nil)
           (new-beg
            (save-excursion
              (when (and (re-search-backward "#>\\|<#\\|#<[<#]\\(.*\\)$" nil t)
                         (not (get-text-property (point) 'font-lock-multiline)))
                (let ((match (match-string 0))
                      (tag (match-string 1)))
                  (cond
                   ((equal match "#>") (point))
                   ((string-match-p "^#<[<#]" match) (setq heredoc tag) (point)))))))
           (new-end
            (save-excursion
              (if heredoc
                  (when (and (re-search-forward (concat "^" (regexp-quote heredoc) "$") nil t)
                             (not (get-text-property (point) 'font-lock-multiline)))
                    (point))
                (when (and (re-search-forward "#>\\|<#" nil t)
                           (not (get-text-property (point) 'font-lock-multiline))
                           (equal (match-string 0) "<#"))
                  (point))))))
      (when (and new-beg new-end)
        ;; (setq font-lock-beg new-beg)
        ;; (setq font-lock-end new-end)
        (with-silent-modifications
          (put-text-property new-beg new-end 'font-lock-multiline t))
        (cons new-beg new-end)))))

(defun nvp-scheme-syntax-propertize-foreign (_ end)
  (save-match-data
    (when (search-forward "<#" end t)
      (with-silent-modifications
        (put-text-property (1- (point)) (point)
                           'syntax-table (string-to-syntax "> cn"))))))

(defun nvp-scheme-syntax-propertize-heredoc (_ _end)
  (save-match-data
    (let ((tag (match-string 2)))
      (when (and tag (re-search-forward (concat "^" (regexp-quote tag) "$") nil t))
        (with-silent-modifications
          (put-text-property (1- (point)) (point)
                             'syntax-table (string-to-syntax "> cn")))))))

;; overwrites function from scheme.el
(defun scheme-syntax-propertize (beg end)
  (goto-char beg)
  (scheme-syntax-propertize-sexp-comment (point) end)
  (funcall
   (syntax-propertize-rules
    ("\\(#\\);"
     (1 (prog1 "< cn" (scheme-syntax-propertize-sexp-comment (point) end))))
    ("\\(#\\)>"
     (1 (prog1 "< cn" (nvp-scheme-syntax-propertize-foreign (point) end))))
    ("\\(#\\)<[<#]\\(.*\\)$"
     (1 (prog1 "< cn" (nvp-scheme-syntax-propertize-heredoc (point) end)))))
   (point) end))

(defun nvp-scheme-chicken-setup ()
  (setq font-lock-extend-region-functions
        (cons 'nvp-scheme-region-extend-function
              font-lock-extend-region-functions)))

;; (add-hook 'scheme-mode-hook 'nvp-scheme-chicken-setup)

(provide 'nvp-scheme-chicken)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-scheme-chicken.el ends here
