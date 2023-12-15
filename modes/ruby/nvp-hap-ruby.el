;;; nvp-hap-ruby.el --- yari help-at-point -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-hap)
(require 'ruby-mode)
(nvp:decls :p (yari ruby) :f (yari-ri-lookup))

(defun nvp-hap--yari-thingatpt (arg)
  (let ((sym (thing-at-point 'symbol t)))
    (pcase (nvp-hap-prefix-action arg)
      (`(prompt . ,_)
       (completing-read "Yari: " (yari-ruby-obarray) nil t sym))
      (_ (when sym
           (or (and (null (string-search "." sym))
                    (try-completion sym (yari-ruby-obarray)))
               (try-completion
                (if (let ((case-fold-search nil))
                      (string-match-p "\\`[A-Z]" sym))
                    (replace-regexp-in-string "[.]" "#" sym)
                  sym)
                (yari-ruby-obarray))
               (completing-read
                "Yari: "
                (completion-pcm--all-completions
                 "" `(any ,(concat
                            "#" (car (last (split-string sym "[.]" t)))))
                 (yari-ruby-obarray) nil))))))))

;;;###autoload
(defun nvp-hap-yari (command &optional arg &rest _args)
  (cl-case command
    (init (require 'yari nil t))
    (thingatpt (nvp-hap--yari-thingatpt arg))
    (doc-buffer (--when-let (yari-ri-lookup arg)
                  (with-current-buffer (nvp-hap-doc-buffer (ansi-color-apply it))
                    (yari-mode)
                    (list (current-buffer)))))))

(provide 'nvp-hap-ruby)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-ruby.el ends here
