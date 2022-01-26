;;; nvp-org-subrs.el --- compile time -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'nvp-macro)
(require 'org-element)

(define-inline nvp:org-property (property element)
  (inline-quote 
   ,(if (stringp element)
        (inline-quote (get-text-property 0 ,property ,element))
      (inline-quote (plist-get (nth 1 ,element) ,property)))))

;; see `org-element-link-parser' for link structure:
;; links are list where car is a `link' and cdr is plist
(defsubst nvp:org-link-name (link)
  (-let (((&plist :contents-begin beg :contents-end end) (nth 1 link)))
    (and beg end (buffer-substring-no-properties beg end))))

(defmacro nvp:with-org-sections (headline-re &rest body)
  "Execute BODY in narrowed regions under headlines matching HEADLINE-RE.
IT is bound to parse tree in BODY."
  (declare (indent 1))
  (require 'org-element)
  (nvp:with-gensyms (tree)
    `(save-restriction
       (let ((,tree (org-element-parse-buffer)))
         (nreverse
          (org-element-map ,tree 'headline
            (lambda (it)
              (when (string-match-p
                     ,headline-re (nvp:org-property :raw-value it))
                (narrow-to-region
                 (nvp:org-property :contents-begin it)
                 (nvp:org-property :contents-end it))
                ,@body))))))))

(provide 'nvp-org-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-org-subrs.el ends here
