;;; nvp-org-subrs.el --- compile time -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'nvp-macro)
(require 'org-element)

;; see `org-element-link-parser' for link structure:
;; links are list where car is a `link' and cdr is plist
(defsubst nvp:org-link-name (link)
  (-let (((&plist :contents-begin beg :contents-end end) (nth 1 link)))
    (and beg end (buffer-substring-no-properties beg end))))

(defmacro nvp:with-org-sections (headline-re &rest body)
  "Execute BODY in narrowed regions under headlines matching HEADLINE-RE.
\\='it is bound to parse tree in BODY, \\='it-type is bound the element
 type, and \\='prefix is bound to string of headings under which \\='it
 appears.

Optional keywords:
\\='types         types to match when recursing under matching headlines
                  defaults to matching headings under top-level matches
\\=':prefix-sep   default \"/\", separates prefix
\\='no-recurse    don't recurse under matching headlines"
  (declare (indent 1))
  (require 'org-element)
  (let (no-recurse (prefix-sep "/") types)
    (nvp:skip-keywords body (no-recurse prefix-sep types))
    (unless no-recurse (setq types (cons 'headline (delq 'headline types))))
    (nvp:with-gensyms (sub-mapper)
      `(save-restriction
         (letrec ((,sub-mapper
                   (lambda (it prefix)
                     (org-element-map it ',types
                       (lambda (it)
                         (let ((it-type (org-element-type it)))
                           (,@(if no-recurse '(progn)
                                `(if (eq it-type 'headline)
                                     (funcall
                                      ,sub-mapper (org-element-contents it)
                                      (concat
                                       (and prefix (concat prefix ,prefix-sep))
                                       (org-element-property :raw-value it)))))
                            (when (memq it-type ',types)
                              (narrow-to-region
                               (org-element-property :contents-begin it)
                               (org-element-property :contents-end it))
                              ,@body))))
                       nil nil ',types))))
           (org-element-map (org-element-parse-buffer) 'headline
             (lambda (it)
               (when (string-match-p ,headline-re
                                     (org-element-property :raw-value it))
                 (funcall ,sub-mapper (org-element-contents it) nil)))
             nil nil org-element-all-elements))))))

(provide 'nvp-org-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-org-subrs.el ends here
