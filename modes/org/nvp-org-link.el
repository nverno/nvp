;;; nvp-org-link.el --- Org links -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (org) :f (org-link-store-props org-element-type))

(autoload 'org-element-map "org-element")


(defmacro nvp-with-org-sections (headline-re &rest body)
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
  (let (no-recurse (prefix-sep "/") types)
    (nvp:skip-keywords body (no-recurse prefix-sep types))
    (unless no-recurse (setq types (cons 'headline (delq 'headline types))))
    (nvp:with-gensyms (sub-mapper)
      `(save-excursion
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
                              (save-restriction
                                (narrow-to-region
                                 (org-element-property :contents-begin it)
                                 (org-element-property :contents-end it)))
                              ,@body))))
                       nil nil ',types))))
           (org-element-map (org-element-parse-buffer) 'headline
             (lambda (it)
               (when (string-match-p ,headline-re
                                     (org-element-property :raw-value it))
                 (funcall ,sub-mapper (org-element-contents it) nil)))
             nil nil org-element-all-elements))))))


;;;###autoload
(defun nvp-org-links (headline-re &optional buffer-or-file type)
  "Gather links of TYPE (or \"https\") nested under sections matching 
HEADLINE-RE in BUFFER if non-nil or current buffer . 
Return cons of \\='(name                           . raw-link)."
  (let ((buf (if buffer-or-file
                 (or (get-buffer buffer-or-file)
                     (find-file-noselect buffer-or-file))
               (current-buffer)))
        res)
    (with-current-buffer buf
      (nvp-with-org-sections headline-re
        :types (link)
        (when (or (null type)
                  (string-match-p type (org-element-property :type it)))
          (push (list
                 (concat
                  (and (length> prefix 0) (concat prefix "/"))
                  (when-let* ((beg (org-element-property :contents-begin it))
                              (end (org-element-property :contents-end it)))
                    (and beg end (buffer-substring-no-properties beg end))))
                 'org-link it)
                res))))
    (nreverse res)))


;; -------------------------------------------------------------------
;;; Custom Link

;; Link format:
;; nvp:library ( '?' section-or-def ( '&' 'type=' (v|f|s) )? )?
(defvar nvp-org-nvp-re
  "\\([^?]+\\)\\(?:[?]\\([^&]+\\)\\)?\\(?:&type=\\([vfs]\\)\\)?"
  "Match parts of link query.")

(defsubst nvp-org--nvp-parse (query)
  (when (string-match nvp-org-nvp-re query)
    (list (match-string 1 query)
          (match-string 2 query)
          (pcase (match-string 3 query)
            ("f" 'function)
            ("s" 'section)
            ("v" 'variable)
            (_ nil)))))

(defun nvp-org-nvp-open (query)
  "Visit nvp FILE-SECTION and goto SECTION if non-nil."
  (-let (((file sec-or-def type) (nvp-org--nvp-parse query)))
    (--if-let (nvp:locate-library file)
        (with-current-buffer (find-file-noselect it)
          (let ((cur (point)) pt)
            (when sec-or-def
              (goto-char (point-min))
              (let* ((prefix-re (if (or (not type) (eq type 'section))
                                    (concat (nvp:heading-create-re) "*")
                                  "^\\s-*(def.*"))
                     (case-fold-search t))
                (when (re-search-forward (concat prefix-re sec-or-def) nil t)
                  (setq pt (nvp:point 'bol)))))
            (pop-to-buffer (current-buffer))
            (unless (or (null pt) (eq pt cur))
              (push-mark))
            (and pt (goto-char pt))
            (recenter-top-bottom)))
      (user-error "No library found for \"%s\"" file))))

(defun nvp-org-nvp-export (file-section desc backend _)
  (-let* (((file _section) (split-string file-section "?"))
          (lib (or (nvp:locate-library file) file)))
    (pcase backend
      (`texinfo (format "@uref{%s,%s}" lib desc))
      (_ lib))))

(defun nvp-org-nvp-store-link (arg &optional _interactive?)
  "Store org \\='nvp link."
  (when-let* ((lib (and (eq major-mode 'emacs-lisp-mode)
                       (file-name-base (buffer-file-name)))))
    (let* ((prompt (> (prefix-numeric-value arg) 1))
           (symt (cond ((nvp:ppss 'cmt) (cons 's nil))
                       (t (cons 'f (nvp-parse-current-function))))))
      (when (or prompt (null (cdr symt)))
        (let* ((typ (nvp:read-char-case "Type: " 'verbose
                      (?s "[s]ection" 's)
                      (?f "[f]unction" 'f)
                      (?v "[v]ariable" 'v)))
               (def (and (eq (car symt) typ) (cdr symt)))
               (sym (read-string "Thing: " def nil def)))
          (setq symt (cons typ sym))))
      (and symt
           (org-link-store-props
            :type "nvp"
            :link (format "nvp:%s?%s&type=%s" lib (cdr symt) (car symt))
            :description (format "%s" (cdr symt)))))))


(provide 'nvp-org-link)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-org-link.el ends here
