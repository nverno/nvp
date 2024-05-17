;;; nvp-ocaml-help.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; #<marker at 5437 in caml-help.el>
;; caml-help:  ocaml-module-alist, ocaml-visible-modules,
;;             ocaml-module-symbols, ocaml-open-module
;; names: tuareg-current-fun-name
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-ocaml)
(with-no-warnings (require 'merlin-company nil t))
(nvp:decls :f (merlin-complete zeal-at-point))

(defvar nvp-ocaml-help-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?' "_" st)
    st))

(eval-when-compile
 (defsubst nvp:ocaml--thingatpt (&optional type)
   (with-syntax-table nvp-ocaml-help-syntax-table
     (thing-at-point (or type 'symbol)))))

(defun nvp-ocaml--desc (str)
  (--when-let (car-safe (merlin-complete str))
    (let ((doc (cdr (assoc 'info it)))
          (type (cdr (assoc 'desc it))))
      (when (and type (string-empty-p type)) (setq type nil))
      (when (and doc (string-empty-p doc)) (setq doc nil))
      (when (or doc type)
        (concat "val " str " : " type
                (and doc (concat "\n\n(** " doc " *)")))))))

;;;###autoload
(cl-defun nvp-hap-merlin (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (nvp:ocaml--thingatpt))
    (doc-buffer 
     (--when-let (nvp-ocaml--desc arg)
       (save-window-excursion
         (with-help-window (help-buffer)
           (with-current-buffer standard-output
             (insert it))
           (list (current-buffer) (point-min) nil)))))))

;;;###autoload
(defun nvp-ocaml-zeal-at-point (arg)
  (interactive "P")
  (with-syntax-table nvp-ocaml-help-syntax-table
    (funcall-interactively 'zeal-at-point arg)))

;; -------------------------------------------------------------------
;;; Library
;; TODO:
;; - better version of `tuareg-browse-library'
;;   should be smart about library path, and offer completing read
;; (nvp-ocaml-read "Module: " :module)
;; - better version of `tuareg-browse-manual'

(nvp:define-cache-runonce nvp-ocaml--manual-index ()
  "Index of modules in online manual."
  (let (res)
    (nvp:with-url-buffer
      "https://caml.inria.fr/pub/docs/manual-ocaml/libref/index_modules.html"
      (goto-char (point-min))
      (while (not (looking-at-p "</head>"))
        (when (looking-at ".*href=\"\\([[:alnum:]]+\\)\.html\"")
          (push (match-string 1) res))
        (forward-line)))
    (nreverse res)))

;;;###autoload
(defun nvp-ocaml-browse-manual-online (module)
  (interactive
   (list (nvp-completing-read "Module: " (nvp-ocaml--manual-index))))
  (browse-url
   (concat "https://caml.inria.fr/pub/docs/manual-ocaml/libref/" module ".html")))

(provide 'nvp-ocaml-help)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ocaml-help.el ends here
