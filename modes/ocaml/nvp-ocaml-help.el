;;; nvp-ocaml-help.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; #<marker at 5437 in caml-help.el>
;; caml-help:  ocaml-module-alist, ocaml-visible-modules,
;;             ocaml-module-symbols, ocaml-open-module
;; names: tuareg-current-fun-name
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-hap)
(require 'nvp-ocaml)
(with-no-warnings (require 'merlin-company nil t))
(nvp-decls :f (merlin/complete zeal-at-point))

(defvar nvp-ocaml-help-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st))

;; display help for function at point in popup tooltip
;;;###autoload
(defun nvp-ocaml-help-at-point ()
  (interactive)
  (with-syntax-table nvp-ocaml-help-syntax-table
    (let ((candidate (thing-at-point 'symbol)))
      (when candidate
        (let ((info (car-safe (merlin/complete candidate))))
          (when info
            (let ((doc (cdr (assoc 'info info)))
                  (type (cdr (assoc 'desc info))))
              (nvp-with-toggled-tip
                (concat "val " candidate " : " type "\n\n(** " doc " *)")))))))))

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

(nvp-define-cache-runonce nvp-ocaml--manual-index ()
  "Index of modules in online manual."
  (let (res)
    (nvp-with-url-buffer
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
;;; nvp-ocaml-help.el ends here
