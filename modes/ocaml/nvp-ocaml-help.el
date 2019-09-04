;;; nvp-ocaml-help.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-hap)
  (require 'nvp-ocaml))
(require 'nvp-ocaml)
(require 'merlin-company nil t)
(nvp-decl merlin/complete zeal-at-point)
(autoload 'ocaml-module-alist "caml-help")

;; #<marker at 103230 in tuareg.el>
;; #<marker at 5437 in caml-help.el>
;; caml-help:  ocaml-module-alist, ocaml-visible-modules,
;;             ocaml-module-symbols, ocaml-open-module
;; #<marker at 99381 in tuareg.el>
;; names: tuareg-current-fun-name

(defvar nvp-nvp-ocaml-help-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st))

;; -------------------------------------------------------------------
;;; Help at point

;; display help for function at point in popup tooltip
;;;###autoload
(defun nvp-ocaml-help-at-point ()
  (interactive)
  (with-syntax-table nvp-nvp-ocaml-help-syntax-table
    (let ((candidate (thing-at-point 'symbol)))
      (when candidate
        (let ((info (car-safe (merlin/complete candidate))))
          (when info
            (let ((doc (cdr (assoc 'info info)))
                  (type (cdr (assoc 'desc info))))
              (nvp-with-toggled-tip
                (concat "val " candidate " : " type "\n\n(** " doc " *)")))))))))

;;;###autoload
(defun nvp-ocaml-help-zeal-at-point (arg)
  (interactive "P")
  (with-syntax-table nvp-nvp-ocaml-help-syntax-table
    (funcall-interactively 'zeal-at-point arg)))

;; -------------------------------------------------------------------
;;; Library
;; TODO:
;; - better version of `tuareg-browse-library'
;;   should be smart about library path, and offer completing read
;; (nvp-ocaml-read "Module: " :module)

;; TODO:
;; - better version of `tuareg-browse-manual'

(defvar nvp-ocaml-help-cheatsheets
  '("ocaml-lang" "ocaml-tools" "ocaml-stdlib" "tuareg-mode" "all"))

(defvar nvp-ocaml-help-topics
  '((".merlin" .
     "https://github.com/the-lambda-church/merlin/wiki/project-configuration")
    ("cheatsheets" . 'nvp-ocaml-help-cheatsheets)
    ("project" . "https://github.com/kmicinski/example-ocaml-merlin")
    ("performance" .
     "https://janestreet.github.io/ocaml-perf-notes.html")))

;; read index of modules in online manual
(nvp-define-cache-runonce nvp-ocaml-help-manual-modules ()
  "Index of online modules."
  (let (res)
    (nvp-with-url-buffer
      "https://caml.inria.fr/pub/docs/manual-ocaml/libref/index_modules.html"
      (goto-char (point-min))
      (while (not (looking-at-p "</head>"))
        (when (looking-at ".*href=\"\\([[:alnum:]]+\\)\.html\"")
          (push (match-string 1) res))
        (forward-line)))
    (nreverse res)))

;; lookup help for module in online manual with completing read
;;;###autoload
(defun nvp-ocaml-help-browse-manual-online (module)
  (interactive
   (list (nvp-ocaml-read "Module: " (nvp-ocaml-help-manual-modules))))
  (browse-url
   (concat "https://caml.inria.fr/pub/docs/manual-ocaml/libref/"
           module ".html")))

;;;###autoload
(defun nvp-ocaml-help-online (topic)
  (interactive
   (list (cdr (assoc (nvp-ocaml-read "Topic: " 'nvp-ocaml-help-topics)
                     nvp-ocaml-help-topics))))
  (pcase topic
    (`(quote ,sym)
     (call-interactively sym))
    ((pred stringp)
     (browse-url topic))))

(defun nvp-ocaml-help-cheatsheets (sheet)
  (interactive
   (list (nvp-ocaml-read "Cheatsheet: " nvp-ocaml-help-cheatsheets)))
  (browse-url
   (if (string= "all" sheet)
       "https://www.typerex.org/cheatsheets.html"
     (format "https://www.typerex.org/files/cheatsheets/%s.pdf"
             sheet))))

(provide 'nvp-ocaml-help)
;;; nvp-ocaml-help.el ends here
