;;; nvp-pip.el --- Pip menu -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'json)
(nvp:decls)

(defvar nvp-pip--packages nil)
(defun nvp-pip--packages (&optional recache)
  "Return installed packages as alist."
  (interactive (list current-prefix-arg))
  (and recache (setq nvp-pip--packages nil))
  (or nvp-pip--packages
      (setq nvp-pip--packages
            (with-temp-buffer
              (call-process "pip" nil (current-buffer) nil
                            "list" "--format=json")
              (goto-char (point-min))
              (--when-let (json-read)
                (seq-map (lambda (e) (cons (assoc-default 'name e) e)) it))))))

(defun nvp-pip-jump (pkg loc)
  "Jump to PKG location LOC."
  (interactive
   (let* ((args (transient-args transient-current-command))
          (recache (member "recache" args))
          (pkg (completing-read "Package: " (nvp-pip--packages recache) nil t)))
     (list pkg (or (--first (member it '("homepage" "docs" "source")) args)
                   "homepage"))))
  (if (not (equal "homepage" loc))
      (user-error "unimplemented '%s'" loc)
    (-> (shell-command-to-string
         (concat "pip show --no-input " pkg "| awk '/^Home-page/ { print $2 }'"))
        (string-trim)
        (browse-url))))

(defvar nvp-pip--defaults '("recache" "homepage"))

;;;###autoload(autoload 'nvp-pip-menu "nvp-pip" nil t)
(transient-define-prefix nvp-pip-menu ()
  "Pip menu."
  :value nvp-pip--defaults
  :incompatible '(("homepage" "docs" "source"))
  [["Actions"
    ("o" "Open" nvp-pip-jump)
    ("j" "Open" nvp-pip-jump)]
   ["Open"
    ("h" "Homepage" ("-h" "homepage"))]]
  ["Options"
   ("r" "Recache" ("-r" "recache"))])

(provide 'nvp-pip)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-pip.el ends here
