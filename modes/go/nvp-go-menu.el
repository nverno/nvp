;;; nvp-go-menu.el --- Go menu -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls)

(defvar-local nvp-go--packages nil)
(defun nvp-go--packages (&optional recache)
  (or (and (null recache) nvp-go--packages)
      (let ((buf (generate-new-buffer "go-packages")))
        (unwind-protect
            (let ((stat (call-process-shell-command
                         "go list -f '{{.ImportPath}}' all"
                         nil (list buf nil))))
              (message "Status: %S" stat)
              (when (zerop stat)
                (setq nvp-go--packages
                      (with-current-buffer buf
                        (string-split (string-trim (buffer-string)))))))
          (kill-buffer buf)))))


;;;###autoload
(defun nvp-go-menu-jump (pkg loc)
  "Jump to PKG location LOC."
  (interactive
   (let* ((args (transient-args transient-current-command))
          (recache (member "recache" args))
          (pkg (completing-read "Package: "
                 (nvp-go--packages recache) nil t)))
     (list pkg
           ;; TODO(7/12/24): homepage, docs, repo
           (or (--first
                (member it '("source" "homepage" "docs" "repository")) args)
               "source"))))
  (let* ((field (pcase loc
                  ("source" ".Dir")
                  (_ (user-error "unimplemented"))))
         (uri (-> (format "go list -f '{{%s}}' %s" field pkg)
                  (shell-command-to-string)
                  (string-trim))))
    (if (equal loc "source")
        (if (file-directory-p uri)
            (dired-jump-other-window (file-name-as-directory uri))
          (find-file-other-window uri))
      (browse-url
       (replace-regexp-in-string
        (rx (or (seq ".git" eos) (seq bos "git+"))) "" uri)))))


(defun nvp-go-menu-search (query &rest args)
  (interactive
   (let ((args (transient-args transient-current-command)))
     (list (read-string "Query: ") args)))
  (ignore query args)
  ;; TODO(7/12/24):
  (user-error "unimplemented"))


(defvar nvp-go-menu-defaults '("recache" "source"))

;;;###autoload(autoload 'nvp-go-menu "nvp-go-menu" nil t)
(transient-define-prefix nvp-go-menu ()
  "Go menu."
  :value nvp-go-menu-defaults
  :incompatible '(("source" "homepage" "docs" "repository"))
  [["Actions"
    ;; ("s" "Search" nvp-go-menu-search)
    ("o" "Open" nvp-go-menu-jump)
    ("j" "Open" nvp-go-menu-jump)]
   ["Open"
    ("m" "Source" ("-m" "source"))
    ("r" "Repo" ("-r" "repository"))
    ("h" "Homepage" ("-h" "homepage"))
    ("d" "Docs" ("-d" "docs"))]]
  ["Options"
   ("-r" "Recache" ("-r" "recache"))])


(provide 'nvp-go-menu)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-go-menu.el ends here
