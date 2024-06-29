;;; nvp-npm.el --- npm menu -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls)

(defvar-local nvp-npm--packages nil)
(defun nvp-npm--packages (&optional recache)
  "Return list of local npm packages, optionally RECACHING results."
  (or (and (null recache) nvp-npm--packages)
      ;; "npm list --depth=0 --only=dev --parseable | awk -F/ '{print $NF}'"
      (--> (-> "npm list --parseable --json 2>/dev/null|jq -r '.dependencies|keys[]'"
               (shell-command-to-string)
               (string-trim)
               (string-split))
           (setq nvp-npm--packages it))))

(defun nvp-npm-jump (pkg loc)
  "Jump to PKG location LOC."
  (interactive
   (let* ((args (transient-args transient-current-command))
          (recache (member "recache" args))
          (pkg (completing-read "Package: " (nvp-npm--packages recache) nil t)))
     (list pkg (or (--first (member it '("homepage" "docs" "repository")) args)
                   "homepage"))))
  (let ((path (pcase loc
                ("homepage" ".homepage")
                ("repository" ".repository.url")
                ("docs" (user-error "unimplemented")))))
    (--> (-> (format "npm show --json %s 2>/dev/null | jq -r '%s'" pkg path)
             (shell-command-to-string)
             (string-trim))
         (replace-regexp-in-string
          (rx (or (seq ".git" eos) (seq bos "git+"))) "" it)
         (browse-url it))))


(defun nvp-npm-search (query &rest args)
  "Search npm packages for QUERY."
  (interactive
   (let ((args (transient-args transient-current-command)))
     (list (read-string "Query: ") args)))
  (ignore args)
  (with-current-buffer (get-buffer-create "*npm-search*")
    (erase-buffer)
    (call-process-shell-command
     (concat "npm search --parseable --searchlimit=5 --json " query "|"
             "jq -r '.[] as{$name,$description,links:{$npm,$homepage}} | "
             "\"\\($name)\\n\\t\\($description)\\n\\t\\($homepage)\"'")
     nil (current-buffer) t)
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))
    (goto-address-mode)))

(defvar nvp-npm--defaults '("recache" "homepage"))

;;;###autoload(autoload 'nvp-npm-menu "nvp-npm" nil t)
(transient-define-prefix nvp-npm-menu ()
  "Npm menu."
  :value nvp-npm--defaults
  :incompatible '(("homepage" "repository" "docs"))
  [["Actions"
    ("o" "Open" nvp-npm-jump)
    ("j" "Open" nvp-npm-jump)
    ("s" "Search" nvp-npm-search)]
   ["Open"
    ("h" "Homepage" ("-h" "homepage"))
    ("r" "Repository" ("-r" "repository"))]]
  ["Options"
   ("-r" "Recache" ("-r" "recache"))])

(provide 'nvp-npm)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-npm.el ends here
