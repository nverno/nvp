;;; nvp-info.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 20:23:53>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 31 January 2019

;;; Commentary:

;; info helpers
;; - gather my nodes
;; - update when file changes
;; - convert org to info and install
;; - jump to source
;; - imenu support

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (nvp-local-vars))
(require 'nvp)
(require 'info)
(require 'filenotify)

(defvar nvp-info-nodes-need-refresh () "Update list when 'dir' changes.")
(nvp-function-with-cache nvp-info-nodes ()
  "List of my info manuals."
  :predicate (not nvp-info-nodes-need-refresh)
  (setq nvp-info-nodes-need-refresh nil)
  (with-temp-buffer
    (insert-file-contents-literally (expand-file-name "dir" nvp/info))
    (goto-char (point-min))
    (search-forward "* Menu")
    (let (nodes)
      (while (re-search-forward
              "^\\*[^\:]+:\\s-*(\\([^)]+\\))\\." nil 'move)
        (push (match-string-no-properties 1) nodes))
      nodes)))

(eval-when (load)
  (file-notify-add-watch (expand-file-name "dir" nvp/info) (list 'change)
                         #'(lambda (&rest _args) (setq nvp-info-nodes-need-refresh t))))

;;;###autoload
(defun nvp-info-open (topic &optional bname)
  "Open info on TOPIC in BNAME."
  (interactive (list (nvp-completing-read "Topic: " (nvp-info-nodes))))
  (let ((buff (or bname (concat "*" topic " info*"))))
    (if (get-buffer buff)
        (progn
          (switch-to-buffer buff)
          (unless (string-match topic Info-current-file)
            (Info-goto-node (format "(%s)" topic))))
      (info topic buff))))

;;;###autoload
(defun nvp-info-install (file)
  "Install org texinfo FILE into info directory."
  (interactive
   (list (ido-read-file-name "File: " (expand-file-name "org" nvp/info))))
  (let ((default-directory (expand-file-name "org" nvp/info))
        (target
         (concat "install-"
                 (file-name-nondirectory (file-name-sans-extension file)))))
    (nvp-with-process "make"
      :proc-name "install-info"
      :proc-args (target))))

;;;###autoload
(defun nvp-info-goto-source (file &optional this-window)
  "Jump to source of current info FILE."
  (interactive
   (let ((fname (concat "org/" (file-name-nondirectory Info-current-file) ".org")))
     (list (expand-file-name fname nvp/info) current-prefix-arg)))
  (if this-window
      (find-file file)
    (find-file-other-window file)))

;; -------------------------------------------------------------------
;;; Imenu support
(require 'imenu)

;;;###autoload
(defun nvp-info-imenu-create-index-function ()
  (goto-char (point-min))
  (search-forward "* Menu:")
  (let ((pat "\\*note[ \n\t]+\\([^:]+\\):\\|^\\* .*:\\|[hf]t?tps?://")
        (case-fold-search t)
        node index-alist)
    (or (eobp) (forward-char 1))
    (while (and (not (eobp)) (Info-next-reference-or-link pat 'link))
      (and (setq node (Info-extract-menu-node-name))
           (push (cons node (copy-marker (point)))
                 index-alist))
      (forward-line 1))
    index-alist))

(provide 'nvp-info)
;;; nvp-info.el ends here
