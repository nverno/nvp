;;; nvp-info.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; info helpers
;; - gather my nodes
;; - update when file changes
;; - convert org to info and install
;; - jump to source
;; - imenu support
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'info)
(require 'filenotify)
(require 'info-look)
(nvp:decls :f (nvp-read-mode))
(nvp:auto "nvp-read" 'nvp-read--info-files)

(defun nvp-info-read-node (manual &optional prompt default)
  (let* ((node (format "(%s)" manual))
         (prompt (or prompt (format "%s node: " manual))))
    (nvp:with-fallback :fallback (lambda (_) node)
      (nvp-completing-read prompt (Info-build-node-completions) nil t nil
        'Info-minibuf-history default))))

(defsubst nvp-info-read-manual (&optional arg)
  (progn
    (info-initialize)
    (nvp-completing-read "Manual name: " (info--manual-names arg) nil t)))

;;;###autoload
(defun nvp-info-lookup-node (&optional arg)
  "Lookup node in info associated with current mode,
or prompt for manual with ARG."
  (interactive "P")
  (--if-let (or (and arg (nvp-info-read-manual))
                (-some-->
                    (or (info-lookup-select-mode)
                        (nvp-read-mode))
                  (info-lookup->doc-spec 'symbol it)
                  (car (nth 0 it)))
                (setq arg (nvp-info-read-manual)))
    (with-current-buffer (get-buffer-create (generate-new-buffer "*info*"))
      (Info-mode)
      (if arg (Info-goto-node (format "(%s)" it))
        (Info-goto-node it))
      (Info-goto-node (nvp-info-read-node it))
      (pop-to-buffer (current-buffer)))
    (user-error "No info found.")))

;;; Imenu support

(defvar nvp-info--imenu-categories-expression '((nil "^\\([[:alnum:]]+\\)" 1)))

(defun nvp-info-imenu ()
  "Look for categories in Top node only."
  (interactive)
  (if (not (equal "Top" Info-current-node))
      (call-interactively #'imenu)
    (let ((imenu-generic-expression nvp-info--imenu-categories-expression)
          (imenu-create-index-function #'imenu-default-create-index-function)
          (saved-index imenu--index-alist)
          (imenu--index-alist nil)
          (imenu-auto-rescan t))
      (imenu--make-index-alist)
      (unwind-protect
          (call-interactively #'imenu)
        (setq imenu--index-alist saved-index)))))

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


;;; Mine

(defvar nvp-info-nodes-need-refresh () "Update list when \"dir\" changes.")
(nvp:define-cache nvp-info-nodes ()
  ;; "List of my info manuals."
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

(cl-eval-when (load)
  (file-notify-add-watch
   (expand-file-name "dir" nvp/info) (list 'change)
   #'(lambda (&rest _args) (setq nvp-info-nodes-need-refresh t))))

;;;###autoload
(defun nvp-info-open-mine (topic &optional bname)
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
(defun nvp-info-install-mine (file)
  "Install org texinfo FILE into info directory."
  (interactive (list (nvp-read--info-files)))
  (let ((default-directory (expand-file-name "org" nvp/info))
        (target (concat "install-" (nvp:path 'fse file))))
    (nvp:with-process "make"
      :proc-name "install-info"
      :proc-args (target))))

(defun nvp-info-goto-source (action file)
  "Jump to source of current info FILE."
  (interactive
   (let ((fname (concat "org/" (file-name-nondirectory Info-current-file) ".org")))
     (list current-prefix-arg (expand-file-name fname nvp/info))))
  (nvp-with-display-actions action
    (pop-to-buffer (find-file-noselect file))))

;; -------------------------------------------------------------------
;;; Transient

(require 'transient)

;;;###autoload(autoload 'nvp-info-menu "nvp-info" nil t)
(transient-define-prefix nvp-info-menu () "Info"
  [["Info"
    ("s" "Symbol" info-lookup-symbol)
    ("k" "Keybinding" Info-goto-emacs-key-command-node)
    ("F" "File" info-lookup-file)]
   ["Search"
    ("a" "Apropos" info-apropos)
    ("l" "Display Manual" info-display-manual)
    ("n" "Lookup node" nvp-info-lookup-node)
    ("f" "Finder" info-finder)
    ("i" "Info" info)
    ("h" "History" Info-history)
    ("R" "Reset" info-lookup-reset)]
   ["Mine"
    ("o" "Mine" nvp-info-open-mine)
    ("j" "Jump" nvp-info-goto-source)
    ("I" "Install" nvp-info-install-mine)]
   ["Emacs"
    ("e" "Emacs manual" info-emacs-manual)
    ("E" "Emacs Command Node" Info-goto-emacs-command-node)]])

(provide 'nvp-info)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-info.el ends here
