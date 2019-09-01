;;; nvp-info.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; info helpers
;; - gather my nodes
;; - update when file changes
;; - convert org to info and install
;; - jump to source
;; - imenu support

;; TODO: node lookup should allow fuzzy matching

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'info)
(require 'filenotify)
(require 'info-look)
(nvp-decls)
(nvp-auto "nvp-read" 'nvp-read--info-files)

;;;###autoload
(defun nvp-info-lookup-node ()
  "Lookup node in info associated with current mode."
  (interactive)
  (-some-->
   (info-lookup-select-mode)
   (info-lookup->doc-spec 'symbol it)
   (car (nth 0 it))
   (with-current-buffer (generate-new-buffer "*info*")
     (Info-mode)
     (Info-goto-node it)
     (call-interactively #'Info-goto-node)
     (pop-to-buffer (current-buffer)))))

;;; Imenu support

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

(defvar nvp-info-nodes-need-refresh () "Update list when 'dir' changes.")
(cl-eval-when (load compile eval)
  (nvp-define-cache nvp-info-nodes ()
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
        nodes))))

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
        (target (concat "install-" (nvp-path 'fse file))))
    (nvp-with-process "make"
      :proc-name "install-info"
      :proc-args (target))))

(defun nvp-info-goto-source (file action)
  "Jump to source of current info FILE."
  (interactive
   (let ((fname (concat "org/" (file-name-nondirectory Info-current-file) ".org")))
     (list (expand-file-name fname nvp/info) current-prefix-arg)))
  (nvp-display-location file :file action))

(provide 'nvp-info)
;;; nvp-info.el ends here
