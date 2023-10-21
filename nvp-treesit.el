;;; nvp-treesit.el --- tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'treesit nil t)
(nvp:decls :p ("treesit-auto") :f (treesit-auto--build-treesit-source-alist))

;;;###autoload
(defun nvp-treesit-add-font-lock (new-fonts &optional prepend feature-list)
  ;; Add additional font-lock settings + features they define in hooks
  (setq-local treesit-font-lock-settings
              (if prepend (append new-fonts treesit-font-lock-settings)
                (append treesit-font-lock-settings new-fonts)))
  (setq-local treesit-font-lock-feature-list
              (--zip-with (seq-uniq (append it other))
                          treesit-font-lock-feature-list
                          (or feature-list
                              (list (mapcar (lambda (e) (nth 2 e)) new-fonts)
                                    nil nil nil)))))

(defun nvp-treesit-ready-p ()
  (ignore-errors (treesit-buffer-root-node)))

;; Setup treesitter sources
(defun nvp-treesit--setup-sources ()
  (when (require 'treesit-auto nil t)
    (unless treesit-language-source-alist
      (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))))

;; maybe load sources
(cl-eval-when (load)
  (nvp-treesit--setup-sources))

;;;###autoload
(defun nvp-treesit-install ()
  "Install tree-sitter grammar."
  (interactive)
  (nvp-treesit--setup-sources)
  ;; Notes:
  ;; To remap mode to tree-sitter mode
  ;; (cl-pushnew '(<major-mode> . <treesit-mode>) major-mode-remap-alist :test #'equal)
  ;; May need to restart emacs for shared grammar library to be loaded?
  (call-interactively #'treesit-install-language-grammar))

;; -------------------------------------------------------------------
;;; Errors / missing nodes

(defface nvp-treesit-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t :underline t :inherit error))
  "Face for tree sitter errors / missing nodes.")

(defvar-local nvp-treesit--errors-query nil
  "Tree sitter query to match errors and missing identifiers")

(defun nvp-treesit--query-compile ()
  "Compile error/missing quiery."
  (when (treesit-available-p)
    (and-let* ((lang (treesit-language-at (point))))
      (setq nvp-treesit--errors-query
            (treesit-query-compile lang '(((identifier) @id (:equal "" @id))
                                          (ERROR) @err))))))

(defun nvp-treesit--make-error-overlay (beg end)
  "Make error overlay from BEG to END."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'nvp-treesit-error)
    (overlay-put ov 'nvp-treesit t)))

(defun nvp-treesit--update-errors (beg end &rest _)
  "Update error/missing node overlays."
  (when nvp-treesit--errors-query
    (and-let* ((lang (treesit-language-at (point))))
      (if-let ((ranges
                (treesit-query-range
                 lang nvp-treesit--errors-query beg end ;; (point-min) (point-max)
                 )))
          (prog1 t
            (dolist (range ranges)
              (nvp-treesit--make-error-overlay (car range) (1+ (cdr range)))))
        (remove-overlays (point-min) (point-max) 'nvp-treesit t)
        nil))))

(defun nvp-treesit-toggle-errors ()
  "Toggle tree sitter error overlays in buffer."
  (interactive)
  (when nvp-treesit--errors-query
    (if (memq #'nvp-treesit--update-errors after-change-functions)
        (remove-hook 'after-change-functions #'nvp-treesit--update-errors t)
      (add-hook 'after-change-functions #'nvp-treesit--update-errors nil t))))

;; -------------------------------------------------------------------
;;; Dev Minor Mode

(defun nvp-treesit-validate (lang query)
  (interactive
   (list
    (or (treesit-language-at (point))
        (completing-read "Language: " (mapcar #'car treesit-language-source-alist)))
    (read--expression "Query: ")))
  (treesit-query-validate lang query))

;;;###autoload
(defun nvp-treesit-explorer-jump (&optional reset)
  "Pop b/w source and explorer buffers."
  (interactive "P")
  (let ((buf
         (cond
          ((eq major-mode 'treesit--explorer-tree-mode)
           (when (buffer-live-p treesit--explorer-source-buffer)
             treesit--explorer-source-buffer))
          (t
           (when (and reset (buffer-live-p treesit--explorer-buffer))
             (kill-buffer treesit--explorer-buffer))
           (unless (and treesit-explore-mode
                        (buffer-live-p treesit--explorer-buffer))
             ;; TODO: remove after patch
             (setq-local treesit--explorer-last-node nil)
             (nvp:with-letf #'completing-read
                 (lambda (&rest _)
                   ;;  (car (mapcar #'treesit-parser-language
                   ;;               (treesit-parser-list nil nil t))))
                   (symbol-name (treesit-language-at (point))))
               (treesit-explore-mode 1)))
           treesit--explorer-buffer))))
    (pop-to-buffer buf)))

(defvar-keymap nvp-treesit-minor-mode-map
  :doc "T/S keymap"
  "C-c C-v"  #'nvp-treesit-validate
  "C-c C-s"  #'nvp-treesit-toggle-errors
  "<f2> z j" #'nvp-treesit-explorer-jump
  "C-M-?"    #'treesit-inspect-node-at-point)

(easy-menu-define nvp-treesit-minor-mode-menu nvp-treesit-minor-mode-map
  "T/S menu."
  '("T/S"
    ["Inspect" treesit-inspect-node-at-point t]
    ["Explorer" nvp-treesit-explorer-jump t]
    ["Validate" nvp-treesit-validate t]
    ["Toggle errors" nvp-treesit-toggle-errors t]))

;;;###autoload
(define-minor-mode nvp-treesit-minor-mode
  "Treesit development minor mode."
  :lighter " T/S"
  :keymap nvp-treesit-minor-mode-map
  ;; XXX: powerline doesn't display `mode-line-misc-info' where
  ;; `treesit-inspect-mode' wants to put the node info
  ;; (powerline-revert)
  (if nvp-treesit-minor-mode
      (when (treesit-language-at (point))
        (powerline-revert)
        (treesit-inspect-mode)
        ;; (add-hook 'window-buffer-change-functions
        ;;           #'nvp-treesit--change-window-hook nil t)
        ;; (and (nvp-treesit--query-compile)
        ;;      (add-hook 'after-change-functions #'nvp-treesit--update-errors nil t))
        )
    (remove-hook 'after-change-functions #'nvp-treesit--update-errors t)
    ;; (remove-hook 'window-buffer-change-functions #'nvp-treesit--change-window-hook t)
    ;; (nvp-theme-switch)
    (ignore-errors 
      (treesit-inspect-mode -1)
      (treesit-explore-mode -1))))

(defun nvp-treesit-minor-mode-on ()
  (interactive)
  (when (treesit-language-at (point))
    (nvp-treesit-minor-mode 1)))

;; (defun nvp-treesit--change-window-hook (win)
;;   (with-current-buffer (window-buffer win)
;;     (when (and nvp-treesit-minor-mode
;;                treesit--explorer-buffer)
;;       (treesit--explorer-refresh)
;;       (delete-other-windows)
;;       (window--display-buffer
;;        (get-buffer treesit--explorer-buffer)
;;        (split-window-horizontally)
;;        'window))))

;;;###autoload
(define-globalized-minor-mode nvp-treesit-mode
  nvp-treesit-minor-mode nvp-treesit-minor-mode-on
  :group 'treesit)

;; -------------------------------------------------------------------
;;; Transient

(require 'transient)
(nvp:auto "ts-util" ts-util-nodes)

(transient-define-infix nvp-treesit-menu--toggle-ts-debug ()
  :class 'transient-lisp-variable
  :variable 'treesit--font-lock-verbose
  :reader (lambda (&rest _) (not treesit--font-lock-verbose)))

(transient-define-infix nvp-treesit-menu--toggle-indent-verbose ()
  :class 'transient-lisp-variable
  :variable 'treesit--indent-verbose
  :reader (lambda (&rest _) (not treesit--indent-verbose)))

;;;###autoload(autoload 'nvp-treesit-menu "nvp-treesit")
(transient-define-prefix nvp-treesit-menu ()
  "Treesit"
  [ :if nvp-treesit-ready-p
    ["Current"
     ("?" "Inspect node" treesit-inspect-node-at-point)
     ("e" "Toggle errors" nvp-treesit-toggle-errors)]
    ["Explorer"
     ("j" "Jump" nvp-treesit-explorer-jump)]
    ["Query"
     ("v" "Validate" nvp-treesit-validate)]]
  [["Parsers"
    ("pn" "List nodes" ts-util-nodes)
    ("pi" "Install parser" nvp-treesit-install)]
   ["Dev Mode"
    ("ml" "local" nvp-treesit-minor-mode)
    ("mg" "global" nvp-treesit-mode)]
   ["Debug"
    (":i" "Toggle indent verbose" nvp-treesit-menu--toggle-indent-verbose)
    (":f" "Toggle font debug" nvp-treesit-menu--toggle-ts-debug)]])

(provide 'nvp-treesit)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-treesit.el ends here
