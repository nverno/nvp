;;; nvp-treesit.el --- tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'treesit nil t)
(nvp:decls :p (ts) :f (ts-error-toggle))

;;;###autoload
(defun nvp-treesit-fontify-hash-bang (node override start end &rest _)
  (save-excursion
    (goto-char (treesit-node-start node))
    (save-match-data
      (when (looking-at
             "\\`\\(#!\\).*/\\([^ \t\n]+\\)\\(?:[ \t]+\\(?:-\\w+[ \t]+\\)*\\([^ \t\n]+\\)\\)?")
        (let ((m (if (and (string= "env" (buffer-substring-no-properties
                                          (match-beginning 2) (match-end 2)))
                          (match-beginning 3))
                     3
                   2)))
          (treesit-fontify-with-override
           (match-beginning 1) (match-end 1)
           'font-lock-comment-delimiter-face override start end)
          (treesit-fontify-with-override
           (match-end 1) (match-beginning m)
           'font-lock-comment-face override start end)
          (treesit-fontify-with-override
           (match-beginning m) (match-end m)
           'nvp-interpreter-face override start end))))))


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
                                    nil nil nil))))
  (treesit-font-lock-recompute-features))

;;;###autoload
(defun nvp-treesit-install (&optional add-sources)
  "Install tree-sitter grammar."
  (interactive "P")
  (when (and add-sources (fboundp 'ts-util-add-treesit-sources))
    (ignore-errors (ts-util-add-treesit-sources)))
  (call-interactively #'treesit-install-language-grammar))

(defun nvp-treesit-update (parsers)
  "Update grammars for PARSERS."
  (interactive
   (list (mapcar #'intern (completing-read-multiple
                           "Language(s): " treesit-language-source-alist))))
  (dolist (lang parsers)
    (treesit-install-language-grammar lang)))

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
             (nvp:with-letf #'completing-read
                 (lambda (&rest _)
                   (symbol-name (treesit-language-at (point))))
               (treesit-explore-mode 1)))
           treesit--explorer-buffer))))
    (pop-to-buffer buf)))

;; -------------------------------------------------------------------
;;; Minor mode

(defvar-keymap nvp-treesit-minor-mode-map
  :doc "T/S keymap"
  "C-c C-v"  #'nvp-treesit-validate
  "C-c C-s"  #'ts-error-toggle
  "<f2> z j" #'nvp-treesit-explorer-jump
  "C-M-?"    #'treesit-inspect-node-at-point)

(easy-menu-define nvp-treesit-minor-mode-menu nvp-treesit-minor-mode-map
  "T/S menu."
  '("T/S"
    ["Inspect" treesit-inspect-node-at-point t]
    ["Explorer" nvp-treesit-explorer-jump t]
    ["Validate" nvp-treesit-validate t]
    ["Toggle errors" ts-error-toggle t]))

;;;###autoload
(define-minor-mode nvp-treesit-minor-mode
  "Treesit development minor mode."
  :lighter " 🌲"
  :keymap nvp-treesit-minor-mode-map
  ;; XXX: powerline doesn't display `mode-line-misc-info' where
  ;; `treesit-inspect-mode' wants to put the node info
  ;; (powerline-revert)
  (if nvp-treesit-minor-mode
      (when (treesit-language-at (point))
        ;; (powerline-revert)
        ;; (add-hook 'window-buffer-change-functions
        ;;           #'nvp-treesit--change-window-hook nil t)
        (treesit-inspect-mode))
    ;; (remove-hook 'window-buffer-change-functions #'nvp-treesit--change-window-hook t)
    ;; (nvp-theme-switch)
    (ignore-errors
      (treesit-inspect-mode -1)
      (treesit-explore-mode -1))))

(defun nvp-treesit-minor-mode-on ()
  (interactive)
  (when (and (not (or noninteractive (eq (aref (buffer-name) 0) ?\s)))
             (treesit-language-at (point)))
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

(defun nvp-treesit-ready-p ()
  (ignore-errors (treesit-buffer-root-node)))

(nvp:transient-toggle nvp-treesit-menu
  treesit--font-lock-verbose
  treesit--indent-verbose)

;;;###autoload(autoload 'nvp-treesit-menu "nvp-treesit" nil t)
(transient-define-prefix nvp-treesit-menu ()
  "Treesit"
  :refresh-suffixes t
  [ :if nvp-treesit-ready-p
    ["Queries"
     ("q" "Highlight query" ts-query-highlight-query :transient t)
     ("Q" "Remove highlights" ts-query-remove-highlights :transient t
      :if-non-nil ts-query--langs)
     ("v" "Validate" nvp-treesit-validate)]
    ["Inspect"
     ("j" "Jump to Explorer" nvp-treesit-explorer-jump)
     ("?" "Inspect node" treesit-inspect-node-at-point :transient t)
     ("e" "Toggle errors" ts-error-toggle)]]
  [["Parsers"
    ("l" "List nodes" ts-parser-list-nodes)
    ("L" "List sources" ts-util-list-sources)
    ("r" "Toggle ranges" ts-parser-toggle-ranges :transient t
     :if nvp-treesit-ready-p)
    ("a" "Add sources" ts-util-add-treesit-sources)
    ("i" "Install parser" nvp-treesit-install)
    ("U" "Update parser(s)" nvp-treesit-update)
    ("b" "Browse repo" ts-util-browse-repo)]
   ["Dev"
    ("m" "Global mode" nvp-treesit-mode)
    ("M" "Local mode" nvp-treesit-minor-mode)
    ("g" "Go to query directories" ts-util-jump-to-queries)
    ("x" "Extract corpus tests" ts-util-extract-corpus-tests)]
   ["Debug"
    (":i" "Indent verbose"
     nvp-treesit-menu--toggle-treesit--indent-verbose)
    (":f" "Font debug"
     nvp-treesit-menu--toggle-treesit--font-lock-verbose)]]
  (interactive)
  (require 'ts-util nil t)
  (transient-setup 'nvp-treesit-menu))

(provide 'nvp-treesit)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-treesit.el ends here
