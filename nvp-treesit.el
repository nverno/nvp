;;; nvp-treesit.el --- tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'treesit nil t)
(nvp:decls :p (ts) :f (ts-error-toggle))
(declare-function treesit-auto--build-treesit-source-alist "treesit-auto")

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
  (call-interactively #'treesit-install-language-grammar))

(defun nvp-treesit-update (&optional parser)
  "Update PARSER grammar or all grammars."
  (interactive
   (list (if current-prefix-arg
             (intern
              (completing-read "Language: "
                               (mapcar #'car treesit-language-source-alist))))))
  (let ((langs (if parser (list parser)
                 (mapcar #'car treesit-language-source-alist))))
    (dolist (lang langs)
      (treesit-install-language-grammar lang))))

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
    ("i" "Install parser" nvp-treesit-install)
    ("U" "Update parser(s)" nvp-treesit-update)]
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
