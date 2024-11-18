;;; nvp-treesit.el --- tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'treesit nil t)
(nvp:decls :p (ts) :f (ts-error-toggle ts-debug-show-missing-indent))


(defvar nvp-treesit--hash-bang-re
  (concat "\\`\\(#!\\).*/\\([^ \t\n]+\\)"
          "\\(?:[ \t]+\\(?:-\\w+[ \t]+\\)*\\([^ \t\n]+\\)\\)?"))

;;;###autoload
(defun nvp-treesit-fontify-hash-bang (node override start end &rest _)
  (save-excursion
    (goto-char (treesit-node-start node))
    (when (looking-at nvp-treesit--hash-bang-re)
      (let ((idx (if (and (match-beginning 3)
                          (string= "env" (match-string-no-properties 2)))
                     3 2)))
        (treesit-fontify-with-override
         (match-beginning 1) (match-end 1)
         'font-lock-comment-delimiter-face override start end)
        (treesit-fontify-with-override
         (match-end 1) (match-beginning idx)
         'font-lock-comment-face override start end)
        (treesit-fontify-with-override
         (match-beginning idx) (match-end idx)
         'nvp-interpreter-face override start end)))))

(eval-when-compile
  (defsubst nvp-treesit:add-sources ()
    (when (fboundp 'ts-util-add-treesit-sources)
      (ignore-errors (ts-util-add-treesit-sources))))

  (defsubst nvp-treesit:read (&optional multiple)
    (apply (if multiple #'completing-read-multiple #'completing-read)
           (format "Language%s: " (if multiple "(s)" ""))
           treesit-language-source-alist
           nil t nil)))

;;;###autoload
(defun nvp-treesit-install (&optional parsers)
  "Install/update tree-sitter grammars for PARSERS.
With prefix, add neovim sources first."
  (interactive (progn (and current-prefix-arg (nvp-treesit:add-sources))
                      (list (mapcar #'intern (nvp-treesit:read 'multiple)))))
  (if parsers
      (dolist (lang parsers)
        (treesit-install-language-grammar lang))
    (call-interactively #'treesit-install-language-grammar)))


;; -------------------------------------------------------------------
;;; Dev Minor Mode

(defun nvp-treesit-validate (lang query)
  (interactive (list (or (treesit-language-at (point))
                         (nvp-treesit:read))
                     (read--expression "Query: ")))
  (treesit-query-validate lang query))

;;;###autoload
(defun nvp-treesit-explorer-jump (&optional reset)
  "Pop b/w source and explorer buffers."
  (interactive "P")
  (let ((buf (cond ((eq major-mode 'treesit--explorer-tree-mode)
                    (when (buffer-live-p treesit--explorer-source-buffer)
                      treesit--explorer-source-buffer))
                   (t (when (and reset (buffer-live-p treesit--explorer-buffer))
                        (kill-buffer treesit--explorer-buffer))
                      (unless (and treesit-explore-mode
                                   (buffer-live-p treesit--explorer-buffer))
                        (nvp:with-letf #'completing-read
                            (lambda (&rest _)
                              (symbol-name (treesit-language-at (point))))
                          (save-window-excursion
                            (treesit-explore-mode 1))))
                      treesit--explorer-buffer))))
    (pop-to-buffer buf)))


;; -------------------------------------------------------------------
;;; Minor mode

(defvar-keymap nvp-treesit-minor-mode-map
  :doc "T/S keymap"
  ;; "C-c C-s"  #'ts-error-toggle
  "C-c C-v"  #'nvp-treesit-validate
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
  (if nvp-treesit-minor-mode
      (when (treesit-language-at (point))
        (treesit-inspect-mode))
    (ignore-errors
      (treesit-inspect-mode -1)
      (treesit-explore-mode -1))))

(defun nvp-treesit-minor-mode-on ()
  (interactive)
  (when (and (not (or noninteractive
                      (eq (aref (buffer-name) 0) ?\s)))
             (treesit-language-at (point)))
    (nvp-treesit-minor-mode 1)))

;;;###autoload
(define-globalized-minor-mode nvp-treesit-mode
  nvp-treesit-minor-mode nvp-treesit-minor-mode-on
  :group 'treesit)


;; -------------------------------------------------------------------
;;; Transient

(defsubst nvp-treesit-ready-p ()
  (ignore-errors (treesit-buffer-root-node)))

(nvp:transient-define-vars nvp--menu
  (treesit--font-lock-verbose . t)
  (treesit--indent-verbose . t))

(autoload 'ts-sources-browse-repo "ts-sources")

;;;###autoload(autoload 'nvp-treesit-menu "nvp-treesit" nil t)
(transient-define-prefix nvp-treesit-menu ()
  "Treesit"
  :refresh-suffixes t
  [ :if nvp-treesit-ready-p
    ["Queries"
     ("i" "Query" ts-query)
     ("q" "Highlight query" ts-query-highlight-query :transient t)
     ("Q" "Remove highlights" ts-query-remove-highlights :transient t
      :if-non-nil ts-query--langs)
     ("v" "Validate" nvp-treesit-validate)]
    ["Inspect"
     ("j" "Jump to Explorer" nvp-treesit-explorer-jump)
     ("n" "Inspect node" treesit-inspect-node-at-point :transient t)
     ("e" "Toggle errors" ts-error-toggle)]]
  [["Parsers"
    ("l" "List nodes" ts-parser-list-nodes)
    ("L" "List sources" ts-sources)
    ("r" "Toggle ranges" ts-parser-toggle-ranges :transient t
     :if nvp-treesit-ready-p)
    ("a" "Add sources" ts-util-add-treesit-sources)
    ("I" "Install/update parsers" nvp-treesit-install)
    ("b" "Browse repo" ts-sources-browse-repo)]
   ["Dev"
    ("m" "Global mode" nvp-treesit-mode)
    ("M" "Local mode" nvp-treesit-minor-mode)
    ("g" "Go to query directories" ts-util-jump-to-queries)
    ("x" "Extract corpus tests" ts-util-extract-corpus-tests)]
   ["Debug"
    ("si" "Toggle missing indent hl" ts-debug-show-missing-indent
     :if nvp-treesit-ready-p)
    (":i" "Indent verbose" nvp--menu-treesit--indent-verbose)
    (":f" "Font debug" nvp--menu-treesit--font-lock-verbose)]]
  (interactive)
  (require 'ts-util nil t)
  (transient-setup 'nvp-treesit-menu))

(provide 'nvp-treesit)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-treesit.el ends here
