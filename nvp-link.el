;;; nvp-link.el --- links -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'avy)
(nvp:decls :v (org-link-any-re) :f (ace-link ace-link-org))

(defvar nvp-link-default-imenu t
  "Jump to imenu in visibile window when no other handlers.")

;; map major-modes to link actions
(defvar nvp-ace-link-major-mode-actions
  `((Info-mode                   . ace-link-info)
    (Man-mode                    . ace-link-man)
    (woman-mode                  . ace-link-woman)
    (eww-mode                    . ace-link-eww)
    (w3m-mode                    . ace-link-w3m)
    (mu4e-view-mode              . ace-link-mu4e)
    (notmuch-show-mode           . ace-link-notmuch)
    (Custom-mode                 . ace-link-custom)
    (sldb-mode                   . ace-link-sldb)
    (slime-xref-mode             . ace-link-slime-xref)
    (slime-inspector-mode        . ace-link-slime-inspector)
    (indium-inspector-mode       . ace-link-indium-inspector)
    (indium-debugger-frames-mode . ace-link-indium-debugger-frames)
    (magit-commit-mode           . ace-link-commit)
    (cider-inspector-mode        . ace-link-cider-inspector)
    ;; (org-agenda-mode             . ace-link-org-agenda)
    ,@(mapcar (lambda (m) (cons m 'ace-link-help))
              '(help-mode
                package-menu-mode
                geiser-doc-mode
                elbank-report-mode
                elbank-overview-mode
                slime-trace-dialog-mode
                helpful-mode
                ;; added
                leetcode--problem-detail-mode))
    ,@(mapcar (lambda (m) (cons m 'ace-link-compilation))
              '(compilation-mode
                grep-mode
                ;; added
                emacs-lisp-compilation-mode
                ag-mode
                rg-mode))
    ,@(mapcar (lambda (m) (cons m 'ace-link-gnus))
              '(gnus-article-mode
                gnus-summary-mode))
    ,@(mapcar (lambda (m) (cons m 'ace-link-org))
              '(org-mode
                org-agenda-mode         ; `ace-link-org-agenda'
                erc-mode elfeed-show-mode
                term-mode vterm-mode
                eshell-mode
                telega-chat-mode))
    ;; added
    (shortdoc-mode               . ace-link-help)
    (shell-mode                  . nvp-link-shell))
  "Mapping of `major-mode' to ace-link actions.")

(defvar nvp-ace-link-minor-mode-actions
  '((compilation-shell-minor-mode . ace-link-compilation)
    (nvp-mark-minor-mode          . nvp-link-mark)
    (lsp-mode                     . nvp-link-lsp))
  "Mapping of minor modes to ace-link actions.")

(defun nvp-ace-link-action (&optional buffer)
  "Return action associated with current buffer, if any."
  (or (cdr (assoc major-mode nvp-ace-link-major-mode-actions))
      (cl-some (lambda (action)
                 (and (boundp (car action))
                      (buffer-local-value
                       (car action) (or buffer (current-buffer)))
                      (cdr action)))
               nvp-ace-link-minor-mode-actions)))

;; sort windows left-to-right: see `winner-sorted-window-list'
(defun nvp-sort-window-list (windows)
  (sort windows
        (lambda (x y)
          (cl-loop for a in (window-edges x)
                   for b in (window-edges y)
                   while (= a b)
                   finally return (< a b)))))

;; collect overlays in window that have PROP
(defun nvp-link--overlay-collect (prop)
  (let (res)
    (dolist (overlay (overlays-in (window-start) (window-end)))
      (if (overlay-get overlay prop)
          (push (overlay-start overlay) res)))
    (nreverse res)))

(defun nvp-link-maybe-imenu ()
  (and nvp-link-default-imenu
       (or (bound-and-true-p imenu--index-alist)
           (and (fboundp 'imenu--make-index-alist)
                (setq imenu--index-alist
                      (save-excursion
                        (funcall imenu-create-index-function)))))))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-ace-link ()
  "Call the ace link function for the current `major-mode'"
  (interactive)
  (if-let ((action (nvp-ace-link-action (current-buffer))))
      (funcall action)
    (unless (and ace-link-fallback-function
                 (funcall ace-link-fallback-function))
      (if (nvp-link-maybe-imenu)
          (nvp-link-imenu)
        (error "%S isn't supported" major-mode)))))

;;;###autoload
(defun nvp-goto-link (&optional arg)
  "Jump to link in current window if mode is recognized.
Otherwise, try jumping to links in visible buffers instead.
With \\[universal-argument] call in next visible window."
  (interactive "P")
  (let ((win (if arg (next-window (selected-window) nil nil)
               (selected-window))))
    (with-selected-window win
      (let ((cur-buf (current-buffer)))
        (let ((ace-link-fallback-function
               (lambda ()
                 (--when-let
                     (nvp:visible-windows
                       :test-fn
                       (lambda (buf)
                         (and (not (eq buf cur-buf))
                              (assoc (buffer-local-value 'major-mode buf)
                                     nvp-ace-link-major-mode-actions))))
                   (with-selected-window (car (nvp-sort-window-list it))
                     (prog1 t
                       (call-interactively #'nvp-ace-link)))))))
          (call-interactively #'nvp-ace-link))))))

;; -------------------------------------------------------------------
;;; Link Extensions

(eval-when-compile
  (defmacro nvp:define-link (name collector &rest action)
    "Wrap `avy-with', bind \\='it to return of COLLECTOR in ACTION."
    (declare (indent 2) (debug t))
    `(defun ,name ()
       (interactive)
       (let ((it (avy-with ,name
                   (avy-process
                    ,collector
                    (avy--style-fn avy-style)))))
         ,@action))))

;;; Mark
(nvp:decl nvp-mark-goto-marker-at-point)
(defvar nvp-mark--regex)

(defun nvp-link--mark-collect ()
  (let (res)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward nvp-mark--regex nil 'move)
        (push (cons (buffer-substring-no-properties
                     (match-beginning 0) (match-end 0))
                    (match-beginning 0))
              res)))
    (nreverse res)))

(nvp:define-link nvp-link-mark
    (--map (cdr it) (nvp-link--mark-collect))
  (when it
    (funcall #'nvp-mark-goto-marker-at-point)))

;;; TODO: dired
(defun nvp-link--dired-collect ())

;;; TODO: Markdown
(nvp:decl markdown-next-link markdown-link-at-pos)
(defun nvp-link--markdown-collect ()
  (let (res)
    (nvp:with-restriction 'visible
      (goto-char (point-min))
      (while (and (markdown-next-link) (point))
        (push (cons (markdown-link-at-pos (point)) (point))
              res)))
    res))

;;; Shell
(nvp:decl comint-next-prompt)

(defun nvp-link--shell-collect ()
  (let (res)
    (nvp:with-restriction 'visible
      (let ((end (1- (line-beginning-position (point-max)))))
        (goto-char (point-min))
        (while (progn (comint-next-prompt 1) (< (point) end))
          (push (cons (string-chop-newline (nvp:tap 'tap 'line))
                      (pos-bol))
                res))))
    (nreverse res)))

(nvp:define-link nvp-link-shell
    (--map (cdr it) (nvp-link--shell-collect))
  (avy-action-yank-line it))

;;; Lsp
(nvp:define-link nvp-link-lsp
    (nvp-link--overlay-collect 'lsp-link)
  (goto-char (1+ it))
  (push-button it))

;;; Imenu
(nvp:auto "nvp-imenu" 'nvp-imenu-cleaned-alist)
(defun nvp-link--imenu-collect ()
  (when (bound-and-true-p imenu--index-alist)
    (let ((beg (window-start))
          (end (window-end)))
      (--keep (let ((pos (marker-position (cdr it))))
                (and (>= pos beg) (<= pos end) pos))
              (nvp-imenu-cleaned-alist)))))

(nvp:define-link nvp-link-imenu
    (nvp-link--imenu-collect)
  (goto-char it))

;; use actual links in agenda buffer - the default is just the same
;; as `avy-goto-line'
(with-eval-after-load 'ace-link
  (advice-add 'ace-link-org-agenda :override #'ace-link-org))

(provide 'nvp-link)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-link.el ends here
