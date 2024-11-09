;;; nvp-link.el --- links -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'avy)
(nvp:decls :p (ace) :v (org-link-any-re goto-address-mode) :f (ace-link-org))


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
                leetcode-detail-mode
                apropos-mode
                shellcheck-mode
                process-menu-mode
                shortdoc-mode
                devdocs-mode
                debugger-mode
                ess-r-help-mode))
    ,@(mapcar (lambda (m) (cons m 'ace-link-compilation))
              '(compilation-mode
                grep-mode
                ;; added
                emacs-lisp-compilation-mode
                ag-mode
                rg-mode
                rustic-compilation-mode))
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
    (shell-mode                  . nvp-link-shell)
    (lsp-help-mode               . nvp-link-lsp-help)
    (markdown-mode               . nvp-link-markdown)
    (dired-mode                  . nvp-link-dired)
    (completion-list-mode        . nvp-link-completion-list)
    (recentf-dialog-mode         . nvp-link-widget)
    (noman-mode                  . nvp-link-noman))
  "Mapping of `major-mode' to ace-link actions.")

(defvar nvp-ace-link-minor-mode-actions
  '((compilation-shell-minor-mode . nvp-link-compilation)
    (nvp-mark-minor-mode          . nvp-link-mark)
    (lsp-mode                     . nvp-link-lsp)
    (goto-address-mode            . ace-link-addr))
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

(defun nvp-sort-window-list (windows)
  "Sort windows left-to-right, see `winner-sorted-window-list'."
  (sort windows
        (lambda (x y)
          (cl-loop for a in (window-edges x)
                   for b in (window-edges y)
                   while (= a b)
                   finally return (< a b)))))

(defun nvp-link--overlay-collect (prop &optional value)
  "Collect overlay positions in the visbile window that have PROP with VALUE."
  (let (res)
    (dolist (overlay (overlays-in (window-start) (window-end nil t)))
      (--when-let (overlay-get overlay prop)
        (when (or (null value)
                  (eq value it))
          (push (overlay-start overlay) res))))
    (nreverse res)))

(nvp:decl widget-forward)
(defun nvp-link--widget-collect ()
  "Collect widget positions in the visible window."
  (save-excursion
    (nvp:with-restriction 'visible
      (goto-char (point-min))
      (let ((pos (point)) res)
        (while (setq pos (progn (widget-forward 1)
                                (and (> (point) pos) (point))))
          (push pos res))
        (nreverse res)))))

(defun nvp-link--maybe-imenu ()
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
  (if-let* ((action (nvp-ace-link-action (current-buffer))))
      (funcall action)
    (unless (and ace-link-fallback-function
                 (funcall ace-link-fallback-function))
      (if (nvp-link--maybe-imenu)
          (nvp-link-imenu)
        (error "%S isn't supported" major-mode)))))

;;;###autoload
(defun nvp-goto-link (&optional win-or-buf all-frames)
  "Jump to link in current buffer or WIN-OR-BUF if mode is recognized.

Otherwise, try jumping to links in other buffers instead.
Prefixes:

  0	Call in current other-window scrolling buffer.
  \\[universal-argument]   Try other buffer immediately.
  \\[universal-argument]\\[universal-argument]   Try visible windows on\
 ALL-FRAMES."
  (interactive (let* ((raw (prefix-numeric-value current-prefix-arg))
                      (arg (abs raw)))
                 (list (cond ((memq arg '(- 0))
                              (and-let* ((win
                                          (nvp-other-window-scroll-default)))
                                (display-buffer (window-buffer win))
                                win))
                             ((= 4 arg) (next-window
                                         (selected-window) nil nil))
                             (t (selected-window)))
                       (and (> arg 4)
                            'visible))))
  (let ((win (cond ((windowp win-or-buf) win-or-buf)
                   ((bufferp win-or-buf)
                    (get-buffer-window win-or-buf all-frames))
                   (t (selected-window)))))
    (with-selected-window win
      (let* ((cur-buf (current-buffer))
             (ace-link-fallback-function
              (lambda ()
                (--when-let
                    (nvp:visible-windows
                      :test-fn (lambda (buf)
                                 (and (not (eq buf cur-buf))
                                      (assoc
                                       (buffer-local-value 'major-mode buf)
                                       nvp-ace-link-major-mode-actions)))
                      :all-frames nil)
                  (with-selected-window (car (nvp-sort-window-list it))
                    (prog1 t
                      (call-interactively #'nvp-ace-link)))))))
        (call-interactively #'nvp-ace-link)))))

;; -------------------------------------------------------------------
;;; Link Extensions

(nvp:auto "ace-link" ace-link--addr-collect)
(eval-when-compile
  (defmacro nvp:link--action (pos &rest body)
    "If POS is an address go there, otherwise handle with BODY."
    (declare (indent 1) (debug t))
    `(if (and (bound-and-true-p goto-address-mode)
              (number-or-marker-p ,pos)
              (cl-some (lambda (ov) (overlay-get ov 'goto-address))
                       (overlays-at ,pos)))
         (progn (goto-char (1+ ,pos))
                (goto-address-at-point))
       ,@body))

  (cl-defmacro nvp:define-link (name &rest action
                                     &key collector style address
                                     &allow-other-keys)
    "Wrap `avy-with', bind \\='it to return of COLLECTOR in ACTION."
    (declare (indent 1) (debug t))
    (nvp:skip-keywords action)
    `(defun ,name ()
       (interactive)
       (let ((it (avy-with ,name
                   (avy-process
                    ,(if (null address)
                         collector
                       (macroexp-let2 nil collector collector
                         `(if (bound-and-true-p goto-address-mode)
                              (sort (append (ace-link--addr-collect) ,collector))
                            ,collector)))
                    (avy--style-fn ,(or style 'avy-style))))))
         ,(if address
              `(nvp:link--action it ,@action)
            `(progn ,@action))))))

;;; Compilation
(nvp:auto "ace-link" ace-link--eww-collect)
(nvp:decl compile-goto-error)

(nvp:define-link nvp-link-compilation
  :address t
  :collector (--map (cdr it) (ace-link--eww-collect 'help-echo))
  (when (number-or-marker-p it)
    (goto-char (1+ it))
    (compile-goto-error)))

;;; Mark
(nvp:decl nvp-mark-goto-marker-at-point nvp-mark--collect-marks)
(defvar nvp-mark--regex)

(nvp:define-link nvp-link-mark
  :collector (nvp-mark--collect-marks)
  (and it (funcall #'nvp-mark-goto-marker-at-point)))

;;; Dired
(nvp:decl dired-next-dirline dired-get-filename dired-find-file)
(defun nvp-link--dired-collect ()
  (unwind-protect
      (save-excursion
        (let (res)
          (when (fboundp 'smooth-scrolling-mode) (smooth-scrolling-mode -1))
          (nvp:with-restriction 'visible
            (goto-char (point-min))
            (while (condition-case nil
                       (progn
                         (dired-next-dirline 1)
                         t)
                     (error nil))
              (let ((dir (dired-get-filename 'verbatim 'no-error)))
                (unless (member dir '("." ".." nil))
                  (push (cons dir (point)) res)))))
          res))
    (when (fboundp 'smooth-scrolling-mode) (smooth-scrolling-mode 1))))

(nvp:define-link nvp-link-dired
  :collector (--map (cdr it) (nvp-link--dired-collect))
  :style 'pre
  (when (numberp it)
    (goto-char (1+ it))
    (dired-find-file)))

;;; Markdown
(nvp:decl markdown-next-link markdown-link-at-pos markdown-follow-thing-at-point)
(defun nvp-link--markdown-collect ()
  (let (res)
    (nvp:with-restriction 'visible
      (goto-char (point-min))
      (while (and (markdown-next-link) (point))
        (push (cons (markdown-link-at-pos (point)) (point))
              res)))
    res))

(nvp:define-link nvp-link-markdown
  :collector (--map (cdr it) (nvp-link--markdown-collect))
  (when (numberp it)
    (goto-char (1+ it))
    (markdown-follow-thing-at-point nil)))

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
  :address t
  :collector (--map (cdr it) (nvp-link--shell-collect))
  (when (numberp it) (avy-action-yank-line it)))

;;; Lsp
(nvp:auto "ace-link" ace-link--help-collect)
(nvp:define-link nvp-link-lsp
  :collector (nvp-link--overlay-collect 'lsp-link)
  (when (numberp it)
    (goto-char (1+ it))
    (push-button it)))

(nvp:decl lsp--help-open-link)
(nvp:define-link nvp-link-lsp-help
  :collector (--map (cdr it) (ace-link--help-collect))
  (when (numberp it)
    (goto-char (1+ it))
    (funcall-interactively #'lsp--help-open-link)))

;;; Imenu
(nvp:auto "nvp-imenu" 'nvp-imenu-cleaned-alist)
(defun nvp-link--imenu-collect ()
  (when (bound-and-true-p imenu--index-alist)
    (let ((beg (window-start))
          (end (window-end nil t)))
      (--keep (let ((pos (marker-position (cdr it))))
                (and (>= pos beg) (<= pos end) pos))
              (nvp-imenu-cleaned-alist)))))

(nvp:define-link nvp-link-imenu
  :collector (nvp-link--imenu-collect)
  (and (numberp it) (goto-char it)))

;;; Completion List
(defun nvp-link--completion-collect ()
  (save-excursion
    (let ((end (window-end nil t))
          (pos (window-start))
          cands)
      (while (and pos
                  (setq pos (next-single-property-change pos 'mouse-face))
                  (< pos end))
        (let ((beg pos))
          (setq pos (next-single-property-change pos 'mouse-face))
          (and pos (push (cons (posn-at-point beg) beg) cands))))
      cands)))

(nvp:define-link nvp-link-completion-list
  :collector (--map (cdr it) (nvp-link--completion-collect))
  (and it (choose-completion it)))

;;* Noman
(nvp:define-link nvp-link-noman
  :address t
  :collector (nvp-link--overlay-collect 'action 'noman--follow-link)
  (when (numberp it)
    (goto-char (1+ it))
    (call-interactively #'push-button)))

;;* Widgets
(nvp:decl widget-apply-action)
(nvp:define-link nvp-link-widget
  :collector (nvp-link--widget-collect)
  ;; Avoid overlays that might have super high priority
  :style 'pre
  (when (number-or-marker-p it)
    (goto-char it)
    (--when-let (get-char-property it 'button)
      (widget-apply-action it))))

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
