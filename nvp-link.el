;;; nvp-link.el --- links -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'avy)
(nvp:decls :v (org-link-any-re) :f (ace-link ace-link-org))

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
                helpful-mode))
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
    (nvp-mark-minor-mode          . nvp-link-mark))
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

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-ace-link ()
  "Call the ace link function for the current `major-mode'"
  (interactive)
  (if-let ((action (nvp-ace-link-action (current-buffer))))
      (funcall action)
    (unless (and ace-link-fallback-function
                 (funcall ace-link-fallback-function))
      (error "%S isn't supported" major-mode))))

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

(defun nvp-link-mark ()
  (interactive)
  (let ((pt (avy-with nvp-link-mark
              (avy-process
               (--map (cdr it) (nvp-link--mark-collect))
               (avy--style-fn avy-style)))))
    (when pt
      (funcall #'nvp-mark-goto-marker-at-point))))

;;; Shell
(nvp:decl comint-next-prompt)

(defun nvp-link--shell-collect ()
  (let (res)
    (nvp:with-restriction 'visible
      (let ((end (1- (point-at-bol (point-max)))))
        (goto-char (point-min))
        (while (progn (comint-next-prompt 1) (< (point) end))
          (push (cons (string-chop-newline (nvp:tap 'tap 'line))
                      (point-at-bol))
                res))))
    (nreverse res)))

(defun nvp-link-shell ()
  (interactive)
  (let ((pt (avy-with nvp-link-shell
              (avy-process
               (--map (cdr it) (nvp-link--shell-collect))
               (avy--style-fn avy-style)))))
    (avy-action-yank-line pt)))

;; use actual links in agenda buffer - the default is just the same
;; as `avy-goto-link'
;; (defun nvp-link--org-agenda-collect ()
;;   (let (res)
;;     (save-excursion
;;       (nvp:with-restriction 'visible
;;         (goto-char (point-min))
;;         (while (re-search-forward org-link-any-re nil t)
;;           (push (cons (buffer-substring-no-properties
;;                        (match-beginning 0) (match-end 0))
;;                       (match-beginning 0))
;;                 res))))
;;     (nreverse res)))

(with-eval-after-load 'ace-link
  (advice-add 'ace-link-org-agenda :override #'ace-link-org))

(provide 'nvp-link)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-link.el ends here
