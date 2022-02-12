;;; nvp-link.el --- links -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :v (org-link-any-re) :f (ace-link ace-link-org))

(defvar nvp-link-supported-modes
  '(Info-mode
    help-mode Man-mode woman-mode Custom-mode
    org-agenda-mode org-mode
    compilation-mode compilation-shell-minor-mode
    grep-mode ag-mode rg-mode
    magit-commit-mode
    gnus-article-mode gnus-summary-mode
    eww-mode w3m-mode mu4e-view-mode notmuch-show-mode
    elfeed-show-mode erc-mode
    term-mode vterm-mode eshell-mode
    telega-chat-mode
    sldb-mode slime-xref-mode slime-inspector-mode
    indium-inspector-mode indium-debugger-frames-mode
    cider-inspector-mode))


;; sort windows left-to-right: see `winner-sorted-window-list'
(defun nvp-sort-window-list (windows)
  (sort windows
        (lambda (x y)
          (cl-loop for a in (window-edges x)
                   for b in (window-edges y)
                   while (= a b)
                   finally return (< a b)))))

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
                              (memq (buffer-local-value 'major-mode buf)
                                    nvp-link-supported-modes))))
                   (with-selected-window (car (nvp-sort-window-list it))
                     (prog1 t
                       (call-interactively #'ace-link)))))))
          (call-interactively #'ace-link))))))

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
