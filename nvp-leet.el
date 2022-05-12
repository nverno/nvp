;;; nvp-leet.el --- leetcode helpers -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'let-alist))
(require 'leetcode)
(require 'aio)
(nvp:decls :f (nvp-leetcode-hook))

(defvar-local nvp-leet-problem-id nil)

(defsubst nvp-leet-problem-buffer ()
  (get-buffer (leetcode--detail-buffer-name nvp-leet-problem-id)))

;; -------------------------------------------------------------------
;;; Test cases

(defun nvp-leet--collect-examples ()
  "Collect example inputs from problem description."
  (-when-let (buf (nvp-leet-problem-buffer))
    (let (inputs)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^Input:[^=]+= *" nil t)
            (let ((lim (point-at-eol))
                  cur)
              (condition-case nil
                  (progn
                    (while (< (point) lim)
                      (let ((beg (point))
                            (end (progn (forward-sexp) (point))))
                        (when (> end beg)
                          (push (buffer-substring beg end) cur)))
                      (re-search-forward "[^=]+= *" lim)))
                (error (progn
                         (setq cur (nreverse cur))
                         (push cur inputs)
                         (goto-char lim))))))))
      inputs)))

(defun nvp-leet-add-examples ()
  "Add all examples from description to tests."
  (interactive)
  (-when-let (inputs (nvp-leet--collect-examples))
    (with-current-buffer
        (get-buffer-create (leetcode--testcase-buffer-name nvp-leet-problem-id))
      (save-excursion
        (goto-char (point-max))
        (cl-loop for input in inputs
                 do (progn
                      (delete-horizontal-space)
                      (unless (bolp) (insert "\n"))
                      (insert (mapconcat 'identity input "\n"))))))))

;; -------------------------------------------------------------------
;;; Window Configuration

(defvar-local nvp-leet-window-configuration nil)

(defun nvp-leet-reset-layout ()
  "Reset window layout to initial solving layout."
  (interactive)
  (when nvp-leet-window-configuration
    (set-window-configuration nvp-leet-window-configuration)))

(defun nvp-leet-result-layout ()
  (when nvp-leet-window-configuration
    (set-window-configuration nvp-leet-window-configuration))
  (save-current-buffer
    (with-selected-window
        (display-buffer (leetcode--result-buffer-name nvp-leet-problem-id))
      (enlarge-window 25))))

(nvp:advise-commands #'nvp-leet-result-layout :before '(leetcode-try leetcode-submit))

;; -------------------------------------------------------------------
;;; Minor mode

(defun nvp-leet-browse ()
  (interactive)
  (with-current-buffer (nvp-leet-problem-buffer)
    (save-excursion
      (goto-char (point-min))
      (forward-button 2)
      (push-button))))

(nvp:bindings nvp-leet-mode nil
  :create t
  :prefix-key "<f2>L"
  ("a" . nvp-leet-add-examples)
  ("d" . leetcode-daily)
  ("e" . nvp-leet-add-examples)
  ("q" . leetcode-quit)
  ("r" . nvp-leet-reset-layout)
  ("s" . leetcode-submit)
  ("t" . leetcode-try)
  ("w" . nvp-leet-browse))
(define-key nvp-leet-mode-map (kbd "C-c C-c") #'leetcode-try)

(defun nvp-leet--setup-buffer (problem-info)
  "Set variables in problem's source code buffer."
  (setq-local nvp-leet-problem-id (leetcode-problem-id problem-info)))

(defun nvp@leet-maybe-setup (orig-fn problem problem-info)
  (let ((title (leetcode-problem-title problem-info)))
    (funcall orig-fn problem problem-info)
    (let* ((buf-name (leetcode--get-code-buffer-name title))
           (buf (and buf-name (get-buffer buf-name))))
      (when buf
        (with-current-buffer (get-buffer buf-name)
          (nvp-leet--setup-buffer problem-info)
          (nvp-leet-mode 1))))))
(advice-add #'leetcode--start-coding :around #'nvp@leet-maybe-setup)

(define-minor-mode nvp-leet-mode "Leetcode minor mode."
  :lighter " LC"
  :keymap nvp-leet-mode-map
  (unless nvp-leet-window-configuration
    (setq nvp-leet-window-configuration (current-window-configuration))))

(provide 'nvp-leet)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-leet.el ends here
