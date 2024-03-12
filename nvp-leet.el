;;; nvp-leet.el --- leetcode helpers -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'let-alist))
(require 'leetcode nil t)
(require 'f)
(nvp:decls :f (nvp-leetcode-hook) :p (leetcode))

(defvar-local nvp-leet-problem-id nil)
(defvar-local nvp-leet-window-configuration nil)
(defvar nvp-leet-directory (expand-file-name "leetcode" (getenv "CLASSDIR")))
(defvar-local nvp-leet--indirect-buffer nil)
(defvar-local nvp-leet--line-skip-re nil)

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
            (let ((lim (line-end-position))
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

(defun nvp-leet-reset-layout ()
  "Reset window layout to initial solving layout."
  (interactive)
  (when nvp-leet-window-configuration
    (set-window-configuration nvp-leet-window-configuration)))

(defun nvp-leet-pre-submit-hook ()
  (when nvp-leet--line-skip-re
    (let ((skip-re (concat "^[ \t]*$\\|" nvp-leet--line-skip-re)))
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (while (and (not (eobp))
                      (looking-at skip-re))
            (goto-char (match-end 0))
            (when (or (not (bolp)) (looking-at-p "[ \t]*$"))
              (forward-line 1))))
        (narrow-to-region (point) (point-max)))))
  (when nvp-leet-window-configuration
    (set-window-configuration nvp-leet-window-configuration))
  (save-current-buffer
    (with-selected-window
        (display-buffer (leetcode--result-buffer-name nvp-leet-problem-id))
      (enlarge-window 25))))

(nvp:advise-commands #'nvp-leet-pre-submit-hook :before (leetcode-try leetcode-submit))

;; -------------------------------------------------------------------
;;; Language setups

(defun nvp@leet-set-language ()
  (let ((dir (expand-file-name
              (pcase leetcode-prefer-language
                ("java" "java/src")
                ("golang" "golang/src")
                (_ leetcode-prefer-language))
              nvp-leet-directory)))
    (setq leetcode-directory dir)))
(advice-add 'leetcode-set-prefer-language :after #'nvp@leet-set-language)

(defun nvp-leet--insert-preamble (preamble &optional skip-prefix)
  (setq-local nvp-leet--line-skip-re (or skip-prefix (regexp-quote preamble)))
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p nvp-leet--line-skip-re)
      (insert (concat preamble "\n")))))

(defun nvp-leet--setup-buffer ()
  (indent-region (point-min) (point-max))
  (goto-char (point-max))
  (forward-line -1)
  (while (not (or (bobp) (looking-at-p "^\\s-*$")))
    (forward-line -1))
  (indent-according-to-mode))

(declare-function nvp-ruby-yardocify-types "nvp-ruby")

(defun nvp@leet-setup (orig-fn problem problem-info)
  (let ((title (leetcode-problem-title problem-info)))
    (funcall orig-fn problem problem-info)
    (let* ((buf-name (leetcode--get-code-buffer-name title))
           (buf (and buf-name (get-buffer buf-name))))
      (when buf
        (with-current-buffer buf
          (save-restriction
            (widen)
            (nvp-leet-minor-mode 1)
            (setq nvp-leet-problem-id (leetcode-problem-id problem-info))
            (pcase leetcode-prefer-language
              ("rust" (let ((buffer buf))
                        (run-with-timer
                         0.2 nil (lambda () (nvp-leet-setup-rust buffer 'and-go)))))
              ("golang" (nvp-leet--insert-preamble "package leetcode"))
              ("racket" (nvp-leet--insert-preamble "#lang racket"))
              ("cpp" (nvp-leet--insert-preamble
                      "#include <bits/stdc++.h>\n#include \"./ds/leet.hpp\"\nusing namespace std;"))
              ("ruby" (nvp-ruby-yardocify-types (point-min) (point-max))
               (setq-local nvp-leet--line-skip-re "require_relative"))
              ("python3" (nvp-leet--insert-preamble
                          "from typing import List, Optional"
                          "from \\(typing\\|\\.leet\\)"))
              (_ nil)))
          (nvp-leet--setup-buffer))))))

(advice-add #'leetcode--start-coding :around #'nvp@leet-setup)

;;; Sql
(defun nvp-leet-start-coding-database ()
  (interactive)
  (if (eq major-mode 'leetcode--problem-detail-mode)
      (let ((leetcode-directory (expand-file-name "sql" nvp-leet-directory)))
        (save-excursion
          (goto-char (next-button (point-min)))
          (call-interactively #'push-button)))
    (user-error "Call from leetcode detail buffer.")))

;;; Rust
(eval-when-compile
  (defsubst nvp:leet--rust-mod-name (buf)
    (format "p%s" (replace-regexp-in-string "-" "_" (f-base (buffer-name buf))))))

(defun nvp-leet--rust-add-mod (modname modfile)
  (with-current-buffer (find-file-noselect modfile)
    (goto-char (point-min))
    (let ((line (format "mod %s;" modname)))
      (unless (save-excursion (re-search-forward line nil t 1))
        (insert (concat line "\n"))
        (save-buffer)))
    (kill-buffer)))

(defun nvp-leet-setup-rust (buffer &optional and-go)
  (interactive (list (current-buffer) t))
  (let* ((file (buffer-file-name buffer))
         (dir (if file (f-dirname file)
                (expand-file-name "rust" nvp-leet-directory)))
         (srcdir (expand-file-name "src/problem" dir))
         (modname (nvp:leet--rust-mod-name buffer))
         (modfile (expand-file-name "mod.rs" srcdir))
         (fname (expand-file-name (concat modname ".rs") srcdir))
         (contents (with-current-buffer buffer (buffer-string)))
         (problem-id nvp-leet-problem-id))
    (nvp-leet--rust-add-mod modname modfile)
    (with-current-buffer (find-file fname)
      (nvp-leet-indirect-minor-mode 1)
      (setq nvp-leet--indirect-buffer buffer
            nvp-leet-problem-id problem-id)
      (and (eq (point-min) (point-max))
           (insert contents))
      (nvp-leet--insert-preamble "use crate::Solution;" "use crate")
      (nvp-leet--setup-buffer)
      (if and-go (pop-to-buffer (current-buffer))
        (current-buffer)))))

;; -------------------------------------------------------------------
;;; Minor mode

(defun nvp-leet-browse ()
  (interactive)
  (with-current-buffer (nvp-leet-problem-buffer)
    (save-excursion
      (goto-char (point-min))
      (forward-button 2)
      (push-button))))

(eval-and-compile
  (defconst nvp--bindings-leet
    '(("a" . nvp-leet-add-examples)
      ("d" . leetcode-daily)
      ("e" . nvp-leet-add-examples)
      ("q" . leetcode-quit)
      ("r" . nvp-leet-reset-layout)
      ("w" . nvp-leet-browse))))

(nvp:bindings nvp-leet-minor-mode nil :create t
  :prefix-key "<f2>L"
  :with (leet)
  ("s" . leetcode-submit)
  ("t" . leetcode-try))
(define-key nvp-leet-minor-mode-map (kbd "C-c C-c") #'leetcode-try)

(define-minor-mode nvp-leet-minor-mode "Leetcode minor mode."
  :lighter " LC"
  :keymap nvp-leet-minor-mode-map
  (unless nvp-leet-window-configuration
    (setq nvp-leet-window-configuration (current-window-configuration))))

;;; Indirect minor mode

(eval-when-compile
  (defmacro nvp:leet-indirect-call (fn)
    `(progn
       (cl-assert (buffer-live-p nvp-leet--indirect-buffer))
       (let ((content (save-excursion
                        (goto-char (point-min))
                        (when nvp-leet--line-skip-re
                          (while (looking-at-p nvp-leet--line-skip-re)
                            (forward-line)))
                        (buffer-substring-no-properties (point) (point-max))))
             (window-conf nvp-leet-window-configuration))
         (with-current-buffer nvp-leet--indirect-buffer
           (erase-buffer)
           (insert content)
           (let ((nvp-leet-window-configuration nil))
             (,fn))
           (when window-conf (set-window-configuration window-conf)))))))

(defun nvp-leet-indirect-try ()
  (interactive)
  (nvp:leet-indirect-call leetcode-try))

(defun nvp-leet-indirect-submit ()
  (interactive)
  (nvp:leet-indirect-call leetcode-submit))

(nvp:bindings nvp-leet-indirect-minor-mode nil :create t
  :prefix-key "<f2>L"
  :with (leet)
  ("s" . nvp-leet-indirect-submit)
  ("t" . nvp-leet-indirect-try))
(define-key nvp-leet-indirect-minor-mode-map
            (kbd "C-c C-c") #'nvp-leet-indirect-try)

(nvp:bindings nvp-leet-indirect-minor-mode-map nil
  :create t
  :prefix-key "<f2> L"
  ("t" . nvp-leet-indirect-try)
  ("s" . nvp-leet-indirect-submit))

(define-minor-mode nvp-leet-indirect-minor-mode
  "Leetcode indirect minor mode."
  :lighter " LCi"
  :keymap nvp-leet-indirect-minor-mode-map
  (if nvp-leet-indirect-minor-mode
      (setq nvp-leet-window-configuration (current-window-configuration))))

;;; Results
(define-minor-mode nvp-leet-result-mode "Leetcode results."
  :lighter " LR")

(defun nvp@leet-show-results (problem-id _submission-detail)
  (with-current-buffer (get-buffer-create (leetcode--result-buffer-name problem-id))
    (nvp-leet-result-mode 1)))

(advice-add #'leetcode--show-submission-result :after #'nvp@leet-show-results)

(provide 'nvp-leet)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-leet.el ends here
