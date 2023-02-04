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

(nvp:advise-commands #'nvp-leet-result-layout :before (leetcode-try leetcode-submit))
;; (nvp:unadvise-commands #'nvp-leet-result-layout (leetcode-try leetcode-submit))

;; -------------------------------------------------------------------
;;; Rust

(defun nvp@leet-set-language ()
  (let ((dir (expand-file-name
              (pcase leetcode-prefer-language
                ("java" "java/src")
                (_ leetcode-prefer-language))
              "~/class/leetcode")))
    (setq leetcode-directory dir)))
(advice-add 'leetcode-set-prefer-language :after #'nvp@leet-set-language)

;; slugify with '_' instead of '-'
(defun nvp@leet-slugify-title (title)
  (let* ((str1 (replace-regexp-in-string "[\s-]+" "_" (downcase title)))
         (res (replace-regexp-in-string "[(),]" "" str1)))
    res))

(nvp:decl f-filename f-dirname)
(defun nvp@leet-get-slug-title (_code-buf)
  (f-filename (f-dirname (buffer-file-name))))

(defsubst nvp-leet--mod-name (buf-name)
  (concat "p"
          (file-name-sans-extension
           (replace-regexp-in-string "-" "_" buf-name))))

(defun nvp-leet--add-mod (mod)
  (with-current-buffer
      (find-file-noselect
       (expand-file-name "main.rs" leetcode-directory))
    (point-min)
    (insert (format "pub mod %s;\n" mod))
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun nvp@leet-get-code-buffer (buf-name)
  (let* ((mod (nvp-leet--mod-name buf-name))
         (file (concat
                (file-name-as-directory
                 (expand-file-name mod leetcode-directory))
                "mod.rs")))
    (unless (file-exists-p file)
      (nvp-leet--add-mod mod))
    (with-current-buffer (find-file-noselect file)
      (rename-buffer buf-name)
      (current-buffer))))

(defun nvp-leet-set-rust ()
  (interactive)
  (setq leetcode-prefer-language "rust")
  (setq leetcode-directory "~/class/leetcode/rust/src/problem")
  ;; (advice-add 'leetcode--get-code-buffer :override #'nvp@leet-get-code-buffer)
  )

(defun nvp-leet-unset-rust ()
  (interactive)
  (setq leetcode-prefer-language "cpp"
        leetcode-directory "~/class/leetcode")
  (advice-remove 'leetcode--get-code-buffer #'nvp@leet-get-code-buffer))

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
