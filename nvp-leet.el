;;; nvp-leet.el --- leetcode helpers -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'f)
(require 'leetcode nil t)
(nvp:decls :f (nvp-leetcode-hook) :p (leetcode))


(defvar nvp-leet-root-directory (expand-file-name "leetcode" (getenv "CLASSDIR")))
(defvar-local nvp-leet-problem-id nil)
(defvar-local nvp-leet--line-skip-re nil)
(defvar-local nvp-leet--skip-sexp nil "Non-nil if mode should skip sexps.")

;; -------------------------------------------------------------------
;;; Test cases

(defun nvp-leet--collect-examples ()
  "Collect example inputs from problem description."
  (-when-let (buf (leetcode-detail-buffer nvp-leet-problem-id))
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
    (with-current-buffer (leetcode-test-buffer nvp-leet-problem-id t)
      (save-excursion
        (goto-char (point-max))
        (cl-loop for input in inputs
                 do (progn
                      (delete-horizontal-space)
                      (unless (bolp) (insert "\n"))
                      (insert (mapconcat 'identity input "\n"))))))))

(defun nvp-leet-buffer-code ()
  "Return buffer's code that should be sent."
  (if (null nvp-leet--line-skip-re)
      (buffer-substring-no-properties (point-min) (point-max))
    (let ((skip-re (concat "^[ \t]*$\\|" nvp-leet--line-skip-re)))
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (while (and (not (eobp))
                      (looking-at skip-re))
            (if (and nvp-leet--skip-sexp (not (nvp:line-empty-p))
                     (or (eq t nvp-leet--skip-sexp)
                         (looking-at nvp-leet--skip-sexp)))
                (forward-sexp 1)
              (goto-char (match-end 0)))
            (when (or (not (bolp)) (looking-at-p "[ \t]*$"))
              (forward-line 1))))
        (buffer-substring-no-properties (point) (point-max))))))

(setq leetcode-buffer-code-function #'nvp-leet-buffer-code)

(defun nvp-leet-pre-submit-hook ()
  (leetcode-restore-layout)
  (--when-let (leetcode-result-buffer nvp-leet-problem-id)
    (with-selected-window (get-buffer-window it)
      (enlarge-window 25))))
(add-hook 'leetcode-before-submit-hook #'nvp-leet-pre-submit-hook)


;; -------------------------------------------------------------------
;;; Language setups

(defun nvp-leet-rust-filename (problem-id title-slug suffix)
  (format "p%04d_%s%s" problem-id
          (replace-regexp-in-string "-" "_" title-slug)
          suffix))

(defun nvp-leet-set-language-hook (lang)
  (let ((dir (expand-file-name
              (pcase lang
                ("java" "java/src")
                ("golang" "golang/src")
                ("python" "python3")
                ("rust" "rust/src/problem")
                (_ leetcode-prefer-language))
              nvp-leet-root-directory)))
    (setq leetcode-directory dir))
  (setq leetcode-filename-function (if (string= lang "rust")
                                       #'nvp-leet-rust-filename
                                     #'leetcode--filename-default)))
(add-hook 'leetcode-after-set-language-hook #'nvp-leet-set-language-hook)

(defun nvp-leet--insert-preamble (preamble &optional skip-prefix)
  (setq-local nvp-leet--line-skip-re (or skip-prefix (regexp-quote preamble)))
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p nvp-leet--line-skip-re)
      (insert (concat preamble "\n")))))

(cl-defun nvp-leet-setup-lang (&key preamble skip skip-sexp)
  (setq-local nvp-leet--skip-sexp skip-sexp)
  (cond (preamble (nvp-leet--insert-preamble preamble skip))
        (skip (setq-local nvp-leet--line-skip-re skip))))

(defun nvp-leet--format-buffer ()
  (indent-region (point-min) (point-max))
  (goto-char (point-max))
  (forward-line -1)
  (while (not (or (bobp) (looking-at-p "^\\s-*$")))
    (forward-line -1))
  (indent-according-to-mode))

(defun nvp-leet--rust-add-mod (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (run-with-timer
   0.2 nil
   (lambda ()
     (let* ((modname (string-remove-suffix ".rs" (buffer-name buffer)))
            (modfile (expand-file-name "mod.rs" leetcode-directory)))
       (with-current-buffer (find-file-noselect modfile)
         (goto-char (point-min))
         (let ((line (format "mod %s;" modname)))
           (unless (save-excursion (re-search-forward line nil t 1))
             (insert (concat line "\n"))
             (save-buffer)))
         (kill-buffer))))))

(declare-function nvp-ruby-yardocify-types "nvp-ruby")

;;; TODO(6/14/24): convert to use hook
(defun nvp@leet-setup (orig-fn problem problem-info)
  (let ((problem-id (leetcode-problem-id problem-info)))
    (funcall orig-fn problem problem-info)
    (when-let ((buf (leetcode-code-buffer problem-id)))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (setq nvp-leet-problem-id problem-id)
          (pcase leetcode-prefer-language
            ("rust"
             (nvp-leet--rust-add-mod buf)
             (nvp-leet-setup-lang
              :preamble "use crate::Solution;\nuse std::collections::*;"
              :skip "use crate"))
            ("golang" (nvp-leet-setup-lang
                       :preamble "package leetcode"
                       :skip (rx (or "package" "import \"leetcode/ds\""))))
            ("racket" (nvp-leet-setup-lang
                       :preamble "#lang racket"
                       :skip (rx (or "#lang" "(require" "(module"))
                       :skip-sexp t))
            ("cpp" (nvp-leet-setup-lang
                    :preamble "#include \"./ds/leet.hpp\"\nusing namespace std;"
                    :skip (rx bol (or "#include" "using namespace std"
                                      (seq "typedef" (* nonl) "NaryNode")))))
            ("c" (nvp-leet-setup-lang
                  :preamble "#include \"./ds/leet.h\""
                  :skip "#include"))
            ("typescript" (nvp-leet-setup-lang :skip "import"))
            ("javascript" (nvp-leet-setup-lang :skip "import"))
            ("ruby" (nvp-ruby-yardocify-types (point-min) (point-max))
             (nvp-leet-setup-lang :skip "require_relative"))
            ("python3" (nvp-leet-setup-lang
                        :preamble "from typing import List, Optional"
                        :skip (rx "from " (or "typing" ".leet" "leet"))))
            (_ nil)))
        (nvp-leet--format-buffer)))))

;; (add-hook 'leetcode-solution-minor-mode-hook #'nvp@leet-setup)
(advice-add 'leetcode--start-coding :around #'nvp@leet-setup)

;;; Sql
(defun nvp-leet-start-coding-database ()
  (interactive)
  (unless (derived-mode-p 'leetcode-detail-mode)
    (user-error "Call from leetcode detail buffer"))
  (let ((leetcode-directory (expand-file-name "sql" nvp-leet-root-directory)))
    (save-excursion
      (goto-char (next-button (point-min)))
      (call-interactively #'push-button))))

(provide 'nvp-leet)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-leet.el ends here
