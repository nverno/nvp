;;; nvp-abbrev-hooks.el --- abbrev expansion predicates -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file shouldn't be loaded until abbrev expansion actually occurs, or
;; other hooks defined here are run.
;;
;; general abbrev hooks
;; - post-insert hook
;; - simple enable-functions
;; - expand hooks
;; - abbrev-edit hook
;;
;; Additional
;; - function to grab previous abbrev
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :f (expand-abbrev-hook
               expand-do-expansion expand-build-marks expand-list-to-markers)
           :v (expand-point expand-list expand-index expand-pos))

;;;###autoload
(defun nvp-abbrev-grab ()
  "Grabs previous symbol if point is at the end of a symbol or if the
`last-input-event' was a space."
  (if (or (and (characterp last-input-event)
               (eq ? (char-syntax last-input-event)))
          (looking-at-p "\\_>"))
      (let ((str (buffer-substring-no-properties
                  (point) (save-excursion (skip-syntax-backward "w_") (point)))))
        ;; so eldoc still displays args, let it know this was a
        ;; `self-insert-command'
        (if (not (equal "" str)) str
          (prog1 nil (setq this-command 'self-insert-command))))))

;; -------------------------------------------------------------------
;;; Post insert
;; C level only calls `expand-abbrev' when preceding char is word syntax
;; so hook into `post-self-insert-hook'
;;;###autoload
(defun nvp-abbrev-expand-after-symbols-hook ()
  (and (memq (car (syntax-after (1- (point)))) '(0 3))
       (setq this-command 'nvp-abbrev-expand-after-symbols)
       (expand-abbrev)))

;; Generate a `post-self-insert-hook' to run `expand-abbrev' using
;; either SYNTAX or CHARS
;;;###autoload
(cl-defun nvp-abbrev-make-post-insert-hook (&key syntax chars)
  ;; (unless (xor syntax chars)
  ;;   (user-error "Must be SYNTAX or CHARS"))
  (let ((sym (cl-gensym "nvp-abbrev-hook"))
        (fn `(lambda ()
               (and
                (or ,@(when syntax
                        `((memq (car (syntax-after (1- (point)))) ',syntax)))
                    ,@(when chars
                        `((memq (char-before (1- (point))) ',chars))))
                (setq this-command 'nvp-abbrev---expand)
                (expand-abbrev)))))
    (defalias `,sym fn)))

;; -------------------------------------------------------------------
;;; Expand Hooks

;; allow abbrevs to expand inside parens
;; (2/14/20) for some reason, can no longer wrap `eolp' around
;; `expand-abbrev-hook' with `cl-letf' without needing to reevaluate
;; `expand-abbrev-hook' in order for it to work...
;;;###autoload
(defun nvp-abbrev-expand-in-paren-hook ()
  (require 'expand)
  ;; this used to work fine:
  ;; (nvp:with-letf #'eolp #'(lambda () (not (eq (char-syntax (char-after)) ?w)))
  ;;   (expand-abbrev-hook))
  (if (not (eq (char-syntax (char-after)) ?w))
      (let ((p (point)))
        (setq expand-point nil)
        (if (and (eq (char-syntax (preceding-char)) ?w)
                 (expand-do-expansion))
            (progn
              (if expand-point
                  (progn
                    (if (vectorp expand-list)
                        (expand-build-marks expand-point))
                    (indent-region p expand-point nil))
                (if (listp expand-list)
                    (setq expand-index 0
                          expand-pos (expand-list-to-markers expand-list)
                          expand-list nil)))
              (run-hooks 'expand-expand-hook)
              t)
          nil))
    nil))
(put 'nvp-abbrev-expand-in-paren-hook 'no-self-insert t)

;; -------------------------------------------------------------------
;;; Expansion predicates

;; dont expand in strings/comments
;;;###autoload
(defun nvp-abbrev-expand-p ()
  ;;(memq this-command '(expand-abbrev nvp-abbrev---expand))
  (not (nvp:ppss 'soc)))

;; don't expand in strings/comments or after [_.-:]
;;;###autoload
(defun nvp-abbrev-expand-not-after-punct-p (&optional chars)
  (not (or (memq last-input-event (or chars '(?_ ?. ?- ?:)))
           (nvp:ppss 'soc))))

;; -------------------------------------------------------------------
;;; Abbrev table modes

;;;###autoload
(defun nvp-abbrev-edit-hook ()
  (setq-local imenu-generic-expression '((nil "^(\\(.*\\))" 1))))
;;;###autoload(add-hook 'edit-abbrevs-mode-hook #'nvp-abbrev-edit-hook)

(provide 'nvp-abbrev-hooks)
;;; nvp-abbrev-hooks.el ends here
