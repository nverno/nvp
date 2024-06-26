;;; nvp-clojure.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'cider nil t)
(nvp:decls :f (org-bookmark-jump-unhide
               cider-pprint-eval-defun-at-point
               cider-eval-defun-at-point cider-switch-to-repl-buffer
               cider-repl-closing-return cider-insert-last-sexp-in-repl
               cider-insert-in-repl cider-switch-to-last-clojure-buffer))

;; -------------------------------------------------------------------
;;; Util

(defvar-local nvp-clojure-root nil)
(defun nvp-clojure-root ()
  (or nvp-clojure-root
      (setq nvp-clojure-root
            (locate-dominating-file buffer-file-name "project.clj"))))

;;; FIXME
(defun nvp-clojure--namespace ()
  (replace-regexp-in-string
   "_" "-"
   (replace-regexp-in-string
    "/" "."
    (string-remove-suffix
     "test/"
     (string-remove-suffix
      "src/"
      (string-remove-suffix
       ".clj"
       (substring (buffer-file-name) (nvp-clojure-root))))))))

(defsubst nvp-clojure--namespace-under-test ()
  (replace-regexp-in-string "-test" "" (nvp-clojure--namespace)))

;; -------------------------------------------------------------------
;;; Commands

;; (defun nvp-clojure-align-vectors (beg end)
;;   (interactive "r")
;;   (align-regexp beg end "^ \\[[^ ]+\\(\\s-+\\)" 1 1 t))

;; FIXME: make better if ever use clojure again
;; Find documentation for given SYMBOL online.
(defun nvp-clojure-help-online (symbol)
  (interactive (list (read-from-minibuffer "Symbol: ")))
  (cl-destructuring-bind (x &optional y)
      (split-string symbol "/")
    (browse-url
     (concat "http://clojuredocs.org/clojure."
             (if y x "core")
             (if (string= "" y) "" "/")
             (url-hexify-string (or y x))))))

;; -------------------------------------------------------------------
;;; nREPL

;; https://github.com/howardabrams/dot-files/blob/master/emacs-clojure.org
;; Sends the s-expression located before the point or the active
;; region to the REPL and evaluates it. Then the Clojure buffer is
;; activated as if nothing happened.
(defun nvp-clojure-send-and-evaluate-sexp ()
  (interactive)
  (if (not (region-active-p))
      (cider-insert-last-sexp-in-repl)
    (cider-insert-in-repl
     (buffer-substring (region-beginning) (region-end)) nil))
  (cider-switch-to-repl-buffer)
  (cider-repl-closing-return)
  (cider-switch-to-last-clojure-buffer)
  (message ""))

;; https://github.com/kototama/.emacs.d/blob/master/lisp/config.el
(defun nvp-clojure-send-ns ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (when (re-search-forward ":require" nil 'move)
      (let ((pos-first-require (1+ (point))))
        (re-search-forward ")")
        (let* ((pos-last-require (1- (point)))
               (requires
                (buffer-substring-no-properties pos-first-require
                                                pos-last-require))
               (quoted-requires
                (replace-regexp-in-string "\n +\\(\\[\\)" "\n'[" requires))
               (form (concat "(require '" quoted-requires ")")))
          (cider-switch-to-repl-buffer)
          ;; (switch-to-buffer-other-window (cider-find-or-create-repl-buffer))
          (goto-char (max-char))
          (insert form))))))

;; Evaluate with `cider-eval-defun-at-point' or
;; `cider-pprint-eval-defun-at-point' when prefixed.
(defun nvp-clojure-clj-eval-defun-at-point (&optional arg)
  (interactive "P")
  (if arg
      (cond
       ((eq 16 (car-safe arg))
        (cider-eval-defun-at-point '(16)))
       (t (cider-pprint-eval-defun-at-point)))
    (cider-eval-defun-at-point)))

(define-advice clojure-test-run-tests (:before (&rest_) "save-first")
  (save-buffer))

(define-advice nrepl-load-current-buffer (:before (&rest_) "save-first")
  (save-buffer))

(provide 'nvp-clojure)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-clojure.el ends here
