;;; nvp-lsp.el --- LSP addons -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; + Enhanced lsp log I/O mode with font-locking and folding
;; + transient menu
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls :p (hs lsp) :f (lsp--erase-log-buffer hs-toggle-hiding))

;;; Transient
(defvar lsp-log-io)
(nvp:transient-toggle nvp-lsp-menu
  lsp-log-io)

(transient-define-infix nvp-lsp-menu--toggle-lsp-server-trace ()
  "Toggle lsp server tracing."
  :class 'transient-lisp-variable
  :variable 'lsp-server-trace
  :reader (lambda (prompt initial-input history)
            (completing-read prompt '("off" "messages" "verbose") nil t
                             initial-input history)))

;;;###autoload(autoload 'nvp-lsp-menu "nvp-lsp" nil t)
(transient-define-prefix nvp-lsp-menu () "Lsp"
  ["Debug"
   ("i" "I/O logging" nvp-lsp-menu--toggle-lsp-log-io)
   ("s" "Server tracing" nvp-lsp-menu--toggle-lsp-server-trace)])

;; -------------------------------------------------------------------
;;; Log IO
;;
;; Adds fontification & code folding (hideshow) to lsp log mode

(defface nvp-lsp-logio-send-face
  '((t :foreground "#00bfff" :bold t))
  "Face for sends."
  :group 'lsp)

(defface nvp-lsp-logio-receive-face
  '((t :foreground "#7fea01" :bold t))
  "Face for receives."
  :group 'lsp)

(defvar nvp-lsp-logio-keywords
  (let ((types (rx (or "request" "response" "notification"))))
    `(("^\\(\\(?:Param\\|Result\\)s?\\):" (1 font-lock-keyword-face))
      (,(concat "\\(Received\\)\\s-+" types) (1 'nvp-lsp-logio-receive-face))
      (,(concat "\\(Sending\\)\\s-+" types) (1 'nvp-lsp-logio-send-face))
      ("^\\\[\\(Trace - \\)\\([^\]]+\\)\\\]"
       (0 (prog1 ()
            (let* ((expr (match-string-no-properties 1))
                   (len (length expr)))
              (compose-region (match-beginning 1) (match-end 1) ""))))
       (0 'font-lock-type-face t)))))

(with-eval-after-load 'hideshow
  (dolist (mode '(lsp-log-io-mode)) ;; nvp-lsp-logio-mode
    (unless (assoc mode hs-special-modes-alist)
      (push `(,mode "\\(\\\[Trace[^\]]+\\\]\\|\\(Param\\|Result\\)s?:\\)" "^\\s-*$")
            hs-special-modes-alist))))

(defvar hs-hide-comments-when-hiding-all)
(defvar-local nvp--lsp-logio-hidden nil)

;;;###autoload
(defun nvp-lsp-logio-setup (&optional fontify)
  ;; variables to set for hideshow to work
  (setq-local comment-start "")
  (setq-local comment-end "")
  (setq-local forward-sexp-function
              (lambda (&optional arg)
                (re-search-forward "^\\s-*$" nil t (or arg 1))))
  (setq-local hs-hide-comments-when-hiding-all nil)
  (setq-local font-lock-defaults (list nvp-lsp-logio-keywords))
  (when fontify
    (font-lock-flush)
    (font-lock-ensure)))

(defun nvp-lsp-logio-toggle-all ()
  "Toggle hiding of all entries."
  (interactive)
  (hs-minor-mode)
  (save-excursion
    (funcall-interactively
     (if (setq nvp--lsp-logio-hidden (not nvp--lsp-logio-hidden))
         #'hs-hide-all
       #'hs-show-all))))

(defun nvp-lsp-logio-toggle ()
  (interactive)
  (hs-minor-mode)
  (save-excursion (call-interactively #'hs-toggle-hiding)))

(defun nvp-lsp-log-io-clear (&optional all)
  "Erase current lsp log buffer.
With \\[universal-argument] ALL, erase all session log buffers."
  (interactive "P")
  (lsp--erase-log-buffer (and all t)))

(provide 'nvp-lsp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lsp.el ends here
