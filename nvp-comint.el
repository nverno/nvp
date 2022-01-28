;;; nvp-comint.el --- Comint helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; - sentinels
;; - manage history
;; - redirect output
;;
;; TODO:
;; - default `comint-input-filter-functions' to ignore blanks, compress newlines
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'comint)
(nvp:decls)

;; kill process before killing buffer -- ensure comint writes history
(defun nvp-comint-kill-proc-before-buffer ()
  (let ((proc (nvp:buffer-process)))
    (when (processp proc)
      (and (derived-mode-p 'comint-mode)
           (comint-write-input-ring))
      (delete-process proc))))


;;; History

;; use if wanting to read history file over tramp connection
;; (defun nvp-create-remote-filename (filename)
;;   (if (file-remote-p default-directory)
;;       (tramp-make-tramp-file-name
;;        (tramp-dissect-file-name default-directory) filename)
;;     filename))

;; write comint-input-ring when buffer is killed: in kill-buffer-hook
(defun nvp-comint-write-history-on-kill ()
  ;; make sure the buffer exists before calling the process sentinel
  (add-hook 'kill-buffer-hook 'nvp-comint-kill-proc-before-buffer nil 'local))

;; Setup history file and hippie expansion
;;;###autoload
(defun nvp-comint-setup-history (filename &rest args)
  (setq comint-input-ring-file-name (expand-file-name filename nvp/cache))
  (comint-read-input-ring 'silent)
  (apply #'nvp-he-history-setup args))
(put 'nvp-comint-setup-history 'lisp-indent-function 1)


;;; I/O
;; evaluate STRING in PROC, but discard output silently
;; TODO: check if this would work better
;; https://github.com/hylang/hy-mode/blob/8699b744c03e0399c049757b7819d69768cac3bc/hy-shell.el#L156
(defun nvp-comint-redirect-silently (proc string &optional prompt)
  (let ((comint-redirect-perform-sanity-check))
    (with-temp-buffer 
      (comint-redirect-send-command-to-process
       string (current-buffer) proc nil 'no-display)
      ;; wait for process to complete
      (with-current-buffer (process-buffer proc)
        (while (and (null comint-redirect-completed) ;ignore output
                    (accept-process-output proc 1)))))
    (with-current-buffer (process-buffer proc)
      (comint-redirect-cleanup)
      (while (and (null comint-redirect-completed)   ;wait for cleanup to finish
                  (accept-process-output proc 1)))
      (and prompt (comint-send-string proc "\n"))))) ;optionally print a new prompt


;;; Font-locking
;; http://www.modernemacs.com/post/comint-highlighting/
;; https://github.com/hylang/hy-mode/blob/master/hy-font-lock.el
(defun nvp-comint-font-lock-keywords (kwd)
  (-let (((matcher . match-highlights) kwd))
    ;; matcher can be function or regexp
    (macroexpand-all
     `((lambda (limit)
         (when ,(if (symbolp matcher)
                    `(,matcher limit)
                  `(re-search-forward ,matcher limit t))
           ;; while SUBEXP can be anything, this search always can use zero
           (-let ((start (match-beginning 0))
                  ((comint-last-start . comint-last-end) comint-last-prompt)
                  (state (syntax-ppss)))
             (and (> start comint-last-start)
                  ;; make sure not in comment/string
                  ;; have to manually do this in custom MATCHERs
                  (not (or (nth 3 state) (nth 4 state)))))))
       ,(if (listp match-highlights)
            `,@match-highlights
          `,match-highlights)))))


(provide 'nvp-comint)
;;; nvp-comint.el ends here
