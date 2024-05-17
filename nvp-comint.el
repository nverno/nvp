;;; nvp-comint.el --- Comint helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; - sentinels
;; - manage history
;; - redirect output
;;
;; Debugging variables:
;; - xterm-color-debug
;; - comint-redirect-verbose
;;
;; TODO:
;; - default `comint-input-filter-functions' to ignore blanks, compress newlines
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'comint)
(nvp:decls :p (ring) :f (ring-ref ring-length))


;; kill process before killing buffer -- ensure comint writes history
(defun nvp-comint-kill-proc-before-buffer ()
  (let ((proc (nvp:buffer-process)))
    (when (processp proc)
      (and (derived-mode-p 'comint-mode)
           (comint-write-input-ring))
      (delete-process proc))))


;;;###autoload
(defun nvp-comint-history-remove-duplicates (&optional silent)
  "Remove duplicates from history and write history file.
Prompt unless SILENT or `noninteractive'."
  (interactive)
  (and noninteractive (setq silent t))
  (let ((proc (nvp:buffer-process)))
    (unless (and (processp proc)
                 (derived-mode-p 'comint-mode)
                 (not (null comint-input-ring-file-name)))
      (user-error "Unknown history file (`comint-input-ring-file-name' is nil)."))
    (when (or silent (y-or-n-p "Remove history duplicates?"))
      (let* ((history-buf (get-buffer-create " *Temp Input History*"))
             (ring comint-input-ring)
             (file comint-input-ring-file-name)
             (initial-size (ring-length ring))
             (index initial-size)
             (hash (make-hash-table :test #'equal))
             (hist (let ((h ()) str)
                     (while (> index 0)
                       (setq index (1- index))
                       (setq str (ring-ref ring index))
                       (unless (gethash str hash nil)
                         (puthash str 1 hash)
                         (push str h)))
                     h))
             (final-size (length hist))
             (index final-size))
        (with-current-buffer history-buf
          (erase-buffer)
          (while (> index 0)
            (setq index (1- index))
            (insert (nth index hist) comint-input-ring-separator))
          (write-region (buffer-string) nil file nil 'no-message)
          (kill-buffer nil))
        (comint-read-input-ring 'silent)
        (unless silent
          (nvp:msg "%s reduced from %s to %s" :append t :clobber "Remove history"
            (file-name-nondirectory file) initial-size final-size))))))


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
(defun nvp-comint-redirect-to-string (command)
  (let* ((proc (nvp:buffer-process))
         (buf (generate-new-buffer "*nvp*" t))
         (comint-redirect-filter-functions '(xterm-color-filter)))
    (comint-redirect-send-command command buf nil t)
    (with-current-buffer (process-buffer proc)
      (while (and (null comint-redirect-completed) ;ignore output
                  (accept-process-output proc 1))))
    (comint-redirect-cleanup)
    (with-current-buffer buf
      ;; drop the last line from 2-line prompt
      (goto-char (point-max))
      (forward-line -1)
      (end-of-line)
      (prog1
          (string-chop-newline
           (buffer-substring-no-properties
            (point-min)
            (max 1 (1- (line-beginning-position)))))
        (and (buffer-name buf)
             (kill-buffer buf))))))

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
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-comint.el ends here
