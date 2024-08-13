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
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'comint)
(nvp:decls :p (ring) :f (ring-ref ring-length))


;;; History

(defvar-local nvp-comint-history-save-on-kill t
  "When non-nil, save history when buffer is killed before killing process.")

(defvar nvp-comint-history-append t
  "When non-nil, writing `comint-input-ring' adds new entries to
`comint-input-ring-file-name' instead of clobbering it.")

(defvar nvp-comint-history-output-filter nil
  "Hook run when writing history entries to file.
Called with history element as argument.")

(defun nvp-comint--write-input-ring ()
  "Modified version of `comint-write-input-ring'.
Applies `nvp-comint-history-output-filter' to history entries.
Binds `comint-input-ring-separator' so local values have effect."
  (cond ((or (null comint-input-ring-file-name)
	     (equal comint-input-ring-file-name "")
	     (null comint-input-ring) (ring-empty-p comint-input-ring))
	 nil)
	((not (file-writable-p comint-input-ring-file-name))
	 (message "Cannot write history file %s" comint-input-ring-file-name))
	(t
	 (let* ((history-buf (get-buffer-create " *Temp Input History*"))
		(ring comint-input-ring)
		(ring-file comint-input-ring-file-name)
                ;; Note(08/05/24): comint should bind this for use in history-buf
                (ring-separator comint-input-ring-separator)
                (filter nvp-comint-history-output-filter)
		(index (ring-length ring)))
	   ;; Write it all out into a buffer first.  Much faster, but messier,
	   ;; than writing it one line at a time.
	   (with-current-buffer history-buf
	     (erase-buffer)
	     (while (> index 0)
	       (setq index (1- index))
	       (insert (funcall (or filter #'identity) (ring-ref ring index))
                       ring-separator))
	     (write-region (buffer-string) nil ring-file nil 'no-message)
	     (kill-buffer nil))))))

(defun nvp-comint-write-input-ring ()
  "Override for `comint-write-input-ring'.
Adds new entries to `comint-input-ring-file-name' instead of clobbering it
when `nvp-comint-history-append' in non-nil.
When add entries, duplicate entries are also removed."
  (if (or (null nvp-comint-history-append)
          (not (file-exists-p comint-input-ring-file-name))
          ;; TODO(08/05/24): remove when separators handled in merge
          (not (equal "\n" comint-input-ring-separator)))
      (nvp-comint--write-input-ring)
    ;; Write history file and ring elements to temp file, then merge
    (let ((histfile comint-input-ring-file-name)
          (comint-input-ring-file-name
           (make-temp-file comint-input-ring-file-name)))
      (nvp-comint--write-input-ring)
      (call-process-shell-command
       (format
        ;; FIXME(08/02/24): don't dedupe history separator lines
        ;; eg. dont clobber separators in `sql-stop'
        (concat "cat \"%s\" >> \"%s\";"
                "awk '!seen[$0]++' \"%s\" > \"%s\"; rm \"%s\"")
        histfile comint-input-ring-file-name
        comint-input-ring-file-name histfile comint-input-ring-file-name)))))

(advice-add 'comint-write-input-ring :override #'nvp-comint-write-input-ring)

(defun nvp-comint-kill-proc-before-buffer ()
  "Kill process before killing buffer to ensure comint writes history."
  (let ((proc (nvp:buffer-process)))
    (when (processp proc)
      (and (derived-mode-p 'comint-mode)
           nvp-comint-history-save-on-kill
           (comint-write-input-ring))
      (delete-process proc))))

(defun nvp-comint--read-history-filename ()
  (let* ((default (bound-and-true-p comint-input-ring-file-name))
         (initial (and default (file-name-nondirectory default))))
    (read-file-name
     "History file: "
     (file-name-as-directory nvp/history) default nil initial)))

(defun nvp-comint-read-history (filename &optional save-current silent)
  "Read history from FILENAME and set as `comint-input-ring-file-name'."
  (interactive (list (nvp-comint--read-history-filename)
                     (and comint-input-ring
                          (not (ring-empty-p comint-input-ring))
                          (y-or-n-p "Save current? "))))
  (unless (derived-mode-p 'comint-mode)
    (user-error "Not a comint"))
  (when save-current
    (nvp-comint-save-history))
  (unless (equal filename comint-input-ring-file-name)
    (setq-local comint-input-ring-file-name filename)
    (comint-read-input-ring silent)))

(defun nvp-comint-write-history (&optional reload silent filename)
  "Save history in `comint-input-ring'.
With prefix, prompt for FILENAME.
When RELOAD is non-nil, reload merged history."
  (interactive
   (list (null current-prefix-arg) nil
         (and current-prefix-arg
              (derived-mode-p 'comint-mode)
              (nvp-comint--read-history-filename))))
  (let ((proc (get-buffer-process (current-buffer)))
        (comint-input-ring-file-name
         (or filename comint-input-ring-file-name)))
    (unless (and (processp proc)
                 (derived-mode-p 'comint-mode)
                 (not (null comint-input-ring-file-name)))
      (user-error
       "Unknown history file (`comint-input-ring-file-name' is nil)."))
    (comint-write-input-ring)
    (when reload
      (comint-read-input-ring silent))))

;;;###autoload
(defun nvp-comint-setup-history (filename &rest args)
  "Setup history file and hippie expansion."
  (setq comint-input-ring-file-name
        (if (file-name-absolute-p filename) filename
          (expand-file-name filename nvp/history)))
  (when (and comint-input-ring
             (ring-empty-p comint-input-ring))
    (comint-read-input-ring 'silent))
  (apply #'nvp-he-history-setup args))
(put 'nvp-comint-setup-history 'lisp-indent-function 1)

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
      (prog1 (string-chop-newline
              (buffer-substring-no-properties
               (point-min)
               (max 1 (1- (line-beginning-position)))))
        (and (buffer-name buf)
             (kill-buffer buf))))))

;; TODO: check if this would work better
;; https://github.com/hylang/hy-mode/blob/8699b744c03e0399c049757b7819d69768cac3bc/hy-shell.el#L156
;; (defun nvp-comint-redirect-silently (proc string &optional prompt)
;;   "Evaluate STRING in PROC, but discard output silently."
;;   (let ((comint-redirect-perform-sanity-check))
;;     (with-temp-buffer
;;       (comint-redirect-send-command-to-process
;;        string (current-buffer) proc nil 'no-display)
;;       ;; wait for process to complete
;;       (with-current-buffer (process-buffer proc)
;;         (while (and (null comint-redirect-completed) ;ignore output
;;                     (accept-process-output proc 1)))))
;;     (with-current-buffer (process-buffer proc)
;;       (comint-redirect-cleanup)
;;       (while (and (null comint-redirect-completed)   ;wait for cleanup to finish
;;                   (accept-process-output proc 1)))
;;       (and prompt (comint-send-string proc "\n"))))) ;optionally print a new prompt

;;; Font-locking
;; http://www.modernemacs.com/post/comint-highlighting/
;; https://github.com/hylang/hy-mode/blob/master/hy-font-lock.el
;; (defun nvp-comint-font-lock-keywords (kwd)
;;   (-let (((matcher . match-highlights) kwd))
;;     ;; matcher can be function or regexp
;;     (macroexpand-all
;;      `((lambda (limit)
;;          (when ,(if (symbolp matcher)
;;                     `(,matcher limit)
;;                   `(re-search-forward ,matcher limit t))
;;            ;; while SUBEXP can be anything, this search always can use zero
;;            (-let ((start (match-beginning 0))
;;                   ((comint-last-start . comint-last-end) comint-last-prompt)
;;                   (state (syntax-ppss)))
;;              (and (> start comint-last-start)
;;                   ;; make sure not in comment/string
;;                   ;; have to manually do this in custom MATCHERs
;;                   (not (or (nth 3 state) (nth 4 state)))))))
;;        ,(if (listp match-highlights)
;;             `,@match-highlights
;;           `,match-highlights)))))


(provide 'nvp-comint)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-comint.el ends here
