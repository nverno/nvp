;;; nvp-config.el --- config file helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/config-tools
;; Last modified: <2019-02-22 19:22:08>
;; Created:  10 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(autoload 'nvp-env-substitute-vars "nvp-env")

;;;###autoload
(defun nvp-config-xev ()
  "Run xev with output to emacs buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*xev*")
    (pop-to-buffer (current-buffer))
    (local-set-key (kbd "C-c C-c") 'kill-this-buffer)
    (nvp-with-process "xev"
      :buffer-fn get-buffer-create
      :proc-filter nil
      :on-success (kill-buffer))))

;; ------------------------------------------------------------
;;; Config files

;; Read config filename lines, expanding environment variables in key-value pairs
;; key-value pairs are separated by SEPARATORS and value may be quoted
;; lines beginning with COMMMENTS regex are ignored
;; separators default to ":=" and comments default to '#'
;; Return list of (key . value) pairs
(defun nvp-config-read-file (filename &optional separators comments)
  (setq separators (regexp-quote (or separators ":=")))
  (setq comments (regexp-quote (or comments "#")))
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((key-val-regex
           (concat "^\\([^" separators "\n]+\\)[" separators "]+\\([^\n]+\\)"))
          (vars))
      (while (not (eobp))
        (when (and (not (looking-at-p comments))
                   (looking-at key-val-regex))
          ;; expand enviroment variables and remove quotes from values
          (push (cons (string-trim (match-string-no-properties 1))
                      (nvp-env-substitute-vars
                       (match-string-no-properties 2) 'unquote))
                vars))
        (forward-line 1))
      vars)))

(provide 'nvp-config)

;;; nvp-config.el ends here
