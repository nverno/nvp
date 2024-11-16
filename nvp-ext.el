;;; nvp-ext.el --- External programs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-proc)
(nvp:decls :v (epg-gpg-home-directory))


;;;###autoload
(defun nvp-ext-vagrant-halt (&optional arg)
  "Halt all running vagrant boxes in `vms'.  With prefix, show output
in buffer *vagrant-status*."
  (interactive "P")
  (or nvp/vms (user-error "'nvp/vms' is nil"))
  (nvp:with-process "bash"
    :proc-bufname (and arg "*vagrant-status*")
    :proc-args ((expand-file-name "vms/vagrant-shizzle" nvp/bin) "-l" nvp/vms "-K"))
  (when (not arg)
    (message "Running vagrant-halt...")))

;;;###autoload
(defun nvp-ext-gpg-export (dir &optional name prog)
  "Export GPG public keys matching NAME (default NVP) to DIR using PROG (gpg/2)."
  (interactive "DExport public key to directory: \nsKey name (NVP): ")
  (or prog (setq prog (or (nvp:program "gpg2") (nvp:program "gpg"))))
  (and (equal name "") (setq name "NVP"))
  (unless (and prog (file-exists-p prog))
    (error (if prog "%s not found" "No gpg found") prog))
  (let ((default-directory dir))
    (nvp:with-process prog
      :proc-name "gpg"
      :buffer-fn get-buffer-create
      :proc-args ("--armor" "--output" "public_key.asc" "--export" name))))

;;;###autoload
(defun nvp-ext-gpg-backup (dir)
  "Copy gpg files to directory for backup/export."
  (interactive "DExport gpg files to directory: ")
  (unless (file-exists-p (nvp:program "gpg"))
    (user-error "gpg program not set."))
  (let ((default-directory epg-gpg-home-directory))
    (mapc (lambda (f)
            (copy-file f dir t))
          '("pubring.gpg" "secring.gpg" "trustdb.gpg"))))


(defun nvp-ext-start-process (cmd)
  (start-process
   cmd nil shell-file-name
   shell-command-switch
   (format "nohup 1>/dev/null 2>/dev/null %s" cmd)))

;;;###autoload
(defun nvp-ext-xev ()
  "Run xev with output to emacs buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*xev*")
    (pop-to-buffer (current-buffer))
    (local-set-key (kbd "C-c C-c") 'kill-current-buffer)
    (nvp:with-process "xev"
      :buffer-fn get-buffer-create
      :proc-filter nil
      :on-success (kill-buffer))))

(defun nvp-md5-file (filename)
  "Generate MD5 of FILENAME contents and prepend to `kill-ring'."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents filename)
    (kill-new (md5 (current-buffer)))))

(provide 'nvp-ext)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ext.el ends here
