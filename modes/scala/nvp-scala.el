;;; nvp-scala.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ensime nil t)
(nvp:decls :p (ensime sbt) :f (ensime) :v (ensime-auto-connect))
(nvp:auto "projectile" 'projectile-project-p)

;; -------------------------------------------------------------------
;;; Ensime

;; switch between source and REPL buffers
(nvp:repl-switch "ensime" (:repl-mode #'ensime-inf-mode
                           :repl-buffer-name ensime-inf-buffer-name
                           :repl-live-p #'ensime-inf-process-live-p
                           :repl-switch-fn 'pop-to-buffer
                           :repl-history t)
  ;; start ensime server if necessary
  (and (not (ensime-connected-p))
       (nvp-ensime-auto-start))
  (ensime-inf-run-scala))

;; (nvp:repl-switch "sbt" (:repl-mode 'sbt-mode
;;                                    :))
;;; Server
(defun nvp-ensime-auto-start ()
  "Auto-start Ensime if possible."
  (when (and (buffer-file-name) (not (ensime-connected-p)))
    (let ((ensime-prefer-noninteractive t)
          (ensime-auto-connect 'always))
      (ensime-auto-connect))))

;; some functions from spacemacs
(defun nvp-ensime-configure ()
  "Ensure the file exists before starting `ensime-mode'."
  (cond
   ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (ensime-mode +1))
   ((buffer-file-name)
    (add-hook 'after-save-hook (lambda () (ensime-mode +1)) nil t))))

(defun nvp-ensime-maybe-start ()
  (when (buffer-file-name)
    (let ((ensime-buffer (nvp-ensime-buffer-for-file (buffer-file-name)))
          (file (ensime-config-find-file (buffer-file-name)))
          (is-source-file (s-matches? (rx (or "/src/" "/test/"))
                                      (buffer-file-name))))
      (when (and is-source-file (null ensime-buffer))
        (let ((ensime-config-file-name file))
          (save-window-excursion
            (ensime)))))))

(defun nvp-ensime-buffer-for-file (file)
  "Find the ensime server buffer corresponding to FILE."
  (let ((default-directory (file-name-directory file))
        (project-name (projectile-project-p)))
    (when project-name
      (car
       (cl-member-if
        #'(lambda (x) (and
                       (string-match-p "inferior-ensime-server" (buffer-name x))
                       (string-match-p (file-name-nondirectory project-name) x)))
        (buffer-list))))))

(defun nvp-ensime-disconnect (arg)
  "Shutdown ensime server, with prefix shutdown all of them."
  (interactive "P")
  (if arg
      (ensime-disconnect-all)
    (ensime-disconnect)))

;; -------------------------------------------------------------------
;;; SBT
(nvp:decl-prefix "sbt")
(require 'sbt-mode nil t)

(defun nvp-scala-pop-to-sbt (new-frame)
  "Get SBT REPL for this project or start (w/ prefix in `NEW FRAME')."
  (interactive "P")
  (when (not (comint-check-proc (sbt:buffer-name)))
    (sbt:run-sbt))
  (let ((display-buffer-overriding-action
         (if new-frame '(display-buffer-pop-up-frame) nil)))
    (pop-to-buffer (sbt:buffer-name))))

(defun nvp-ensime-gen-restart ()
  "Regenerate `.ensime' file and restart the ensime server."
  (interactive)
  (progn
    (sbt-command ";ensimeConfig;ensimeConfigProject")
    (ensime-shutdown)
    (ensime)))

(defun nvp-sbt-switch-submode-hook (&rest _ignored)
  (comint-write-input-ring))

(advice-add #'sbt:switch-submode :before #'nvp-sbt-switch-submode-hook)

(provide 'nvp-scala)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-scala.el ends here
