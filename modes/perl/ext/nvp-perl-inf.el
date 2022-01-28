;;; nvp-perl-inf.el --- init inf perl -*- lexical-binding: t; -*-
(eval-when-compile (require 'nvp-macro))
(require 'nvp-perl)
(nvp:decls :v (nvp-perl-pde-path inf-perl-start-file inf-perl-shell-program))

;; if non-nil, install Emacs::PDE perl module,
;; otherwise, just use patched version for inf-perl
(defvar nvp-perl-use-pde nil)

;; path to pde, or where to install it; for inf-perl
(nvp:defvar nvp-perl-pde-path (expand-file-name "pde" "~/.emacs.d"))

(when nvp-perl-use-pde
  (add-to-list 'load-path nvp-perl-pde-path))
(require 'inf-perl nil t)

;; inf-perl/pde settings
(setq inf-perl-shell-program (expand-file-name "tools/psh.pl" perl-tools--dir))
(setq inf-perl-start-file (expand-file-name "tools/.pshrc" perl-tools--dir))
(defvar pde-perl-program "perl")

;; ------------------------------------------------------------
;;; Install PDE

(defun nvp-perl-inf-install ()
  (when (and nvp-perl-use-pde
             (not (file-exists-p nvp-perl-pde-path)))
    (let ((default-directory perl-tools--dir))
      (set-process-sentinel
       (start-process "bash" "*nvp-install*" "bash"
                      "-c" ". ./tools/utils.sh && install_pde")
       #'(lambda (p _m)
           (when (zerop (process-exit-status p))
             (pop-to-buffer "*nvp-install*")
             (require 'inf-perl)))))))

;; If mark active, send region, otherwise send current line. Start perl if
;; not active.  Don't send cursor to perl buffer (or actually send it back
;; since the `inf-perl-start' pops to that buffer.
(defun nvp-perl-inf-send ()
  (interactive)
  (let ((buf (current-buffer)))
    (save-mark-and-excursion
     (or (buffer-live-p "*perl*")
         (nvp-perl-inf-start)))
    (if (and mark-active transient-mark-mode)
        (inf-perl-send-region (region-beginning)
                              (region-end))
      (inf-perl-send-line))
    (switch-to-buffer-other-window buf)))

(defvar nvp-perl--perl-buffer)
(defun nvp-perl-inf-switch (arg)
  (interactive "P")
  (if (eq major-mode 'inf-perl-mode)
      (pop-to-buffer nvp-perl--perl-buffer)
    (let ((buff (current-buffer)))
      (inf-perl-switch-to-end-perl arg)
      (setq nvp-perl--perl-buffer buff))))

;;;###autoload
(defun nvp-perl-inf-start (&optional buffer)
  (interactive)
  (condition-case nil
      (inf-perl-start buffer)
    (error
     (message "Inf perl not installed, installing now.")
     (nvp-perl-inf-install))))

(provide 'nvp-perl-inf)
;;; nvp-perl-inf.el ends here
