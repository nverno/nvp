;;; nvp-dired.el --- dired helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'dired)
(nvp-decls :v (nvp-dired-external-filelist-cmd nvp-dired-external-program)
           :f (comint-mode
               org-texinfo-export-to-info org-latex-export-to-pdf comint-mode
               dired-dwim-target-directory dired-read-shell-command  ; dired-aux
               dired-filename-at-point                               ; dired-x
               conda-env-read-env))                                  ; conda-env
(nvp-auto "f" 'f-same-p)

;; -------------------------------------------------------------------
;;; Imenu

;; imenu indexer to simply list all files/dirs except . and ..
;; faster than using prev/next position functions
(defun nvp-dired-imenu-create-index ()
  (let ((case-fold-search t)
        item
        index-alist)
    (goto-char (point-max))
    (beginning-of-line)
    (save-match-data
      (while (not (bobp))
        (when (setq item (dired-get-filename 'verbatim 'no-error)))
        (unless (member item '("." ".." nil))
          (push (cons item (copy-marker (dired-move-to-filename))) index-alist))
        (forward-line -1)))
    index-alist))


;; -------------------------------------------------------------------
;;; Movement 

(defun nvp-dired-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(defun nvp-dired-beginning-of-filename ()
  (interactive)
  (dired-move-to-filename 'no-error))

(defun nvp-dired-beginning-of-buffer ()
  (interactive)
  (or (region-active-p) (push-mark))
  (widen)
  (goto-char (point-min))
  (forward-line 2)
  (dired-move-to-filename 'no-error))

(defun nvp-dired-end-of-buffer ()
  (interactive)
  (or (region-active-p) (push-mark))
  (goto-char (point-max))
  (dired-next-line -1)
  (recenter-top-bottom))

;; or C-x C-j
(defun nvp-dired-find-alternate-file ()
  (interactive)
  (find-alternate-file ".."))

(defun nvp-dired-next5 ()
  (interactive)
  (forward-line 5)
  (dired-move-to-filename))

(defun nvp-dired-prev5 ()
  (interactive)
  (forward-line -5)
  (dired-move-to-filename))

(defun nvp-dired-scroll-up ()
  (interactive)
  (scroll-up-command)
  (dired-move-to-filename))

(defun nvp-dired-scroll-down ()
  (interactive)
  (scroll-down-command)
  (dired-move-to-filename))

(defhydra nvp-dired-hydra (:color red :hint nil)
  "move"
  ("M-n" nvp-dired-next5)
  ("M-p" nvp-dired-prev5)
  ("n" nvp-dired-next5)
  ("p" nvp-dired-prev5)
  ("i" nvp-dired-scroll-down)
  ("k" nvp-dired-scroll-up)
  ("l" dired-prev-dirline)
  ("j" dired-next-dirline))


;; -------------------------------------------------------------------
;;; Advices

;; covers both next & previous
(define-advice dired-next-dirline (:before (&rest _args) "push-mark")
  (or (region-active-p) (push-mark)))

;; #<marker at 171949 in simple.el.gz>
;; FIXME: How to determine the number of C-u before numeric arg????
;; advice for copy/rename w/ multiple open direds
(defun nvp-dired-w/o-dwim (cmd &optional _arg)
  ;; (message "%S" current-prefix-arg)
  (let ((dired-dwim-target (equal '(4) current-prefix-arg)))
    ;; nil just assumes current or marked files
    ;; should be able to pass the numeric argument along properly
    (apply cmd nil)))

(nvp-advise-commands #'nvp-dired-w/o-dwim :around (dired-do-rename dired-do-copy))


;; -------------------------------------------------------------------
;;; Dired actions

;; Add buffer file name or marked file to kill.
;;;###autoload
(defun nvp-dired-or-buffer-filename (&optional arg)
  "Add `buffer-file-name' or marked file in `dired-mode' to kill."
  (interactive "P")
  (if (eq major-mode 'dired-mode)
      (nvp-dired-kill arg)
    (let ((filename (buffer-file-name)))
      (when filename
        (kill-new filename)
        (message "Copied as kill: %s" filename)))))


;; copy absolute filenames as string to kill ring
;; with prefix, separate with ', ', otherwise ' '
(defun nvp-dired-kill (arg)
  (interactive "P")
  (let ((files (condition-case nil
                   (or (dired-get-marked-files)
                       (cons (dired-filename-at-point) nil))
                 (error nil)))
        string)
    (when files
      (setq string (mapconcat 'identity files (if arg ", " " ")))
      (if (eq last-command 'kill-region)
          (kill-append string nil)
        (kill-new string))
      (message "Copied as kill: %S" string))))

;; create new empty FILENAME in dired directory
(defun nvp-dired-touch (filename)
  (interactive (list (read-string "Filename (.gitkeep): " nil nil ".gitkeep")))
  (with-temp-buffer
    (write-file filename)))


;; -------------------------------------------------------------------
;;; External actions

;;-- Open external files

;; Open current or marked dired files in external app.
(defun nvp-dired-external-open ()
  (interactive)
  (let ((files (or (dired-get-marked-files)
                   (dired-file-name-at-point))))
    (nvp-with-gnu/w32
     (mapc (lambda (path)
             (let ((process-connection-type nil)
                   (ext (file-name-extension path)))
               (pcase ext
                 ("ipynb"
                  (require 'conda-env)
                  (let ((env (conda-env-read-env)))
                    (start-process-shell-command
                     "jupyter-notebook"
                     (nvp-comint-buffer :name "*jupyter-notebook*")
                     (format "cd %s && source activate %s && jupyter-notebook &"
                             (read-directory-name "Jupyter root: ") env))))
                 (_ (start-process "" nil "xdg-open" path)))))
           files)  
     (mapc (lambda (path)
             (w32-shell-execute "open" (w32-long-file-name path)))
           files))))

;; Open directory in gui
(defun nvp-dired-external-explorer ()
  (interactive)
  (nvp-with-gnu/w32
      (let ((process-connection-type nil)
            (prog (if (file-exists-p "/usr/bin/gvfs-open")
                      "/usr/bin/gvfs-open"
                    "/usr/bin/xdg-open")))
        (start-process "" nil prog "."))
    (w32-shell-execute "open" default-directory)))

;; Open file[s] in external program 
(defun nvp-dired-external ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dolist (file files)
      (when-let*
          ((prog (assoc (file-name-extension file) nvp-dired-external-program)))
        (let ((cmd (cdr (assoc 'cmd prog))))
          (nvp-with-gnu/w32
              (start-process cmd nil cmd file)
            (w32-shell-execute (cdr (assoc 'cmd prog)) file)))))))

;;-- Install info

(defun nvp-dired-convert-and-install-info (info-dir &optional keep-info)
  "Convert marked .org files to .info and install.
If marked files are already .info files, just install. By default, installs
to `nvp/info' if INFO-DIR is nil, but can be prompted with \\[universal-argument]."
  (interactive
   (let ((dir (nvp-prefix nil nvp/info
                (read-directory-name "Install info to directory: "))))
     (list (directory-file-name dir) (f-same-p dir default-directory))))
  ;; convert org files to .info
  (mapc (lambda (f)
          (when (string= "org" (file-name-extension f))
            (with-current-buffer (find-file-noselect f)
              (org-texinfo-export-to-info)
              (kill-buffer (current-buffer))))
          (let* ((base (file-name-nondirectory (file-name-sans-extension f)))
                 (info (concat base ".info"))
                 (dest (expand-file-name info info-dir)))
            ;; install .info files, delete intermediates, copy to destination
            (when (file-exists-p info)
              (delete-file (concat base ".texi"))
              (copy-file info dest t)
              ;; delete .info file if .org exists and moving to different
              ;; info directory
              (and (not keep-info) 
                   (file-exists-p (concat base ".org"))
                   (delete-file info))
              (nvp-with-process "install-info"
                :proc-args ((concat "--info-dir=" info-dir)
                            (concat "--info-file=" dest))))))
        (dired-get-marked-files)))

;;-- Compress

(defun nvp-dired-zip ()
  (interactive)
  (let* ((files (dired-get-marked-files))
         (out-file (if (or current-prefix-arg (> (length files) 1))
                       (read-from-minibuffer "Archive name: ")
                     (concat (file-name-nondirectory (car files))
                             ".zip")))
         (in-files (mapconcat #'(lambda (x)
                                  (let ((file
                                         (file-name-nondirectory x)))
                                    (if (file-directory-p x)
                                        (concat file "/*")
                                      file)))
                              files " ")))
    (if (= 1 (length files))
        ;; let* ((file (file-name-nondirectory (car files))))
        (start-process "compress" "*compress*" "7za" "a" "-tzip" out-file in-files)
      (start-process-shell-command
       "compress" "*compress*" (format "7za a -tzip %s %s" out-file in-files)))))

(defun nvp-dired-unzip ()
  (interactive)
  (let ((file (dired-get-marked-files)))
    (if (> 1 (length file))
        (user-error "TODO: unzip multiple files")
      (start-process-shell-command "unzip" nil (format "unzip %s" (car-safe file))))))

;;-- Shell

(defun nvp-dired-shell-here ()
  "Open an `shell' in current directory."
  (interactive)
  (nvp-shell 'current-dir))

;;-- Process

(defun nvp-dired-start-process (cmd &optional file-list)
  "Call shell command CMD on FILE-LIST."
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      (nvp-with-gnu (dired-read-shell-command "& on %s: " current-prefix-arg files))
      files)))
  (nvp-with-gnu/w32
      (let (list-switch)
        (start-process
         cmd nil shell-file-name
         shell-command-switch
         (format
          "nohup 1>/dev/null 2>/dev/null %s \"%s\""
          (if (and (> (length file-list) 1)
                   (setq list-switch
                         (cadr
                          (assoc cmd
                                 nvp-dired-external-filelist-cmd))))
              (format "%s %s" cmd list-switch)
            cmd)
          (mapconcat #'expand-file-name file-list "\" \""))))
    (dolist (file file-list)
      (w32-shell-execute "open" (expand-file-name file)))))

;;-- Org to PDF
(defun nvp-dired-convert-org-to-pdf ()
  "Convert marked org files to PDF."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (mapc (lambda (f) (with-current-buffer (find-file-noselect f)
                   (org-latex-export-to-pdf)))
          files)))

;; from abo-abo config
(defun nvp-dired-ediff ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name "File to diff against: "
                                       (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    `(lambda ()
                       (setq ediff-after-quit-hook-internal nil)
                       (set-window-configuration ,wnd))))
      (error "No more that 2 files should be marked"))))

(defun nvp-dired-rsync (dest)
  (interactive
   (list
    (expand-file-name (read-file-name "Rsync to: " (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        (rsync-command "rsync -aP "))
    (dolist (file files)
      (setq rsync-command (concat rsync-command (shell-quote-argument file) " ")))
    (setq rsync-command (concat rsync-command (shell-quote-argument dest)))
    (async-shell-command rsync-command "*rsync*")
    (other-window 1)))

(provide 'nvp-dired)
;;; nvp-dired.el ends here
