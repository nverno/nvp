;;; nvp-dired.el --- dired helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'dired)
(nvp:decls :p (org dired conda comint)
           :v (nvp-dired-external-filelist-cmd nvp-dired-external-program))
(nvp:auto "f" 'f-same-p)

(declare-function org-texinfo-export-to-info "ox-texinfo")
(declare-function org-latex-export-to-pdf "ox-latex")
(declare-function dired-read-shell-command "dired-aux")
(declare-function dired-dwim-target-directory "dired-aux")
(declare-function comint-mode "comint")

;;; Add dired directory to z.sh database
(defun nvp-dired-z-add-directory ()
  (let ((z-sh (expand-file-name "include/bash.d/z.sh" nvp/bin)))
    (when (file-exists-p z-sh)
      (start-process-shell-command
       "bash" nil
       (concat
        "env _Z_DATA=$HOME/.cache/.z "
        (format "bash -c '. %s; cd %s && _z --add \"$(pwd)\"'"
                z-sh (expand-file-name default-directory)))))))

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
        (when (setq item (dired-get-filename 'verbatim 'no-error))
          (unless (member item '("." ".." nil))
            (push (cons item (copy-marker (dired-move-to-filename))) index-alist)))
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

(defun nvp-dired-next-dirline (arg)
  "Move to next directory line, wrapping if at end."
  (interactive "p")
  (let ((wrap-pt (if (> arg 0) (point-min) (point-max))))
    (condition-case nil
        (funcall #'dired-next-dirline arg wrap-pt)
      (error (funcall #'dired-next-dirline arg)))))

(defun nvp-dired-prev-dirline (arg)
  (interactive "p")
  (nvp-dired-next-dirline (- arg)))

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

(nvp:def-keymap nvp-repeat-dired-dirline-map
  :repeat t
  "n" #'dired-next-dirline
  "p" #'dired-prev-dirline)

(nvp:def-keymap nvp-repeat-dired-move-map
  :wrap (dired-next-dirline dired-prev-dirline) :wrap-pref "nvp"
  :repeat (nvp-dired-next5 nvp-dired-prev5)
  "n" #'nvp-dired-next5
  "p" #'nvp-dired-prev5
  "i" #'nvp-dired-scroll-down
  "k" #'nvp-dired-scroll-up
  "l" #'nvp/dired-prev-dirline
  "j" #'nvp/dired-next-dirline)


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

(nvp:advise-commands #'nvp-dired-w/o-dwim :around (dired-do-rename dired-do-copy))


;; -------------------------------------------------------------------
;;; Dired actions

(defun nvp-dired-do-eval (form &optional silent)
  "Evaluate FORM in marked files.
With prefix SILENT, save files without prompting."
  (interactive (list (read--expression "Form to run on files: ") current-prefix-arg))
  (save-window-excursion
    (mapc (lambda (file)
            (with-current-buffer (find-file file)
              (eval form)
              (when (or silent (y-or-n-p "Save file? "))
                (save-buffer))))
          (dired-get-marked-files))))

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
                       (cons (dired-file-name-at-point) nil))
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

(defun nvp-dired-mark-files-extension (unflag-p)
  "Mark all files by extension."
  (interactive "P")
  (let ((ext (file-name-extension (dired-file-name-at-point)))
        (dired-marker-char (if unflag-p ?\s dired-marker-char)))
    (when ext
      (dired-mark-if (looking-at-p (concat ".*." ext)) "file extension"))))


;; -------------------------------------------------------------------
;;; External actions

(nvp:auto "conda-env" 'conda-env-read-env)
(defvar nvp-dired-vlc-files)

(defun nvp-dired-external-open (&optional arg)
  "Open current or marked dired files in external app."
  (interactive "p")
  (let ((files (or (dired-get-marked-files)
                   (dired-file-name-at-point)))
        (grouped nil))
    (mapc (lambda (path)
            (let ((process-connection-type nil)
                  (ext (file-name-extension path)))
              (pcase ext
                ("ipynb"
                 (let ((env (conda-env-read-env)))
                   (start-process-shell-command
                    "jupyter-notebook"
                    (nvp:comint-buffer :name "*jupyter-notebook*")
                    (format "cd %s && source activate %s && jupyter-notebook &"
                            (read-directory-name "Jupyter root: ") env))))
                ((pred (string-match-p nvp-dired-vlc-files))
                 (if arg
                     (start-process-shell-command
                      "vlc" nil
                      (format "vlc -L %s &" (shell-quote-argument path)))
                   (push (shell-quote-argument path) grouped)))
                (_ (start-process "" nil "xdg-open" path)))))
          files)
    (when grouped
      (start-process-shell-command
       "vlc" nil (format "vlc -L %s &" (mapconcat 'identity grouped))))))

(defun nvp-dired-external-explorer ()
  "Open directory in gui."
  (interactive)
  (nvp:with-gnu/w32
      (let ((process-connection-type nil)
            (prog (if (file-exists-p "/usr/bin/gvfs-open")
                      "/usr/bin/gvfs-open"
                    "/usr/bin/xdg-open")))
        (start-process "" nil prog "."))
    (w32-shell-execute "open" default-directory)))

(defun nvp-dired-external ()
  "Open file[s] in external program."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dolist (file files)
      (when-let*
          ((prog (assoc (file-name-extension file) nvp-dired-external-program)))
        (let ((cmd (cdr (assoc 'cmd prog))))
          (nvp:with-gnu/w32
              (start-process cmd nil cmd file)
            (w32-shell-execute (cdr (assoc 'cmd prog)) file)))))))


;;; Install info

(defun nvp-dired-convert-and-install-info (info-dir &optional keep-info)
  "Convert marked .org files to .info and install.
If marked files are already .info files, just install. By default, installs
to `nvp/info' if INFO-DIR is nil, but can be prompted with \\[universal-argument]."
  (interactive
   (let ((dir (nvp:prefix nil nvp/info
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
              (nvp:with-process "install-info"
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
      (nvp:with-gnu (dired-read-shell-command "& on %s: " current-prefix-arg files))
      files)))
  (nvp:with-gnu/w32
      (let (list-switch)
        (start-process
         cmd nil shell-file-name
         shell-command-switch
         (format
          "nohup 1>/dev/null 2>/dev/null %s \"%s\""
          (if (and (> (length file-list) 1)
                   (setq list-switch
                         (cadr (assoc cmd nvp-dired-external-filelist-cmd))))
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
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-dired.el ends here
