;; -*- no-byte-compile: t; -*-


;;; Pyenv

;; Set pyenv version from ".python-version" by looking in parent directories.
(defun nvp-python-pyenv-set-local-version ()
  (interactive)
  (let ((root-path (locate-dominating-file default-directory ".python-version")))
    (when root-path
      (let* ((file-path (expand-file-name ".python-version" root-path))
             (version (with-temp-buffer
                        (insert-file-contents-literally file-path)
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))))
        (if (member version (pyenv-mode-versions))
            (pyenv-mode-set version)
          (message "pyenv: version `%s' is not installed (set by %s)"
                   version file-path))))))

;; Set pyenv version matching project name.
(defun nvp-python-projectile-pyenv-mode-set ()
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(with-eval-after-load 'projectile-mode
  (when (and nil (fboundp 'pyenv-mode) (pyenv-mode))
    (add-hook 'projectile-after-switch-project-hook
              #'nvp-python-projectile-pyenv-mode-set)))

;;; Completion

;; goto beginning of current class if in one
(defun nvp-python-beginning-of-class ()
  (let ((start (point)))
    (while (not (or (bobp) (bolp) (looking-at-p "class")))
      (python-nav-backward-block))
    (unless (looking-at-p "class")
      (goto-char start))))

;; name of current class and super class if exists
(defun nvp-python-current-class ()
  (save-excursion
    (nvp-python-beginning-of-class)
    (and (looking-at (nvp:concat "class +\\([A-Z][A-Za-z0-9_]*\\)"
                                 "\\(?:(\\([^)]+\\))\\)?"))
         (cons (match-string 1) (match-string 2)))))

;; ;; complete for current class methods
(defun nvp-python-class-candidates ()
  (interactive)
  (let ((super (cdr (nvp-python-current-class)))
        (start (point)))
    (when super
      (let* ((bnds (bounds-of-thing-at-point 'symbol))
             (tmp (if bnds (car bnds) (point))))
        (goto-char tmp)
        (insert (concat super "."))
        (goto-char (+ 1 start (length super)))
        (anaconda-mode-call
         "complete"
         (lambda (result)
           (let ((company-candidates (anaconda-mode-complete-extract-names result)))
             (delete-region tmp (+ 1 tmp (length super)))
             (company-complete))))))))

(defun nvp-python-class-complete ()
  (interactive)
  (let ((completion-at-point-functions '(nvp-python-class-candidates))
        (company-backends '(company-capf)))
    (company-complete)))
