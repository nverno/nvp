;;; nvp-cool.el --- cool compiler -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

(defvar nvp-cool-root-path "~/.local/cool/")
(defvar nvp-cool-history () "Jumping history.")

;; -------------------------------------------------------------------
;;; Utils

(eval-when-compile
  (defmacro nvp-cool--read-dir (&optional dir prompt)
    (macroexp-let2 nil dir (if dir `(expand-file-name ,dir nvp-cool-root-path)
                             nvp-cool-root-path)
      `(nvp-completing-read
        (nvp-prompt-default ,(or prompt "Directory: ") ,dir)
        (directory-files ,dir 'full "[^.].*[^J]$")
        nil nil nil 'nvp-cool-history ,dir))))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-cool-setenv (dir)
  "Add headers/src to include path."
  (interactive (list (nvp-cool--read-dir "include" "Path to add: ")))
  (nvp-env-add "CPLUS_INCLUDE_PATH" dir)
  (nvp-env-add "CPLUS_INCLUDE_PATH"
   (replace-regexp-in-string "include" "cool/src" dir)))

(defun nvp-cool-jump-to-src (dir)
  "Jump to source directory."
  (interactive (list (or (and (not current-prefix-arg) (car nvp-cool-history))
                         (nvp-cool--read-dir "include"))))
  (dired dir))

(defun nvp-cool-add-path (arg)
  "Add cool executables to path"
  (interactive "P")
  (let ((dir (if arg (ido-read-directory-name
                      "Directory to add to path: " "/")
               (expand-file-name "bin" nvp-cool-root-path))))
   (nvp-env-path-add dir)))

;;;###autoload
(defun nvp-cool-add-bindings ()
  (interactive)
  (nvp-bindings "c++-mode" 'cc-mode
    ("<f2> m j h" . nvp-cool-jump-to-src)
    ("<f2> m e c" . nvp-cool-setenv))
  (nvp-bindings "flex-mode" nil
    ("<f2> m j h" . nvp-cool-jump-to-src)
    ("<f2> m e c" . nvp-cool-setenv)))

;;; FIXME: unused
(defun nvp-cool-vm ()
  "Tramp into vagrant VM with cool compiler setup (windows)."
  (interactive)
  (require 'tramp)
  (add-to-list 'tramp-remote-path "/usr/class/cs143/cool/bin")
  (setq-local tramp-default-user "vagrant")
  (global-set-key [(control x) (control y)]
		  (lambda ()
		    (interactive)
		    (find-file
		     (read-file-name
		      "find tramp file: "
		      "/pscp:vagrant@192.168.32.14:~/"))))
  ;; (setenv \"cool\" \"/plink:compilers@:192.168.56.101:~/\")
  )

(provide 'nvp-cool)
;;; nvp-cool.el ends here
