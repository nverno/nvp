;;; nvp-d.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-complete)
  (require 'cl-lib)
  (defvar yas-snippet-dirs))
(nvp-decls)
(autoload 'nvp-env-path-add "nvp-env")

;; server binaries
(defvar nvp-d-dcd-bin-dir
  (expand-file-name "dcd/bin" (getenv "DEVEL")))

;; -------------------------------------------------------------------
;;; Install 

;; (defun nvp-d-install ()
;;   (interactive)
;;   (nvp-with-install-script nvp-d--dir "install"))

;; install dcd-server from git
;; (defun nvp-d-install-dcd (&optional post-action)
;;   (interactive)
;;   (unless (file-exists-p nvp-d-dcd-bin-dir)
;;     (nvp-with-process-log
;;       (start-process-shell-command
;;        "bash" (nvp-process-buffer)
;;        (format "bash -l %s install_dcd_server"
;;                (expand-file-name "install.sh" nvp-d--dir)))
;;       :pop-on-error
;;       (and post-action (funcall post-action)))))

;; init environment variables
;; (defsubst nvp-d-ensure-process-env ()
;;   (if (file-exists-p nvp-d-dcd-bin-dir)
;;       (nvp-env-path-add nvp-d-dcd-bin-dir)
;;     (if (y-or-n-p "Install dcd? ")
;;         (nvp-d-install-dcd)
;;       (user-error "No dcd-server... aborting."))))

;; -------------------------------------------------------------------
;;; Commands 

(defun nvp-d-info ()
  (interactive)
  (browse-url "http://prowiki.org/wiki4d/wiki.cgi?EditorSupport/EmacsEditor"))

;; from http://prowiki.org/wiki4d/wiki.cgi?EditorSupport/EmacsEditor

;; -------------------------------------------------------------------
;;  Many of DMD's compilation error messages lack a category prefix (like
;;  "Error" or "Warning"). Without the category, the messages don't match
;;  Emacs' built-in error matching regexps used for M-x next-error after
;;  a M-x compile. That makes M-x compile unable to take you to the the
;;  file and line where the error is.
;; Add this to your .emacs and it should fix it.
;; (require 'compile)
;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
;;    1 2 nil (3 . 4)))

;; Also (this is Windows-specific) but if you don't have your DMD stuff
;; always added to your path by default, you can add it in your .emacs. I
;; always have an environment variable set called "DMDDIR" that points to
;; where DMD is installed. Then I add this to my .emacs (uncomment the
;; INCLUDE part if you have some default includes you want to add):
;; ; modify executable path to include DMD stuff (windows)
;; (setq exec-path (cons "C:/dmd/bin" exec-path))
;; ; Or this more tricky version that uses a DMDDIR environment variable
;; ;(setq exec-path (cons 
;; ;		 (concat (replace-regexp-in-string "\\\\" "/" (getenv "DMDDIR")) "/dmd/bin")
;; ;		 exec-path))
;; (let ((DMDDIR (getenv "DMDDIR")))
;;   (setenv "PATH" 
;; 	  (concat DMDDIR "\\dmd\\bin;" 
;; 		  DMDDIR "\\dm\\bin;" 
;; 		  DMDDIR "\\dsss\\bin;"
;; 		  (getenv "PATH") ))
;;   (setenv "LIB" 
;; 	  (concat DMDDIR "\\dmd\\lib;" 
;; 		  DMDDIR "\\dm\\lib;" 
;; 		  (getenv "LIB") ))
;; ;  (setenv "INCLUDE" 
;; ;	  (concat DMDDIR "\\dmd\\lib;" 
;; ;		  DMDDIR "\\dm\\lib;" 
;; ;		  (getenv "INCLUDE") ))
;;   )
;; -------------------------------------------------------------------

;;; Newline dwim

(nvp-newline nvp-d-newline-dwim nil
  :pairs (("{" "}") ("(" ")"))
  :comment-re (" *\\(?:/\\*\\|\\*\\)" . "\\*/ *")
  :comment-start " * ")

;;; Run/Compile

(eval-when-compile
  (defmacro nvp-d-compile-with-completion (args cache cmd prompt
                                                  &rest body)
    (declare (indent defun))
    `(nvp-complete-compile-with-completion "dmd" ,args ,cache
       ,cmd ,prompt ,body)))

(defvar nvp-d--dmd-switches nil)

;; compile with minibuffer completion for dmd switches
(defun nvp-d-compile ()
  (interactive)
  (nvp-complete-compile "dmd" "dmd -g -wi %s" '("--help")
                        ;; 'nvp-d--dmd-switches
                        "dmd" "dmd args: "))

;; FIXME:
(defun nvp-d-compile-and-run (arg)
  (interactive "P")
  (ignore arg))

(provide 'nvp-d)
;;; nvp-d.el ends here
