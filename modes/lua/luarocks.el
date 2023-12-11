;;; luarocks.el --- Luarocks -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'transient)

(defcustom luarocks-command "luarocks"
  "Command to run luarocks executable."
  :type 'path
  :group 'luarocks)

(defvar luarocks-browser-function #'browse-url-default-browser)

(defvar luarocks--installed-rocks nil)
(defun luarocks--gather-rocks (&optional recompute)
  (or (and (not recompute) luarocks--installed-rocks)
      (setq luarocks--installed-rocks
            (cl-loop for line in (process-lines "luarocks" "list" "--porcelain")
                     for parts = (string-split line)
                     collect (cons (car parts) (nth 3 parts))))))

(defun luarocks-read-rock (&optional prompt recompute nil-ok-p)
  "Read installed rock and optionally RECOMPUTE installed."
  (let ((res (completing-read
              (or prompt "Rock: ")
              (luarocks--gather-rocks recompute) nil (not nil-ok-p))))
    (when (and (not nil-ok-p) (member res '(nil "")))
      (user-error "Rock required"))
    res))

(defun luarocks--modules (rock)
  "Modules and locations for ROCK."
  (cl-mapcan (lambda (line)
               (when (string-prefix-p "module" line)
                 (list (cdr (split-string line)))))
             (process-lines "luarocks" "show" "--porcelain" rock)))

(defun luarocks-find-file (entry)
  (let ((browse-url-browser-function luarocks-browser-function)
        (file (pcase entry
                ((pred stringp) entry)
                ((pred vectorp) (elt entry 1))
                (_ (user-error "unhandled")))))
    (pcase (file-name-extension file)
      ("html" (browse-url (browse-url-file-url (expand-file-name file))))
      (_ (find-file-other-window file)))))

(defun luarocks-command-to-string (&rest args)
  (with-temp-buffer
    (let* ((status (apply #'process-file luarocks-command
                          nil (current-buffer) nil args))
           (res (string-chop-newline
                 (buffer-substring-no-properties (point-min) (point-max)))))
      (unless (zerop status)
        (user-error "%s: exit %s" res status))
      res)))

(eval-when-compile
  (defmacro luarocks:menu--read-rock (&optional prompt nil-ok-p)
    `(let ((args (transient-args transient-current-command)) recache res)
       (dolist (a args)
         (pcase a
           ("--recache" (setq recache t))
           (_ (push a res))))
       (list (luarocks-read-rock ,prompt recache ,nil-ok-p) res))))

(defun luarocks--filter-args (prefix args)
  (cl-loop for a in args
           with l = (length prefix)
           if (string-prefix-p prefix a)
           collect (concat "-" (substring a l))))

;;;###autoload
(defun luarocks-open (rock &optional args)
  "Open ROCK location.
Locations are homepage, docs, or modules."
  (interactive (luarocks:menu--read-rock "Open: "))
  (cond
   ((member "--home" args)
    (call-process "luarocks" nil nil nil "doc" rock "--home" "--porcelain"))
   ((member "--module" args)
    (let* ((mods (luarocks--modules rock))
           (mod (if (length= mods 1) (car mods)
                  (assoc-string (completing-read "Module: " mods nil t) mods)))
           (loc (and mod (cadr mod))))
      (luarocks-find-file loc)))
   (t
    (let ((docs (process-lines "luarocks" "doc" rock "--list" "--porcelain")))
      (if (length= docs 1)
          (luarocks-find-file (car docs))
        (let* ((prefix (cadr (completion-pcm--merge-completions docs '(any))))
               (len (length prefix)))
          (nvp:with-tabulated-list :name (format "luarocks[%s]" rock)
            :format [("File" 40 t) ("Source" 50 t)]
            :entries (cl-loop for d in docs
                              for k = (substring d len)
                              collect (list k `[,k ,(abbreviate-file-name d)]))
            :action (lambda (_id entry) (luarocks-find-file entry)))))))))

;;;###autoload
(defun luarocks-jump-to-module (rock)
  (interactive (list (luarocks-read-rock "Rock: " current-prefix-arg)))
  (luarocks-open rock '("--module")))

;;;###autoload
(defun luarocks-config (&optional rock args)
  "Call luarocks config."
  (interactive (luarocks:menu--read-rock "Open: " 'nil-ok-p))
  (let ((rock (or rock ""))
        (key-val "") flags json-p)
    (dolist (a args)
      (pcase a
        ("--json" (push a flags) (setq json-p t))
        ("--unset" (push a flags))
        ((guard (string-prefix-p "--scope" a)) (push a flags))
        ((guard (string-match "^--key " a))
         (setq key-val (concat (substring a (match-end 0)) " " key-val)))
        ((guard (string-match "^--value " a))
         (setq key-val (concat key-val (substring a (match-end 0)))))
        (_ nil)))
    (let ((args (concat key-val " " (mapconcat 'identity flags " ")
                        (and json-p " | jq"))))
      (with-current-buffer (get-buffer-create "*luarocks[config]*")
        (erase-buffer)
        (call-process-shell-command
         (format "luarocks config %s %s" rock args) nil (current-buffer) t)
        (if json-p (json-ts-mode) (lua-ts-mode))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(defun luarocks-path (&optional args)
  (interactive (list (luarocks--filter-args
                      "--path" (transient-args transient-current-command))))
  (cond ((member "--export" args)
         (pcase-dolist (`(,var . ,cmd) '(("LUA_PATH" . "--lr-path")
                                         ("LUA_CPATH" . "--lr-cpath")))
           (let ((path (luarocks-command-to-string "path" cmd)))
             (setenv var (concat path (if (equal var "LUA_PATH") ";./?.lua;;"
                                        ";./?.so;;"))))))
        (t
         (nvp:with-results-buffer :buffer "*luarocks[path]*"
           (apply #'process-file
                  luarocks-command nil (current-buffer) nil "path" args)))))

(defun luarocks-kill-buffers (&optional _args)
  (interactive (list (transient-args transient-current-command)))
  (--map (when (string-prefix-p "*luarocks" (buffer-name it))
           (kill-buffer it))
         (buffer-list)))

;;;###autoload(autoload 'luarocks-menu "luarocks")
(transient-define-prefix luarocks-menu ()
  "Luarocks menu."
  :value '("--recache" "--json" "--module")
  :incompatible '(("--home" "--docs" "--module"))
  ["Arguments"
   ("-r" "Recache" ("-r" "--recache"))]
  [ :pad-keys 2
    ["Open"
     ("h" "Homepage" ("-h" "--home"))
     ("d" "Docs" ("-d" "--docs"))
     ("m" "Module" ("-m" "--module"))]
    ;; TODO: search/show
    ;; ["Search"]
    ["Path"
     ("-a" "Append to PATH" ("--append" "--path-append"))
     ("-e" "Export lua c/path" ("-e" "--path-export"))]
    ["Config"
     ("k" "Key" ("-k" "--key ") :class transient-option)
     ("v" "Value" ("-v" "--value ") :class transient-option)
     ("-s" "Scope" ("-s" "--scope ") :class transient-option
      :choices ("system" "user" "project"))
     ("-j" "Json" ("-j" "--json"))
     ("-U" "Unset" ("-u" "--unset"))]]
  ["Actions"
   ("o" "Open" luarocks-open)
   ("j" "Open" luarocks-open)
   ("p" "Path" luarocks-path)
   ("c" "Config" luarocks-config)
   ;; ("s" "Search" luarocks-search)
   ("K" "Kill buffers" luarocks-kill-buffers)])

(provide 'luarocks)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; luarocks.el ends here
