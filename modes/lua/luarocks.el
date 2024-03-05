;;; luarocks.el --- Luarocks -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls :f (xterm-color-colorize-buffer))
(autoload 'f-read-text "f")


(defcustom luarocks-command "luarocks"
  "Command to run luarocks executable."
  :type 'path
  :group 'luarocks)

(defcustom luarocks-browser-function #'browse-url-default-browser
  "Function to browse urls."
  :type 'function
  :group 'luarocks)

(defvar luarocks--global-args
  '("--dev" "--lua-dir" "--lua-version" "--namespace" "--server" "--no-project"))

(defvar luarocks--menu-defaults
  (list "--recache"                     ; recompute installed rocks
        "--json"                        ; config format
        "--module"                      ; default open/jump
        "--no-project"                  ; global rocks
        "--append"                      ; append paths
        (concat "--lua-version "
                (string-trim
                 (shell-command-to-string "luarocks config lua_version")))))

(defun luarocks--filter-args (allowed args)
  (--filter (member (car (string-split it)) (append allowed luarocks--global-args))
            args))

(defun luarocks--call (cmd split-lines &rest args)
  (let ((err-file (make-temp-file "luarocks"))
        (args (cl-loop for a in (flatten-list args)
                       nconc (if (string-search " " a)
                                 (string-split a)
                               (list a)))))
    (unwind-protect
        (with-temp-buffer
          (let* ((status (apply #'call-process "luarocks" nil
                                (list (current-buffer) err-file) nil
                                cmd (delq nil args)))
                 (res (string-trim
                       (buffer-substring-no-properties (point-min) (point-max)))))
            (if (zerop status)
                (if split-lines (string-split res "[\n\r]+" t "[ \t]+") res)
              (user-error
               (format "%s: exit %d"
                       (string-trim (f-read-text err-file)) status)))))
      (when (file-exists-p err-file)
        (delete-file err-file)))))

(defvar luarocks--installed-rocks nil)
(defun luarocks--gather-rocks (&optional recompute &rest args)
  (or (and (not recompute) luarocks--installed-rocks)
      (setq luarocks--installed-rocks
            (cl-loop for line in (apply #'luarocks--call "list" t "--porcelain" args)
                     for parts = (string-split line)
                     collect (cons (car parts) (nth 3 parts))))))

(defun luarocks-read-rock (&optional prompt recompute nil-ok-p &rest args)
  "Read installed rock and optionally RECOMPUTE installed."
  (let ((res (completing-read
              (or prompt "Rock: ")
              (apply #'luarocks--gather-rocks recompute args) nil (not nil-ok-p))))
    (when (and (not nil-ok-p) (member res '(nil "")))
      (user-error "Rock required"))
    res))

(defun luarocks--modules (rock &rest args)
  "Modules and locations for ROCK."
  (cl-mapcan (lambda (line)
               (when (string-prefix-p "module" line)
                 (list (cdr (split-string line)))))
             (apply #'luarocks--call "show" t "--porcelain" rock args)))

(defun luarocks-find-file (entry)
  (let ((browse-url-browser-function luarocks-browser-function)
        (file (pcase entry
                ((pred stringp) entry)
                ((pred vectorp) (elt entry 1))
                (_ (user-error "unhandled")))))
    (pcase (file-name-extension file)
      ("html" (browse-url (browse-url-file-url (expand-file-name file))))
      (_ (find-file-other-window file)))))

(eval-when-compile
  (defmacro luarocks:menu--read-rock (&optional prompt nil-ok-p allowed-args)
    `(let ((args (transient-args transient-current-command)))
       (list (luarocks-read-rock ,prompt (member "--recache" args) ,nil-ok-p
                                 (luarocks--filter-args nil args))
             (luarocks--filter-args ,allowed-args args)))))

;;;###autoload
(defun luarocks-open (rock &optional args)
  "Open ROCK location.
Locations are homepage, docs, or modules."
  (interactive (luarocks:menu--read-rock
                "Open: " nil '("--home" "--module" "--docs")))
  (let ((global-args (luarocks--filter-args nil args)))
    (cond
     ((member "--home" args)
      (apply #'luarocks--call "doc" nil rock "--home" "--porcelain" global-args))
     ((member "--module" args)
      (let* ((mods (luarocks--modules rock global-args))
             (mod (if (length= mods 1) (car mods)
                    (assoc-string (completing-read "Module: " mods nil t) mods)))
             (loc (and mod (cadr mod))))
        (luarocks-find-file loc)))
     (t
      (let ((docs (apply #'luarocks--call "doc"
                         t rock "--list" "--porcelain" global-args)))
        (if (length= docs 1)
            (luarocks-find-file (car docs))
          (let* ((prefix (cadr (completion-pcm--merge-completions docs '(any))))
                 (len (length prefix)))
            (nvp:with-tabulated-list :name (format "luarocks[%s]" rock)
              :format [("File" 40 t) ("Source" 50 t)]
              :entries (cl-loop for d in docs
                                for k = (substring d len)
                                collect (list k `[,k ,(abbreviate-file-name d)]))
              :action (lambda (_id entry) (luarocks-find-file entry))))))))))

;;;###autoload
(defun luarocks-jump-to-module (rock)
  (interactive (list (luarocks-read-rock "Rock: " current-prefix-arg)))
  (luarocks-open rock '("--module")))

;;;###autoload
(defun luarocks-jump-to-docs (rock)
  (interactive (list (luarocks-read-rock "Rock: " current-prefix-arg)))
  (luarocks-open rock '("--docs")))

;;;###autoload
(defun luarocks-config (&optional args)
  "Call luarocks config.
When displaying config with as Json (with --json), it's prettified with Jq
and displayed in `json-ts-mode'. Otherwise, display in `lua-ts-mode'."
  (interactive (list (luarocks--filter-args
                      '("--json" "--unset" "--scope" "--key" "--value")
                      (transient-args transient-current-command))))
  (let ((global-args (luarocks--filter-args nil args))
        (key-val "")
        (jq-cmd "")
        flags)
    (dolist (a args)
      (pcase a
        ("--json" (push a flags) (setq jq-cmd " | jq"))
        ("--unset" (push a flags))
        ((guard (string-prefix-p "--scope" a)) (push a flags))
        ((guard (string-match "^--key " a))
         (setq key-val (concat (substring a (match-end 0)) " " key-val)))
        ((guard (string-match "^--value " a))
         (setq key-val (concat key-val (substring a (match-end 0)))))
        (_ nil)))
    (with-current-buffer (get-buffer-create "*luarocks[config]*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process-shell-command
         (format "luarocks config %s %s %s"
                 (mapconcat 'identity (append flags global-args) " ")
                 key-val jq-cmd)
         nil (current-buffer) t))
      (if (not (string-empty-p jq-cmd))
          (json-ts-mode)
        (lua-ts-mode))
      (goto-char (point-min))
      (view-mode)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun luarocks-search (&optional query args)
  "Search rocks for QUERY with ARGS.
Results are in `tabulated-list-mode'. The default selection action installs
the rock at point."
  (interactive (list (read-string "Search: ")
                     (luarocks--filter-args
                      '("--binary" "--source")
                      (transient-args transient-current-command))))
  (let ((entries (apply #'luarocks--call "search" t query "--porcelain" args)))
    (nvp:with-tabulated-list
      :name (format "Search: %s" query)
      :format [("Rock" 15) ("Version" 10) ("Type" 10) ("Url" 50)]
      :entries (cl-loop for e in entries
                        for parts = (split-string e)
                        collect (list (car parts) `[,@parts]))
      :action (lambda (_id entry)
                (let ((rock (elt entry 0))
                      (version (elt entry 1)))
                  (when (y-or-n-p (format "Install %s v%s? " rock version))
                    (let ((proc-buf (get-buffer-create
                                     (format "*luarocks-install[%s]*" rock))))
                      (nvp:with-process "luarocks"
                        :proc-buff proc-buf
                        :proc-args ("install" rock)
                        :on-success (progn (nvp-indicate-modeline
                                            (format "Installed '%s'" rock))
                                           (kill-buffer))
                        :on-failure (progn (xterm-color-colorize-buffer)
                                           (view-mode)
                                           (nvp-indicate-modeline "oops" 'failure)
                                           (pop-to-buffer (current-buffer))))
                      (display-buffer proc-buf)))))
      (setq tabulated-list-sort-key '("Rock" . nil)))))


(defun luarocks-path (&optional args)
  (interactive (list (luarocks--filter-args
                      '("--append" "--export")
                      (transient-args transient-current-command))))
  (let ((export (when (member "--export" args)
                  (setq args (--filter (equal it "--export") args))
                  t)))
    (if export
        (pcase-dolist (`(,var . ,cmd) '(("LUA_PATH" . "--lr-path")
                                        ("LUA_CPATH" . "--lr-cpath")))
          (let ((path (apply #'luarocks--call "path" nil cmd args)))
            (setenv var (concat path (if (equal var "LUA_PATH") ";./?.lua;;"
                                       ";./?.so;;")))))
      (nvp:with-results-buffer :buffer "*luarocks[path]*"
        (insert (apply #'luarocks--call "path" nil args))))))

(defun luarocks-kill-buffers (&optional _args)
  (interactive (list (transient-args transient-current-command)))
  (--map (when (string-prefix-p "*luarocks" (buffer-name it))
           (kill-buffer it))
         (buffer-list)))

;;;###autoload(autoload 'luarocks-menu "luarocks" nil t)
(transient-define-prefix luarocks-menu ()
  "Luarocks menu."
  :value luarocks--menu-defaults
  :incompatible '(("--home" "--docs" "--module"))
  ["Arguments"
   ("-r" "Recache rocks" ("-r" "--recache"))
   ("--dev" "Enable dev rocks" ("--dev" "--dev"))
   ("--lua-dir" "Lua installation" ("--lua-dir" "--lua-dir ")
    :class transient-option)
   ("--lua-version" "Lua version to use" ("--lua-version" "--lua-version ")
    :class transient-option)
   ("--namespace" "Namespace" ("--namespace" "--namespace ")
    :class transient-option)
   ("--server" "Server" ("--server" "--server ") :class transient-option)
   ("--no-project" "Dont use project tree" ("--no-project" "--no-project"))]
  [["Open"
    ("h" "Homepage" ("-h" "--home"))
    ("d" "Docs" ("-d" "--docs"))
    ("m" "Module" ("-m" "--module"))]
   ["Search"
    ("--source" "Source only" ("--source" "--source"))
    ("--binary" "Binary only" ("--binary" "--binary"))]]
  [ :pad-keys 2
    ["Path"
     ("-a" "Append to PATH" ("-a" "--append"))
     ("-e" "Export lua c/path" ("-e" "--export"))]
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
   ("s" "Search" luarocks-search)
   ("K" "Kill buffers" luarocks-kill-buffers)])

(provide 'luarocks)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; luarocks.el ends here
