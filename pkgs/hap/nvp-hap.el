;;; nvp-hap.el --- help-at-point functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Help at point:
;; - display help for context around point in first applicable (1) popup tooltip,
;;   (2) help buffer, or (3) external application
;; - use transient bindings to execute actions (unless external)
;;
;; Backends are expected to (1) determine thing at point, (2) respond to either
;; 'doc-string or 'doc-buffer, providing a string or buffer containing help for
;; the thing at point.
;; A response to 'doc-buffer should be a list of '(buffer start end),
;; where start and end, if non-nil delimit the region containing relevant info.
;; When nil, point-min/max are used.
;;
;; Backend commands and expected results:
;; - thingatpt => string or nil
;; - doc-string => string
;; - doc-buffer => list '(<buffer> [start] [end])
;;
;; Backends defined here:
;; - info
;; - elisp
;; - company
;;
;; Refs:
;; popup.el/pos-tip.el/quickhelp.el to truncate pop-tips
;;
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'pos-tip)
(require 'company)
(require 'compile)                      ; compilation faces
(nvp:decls :v (info-lookup-other-window-flag)
           :f (nvp-hap-treesit-local-active-p))
(nvp:auto "info-look" 'info-lookup-select-mode 'info-lookup-guess-default)


(defvar nvp-hap-verbose nil
  "Non-nil for verbose messaging.")

(defvar-local nvp-hap-backend nil
  "Active backend.")

;; see `company--group-lighter' for idea to display current backend in mode-line
(defvar nvp-hap-popup-max-lines 25
  "Max lines to display in popup.")

(defvar nvp-hap-popup-timeout 60)

(defvar nvp-hap--saved-window-configuration ())

(defvar nvp-hap--electric-commands
  '(scroll-other-window scroll-other-window-down mwheel-scroll))

;; get first result from `nvp-help-at-point-functions'
;; (defsubst nvp-hap--call (&rest args)
;;   (let ((result (apply #'run-hook-with-args-until-success
;;                        'nvp-help-at-point-functions args)))
;;     (and result (eq (car args) 'doc-buffer)
;;          (setq nvp-hap--doc-buffer result))
;;     result))

(defun nvp-hap-prefix-action (prefix)
  (let ((val (prefix-numeric-value prefix)))
    (cons (and (> val 4) 'prompt) val)))

(defun nvp-hap-thing-at-point (prefix &optional type prompt completions)
  "Get thing at point of TYPE (default \\='symbol).
If nothing found, or C-u C-u PREFIX arg, read from minibuffer prompting
with PROMPT (default \"Describe: \") using COMPLETIONS if non-nil."
  (pcase-let ((`(,force-prompt-p . ,_val) (nvp-hap-prefix-action prefix)))
    (let ((sym (thing-at-point (or type 'symbol) t)))
      (if (or force-prompt-p (not sym))
          (let* ((prompt (or prompt "Describe: "))
                 (sym (string-trim
                       (if completions
                           (completing-read prompt completions nil t)
                         (read-from-minibuffer prompt sym)))))
            (unless (string-blank-p sym)
              sym))
        sym))))

;;; Window configurations
;; `company--electric-restore-window-configuration', `company--electric-do'
;; FIXME: this very simple window config. fails often
(defun nvp-hap--electric-restore-windows ()
  (when (and nvp-hap--saved-window-configuration
             (not (memq this-command nvp-hap--electric-commands)))
    (set-window-configuration nvp-hap--saved-window-configuration)
    (setq nvp-hap--saved-window-configuration nil)))

(defmacro nvp-hap--electric-do (&rest body)
  (declare (indent 0) (debug t))
  `(progn
     (setq nvp-hap--saved-window-configuration (current-window-configuration))
     ,@body))

;;; Keymap
(defvar nvp-hap-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-v")   #'scroll-other-window)
    (define-key map (kbd "M-V")   #'scroll-other-window-down)
    (define-key map "h"           #'nvp-hap-pop-to-buffer)
    (define-key map (kbd "C-h")   #'nvp-hap-show-doc-buffer)
    (define-key map (kbd "C-o")   #'nvp-hap-next-backend)
    (define-key map (kbd "C-r")   #'nvp-hap-search-remote)
    (define-key map (kbd "C-l")   #'nvp-hap-search-local)
    map))

;; Manage the transient map. On exit, restore prior window configuration.
(defun nvp-hap-uninstall-keymap (&optional kill-map)
  (ignore-errors (nvp-indicate-cursor-post))
  (nvp-hap--electric-restore-windows)
  (x-hide-tip)
  (and kill-map (setq overriding-terminal-local-map nil)))

(defun nvp-hap-install-keymap ()
  (nvp-indicate-cursor-pre)
  (set-transient-map
   nvp-hap-keymap t #'nvp-hap-uninstall-keymap (not nvp-hap-verbose)))


;; -------------------------------------------------------------------
;;; Help Buffer

;; cache doc-buffer to jump from popup
(defvar-local nvp-hap--doc-buffer nil)
(defvar-local nvp-hap--thingatpt nil)

(defun nvp-hap-doc-buffer (&optional string)
  (with-current-buffer (get-buffer-create "*hap-documentation*")
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (and string (save-excursion (insert string)))
      (current-buffer))))

(defun nvp-hap-get-doc-buffer (&optional prefix)
  (or (and nvp-hap--doc-buffer (buffer-live-p (car nvp-hap--doc-buffer))
           nvp-hap--doc-buffer)
      (and nvp-hap--thingatpt
           (setq nvp-hap--doc-buffer
                 (nvp-hap-call-backend 'doc-buffer nvp-hap--thingatpt prefix)))))

(defun nvp-hap-pop-to-buffer ()
  (interactive)
  (-when-let (buff (nvp-hap-get-doc-buffer))
    (setq nvp-hap--saved-window-configuration nil)
    (x-hide-tip)
    (pop-to-buffer (car buff))
    (unless (eq (cadr buff) :set)
      (goto-char (or (cadr buff) (point-min))))))

(defun nvp-hap-show-doc-buffer (&optional prefix)
  (interactive)
  (setq nvp-hap--saved-window-configuration nil)
  (let (other-window-scroll-buffer)
    (nvp-hap--electric-do
      (-when-let (buff (nvp-hap-get-doc-buffer prefix))
        ;; don't show an empty help buffer
        (unless (with-current-buffer (car buff)
                  (zerop (buffer-size)))
          (setq other-window-scroll-buffer (get-buffer (car buff)))
          (let ((win (display-buffer (car buff) t)))
            (set-window-start
             win (if (eq (cadr buff) :set)
                     (window-point win)
                   (or (cadr buff) (point-min))))))))))

(defun nvp-hap-next-backend ()
  (interactive)
  (let ((nvp-help-at-point-functions
         (or (cdr (member nvp-hap-backend nvp-help-at-point-functions))
             nvp-help-at-point-functions)))
    (funcall-interactively #'nvp-help-at-point)))

;; -------------------------------------------------------------------
;;; Search external

(defun nvp-hap-search-remote ()
  (interactive)
  (--if-let (nvp-hap-call-backend 'search-remote nvp-hap--thingatpt)
    (pcase it
      ((pred stringp) (browse-url it))
      ((pred processp) nil)
      (_ (user-error "unhandled '%S'" it)))
    (user-error "unimplemented")))

(defun nvp-hap-search-local ()
  (interactive)
  (or (nvp-hap-call-backend 'search-local nvp-hap--thingatpt)
      (user-error "unimplemented")))

;; -------------------------------------------------------------------
;;; Popup
;; popup looks for (1) doc-string then (2) doc-buffer

(defsubst nvp-hap--skip-footers ()
  (beginning-of-line)
  (while (and (not (bobp))
              (looking-at-p "\\[back\\]\\|\\[source\\]\\|^\\s-*$"))
    (forward-line -1))
  (end-of-line))

(defun nvp-hap--docstring-from-buffer (beg &optional end)
  (goto-char beg)
  (save-restriction
    (and end (narrow-to-region beg end))
    (let* ((end (progn
                  (forward-line nvp-hap-popup-max-lines)
                  (pos-eol)))
           (truncated (> (point-max) end)))
      (nvp-hap--skip-footers)
      (unless (eq beg (point))
        (if truncated
            (concat (buffer-substring beg (point)) "\n\n[...]")
          (buffer-substring beg (point)))
        ;; (with-temp-buffer
        ;;   (insert str)
        ;;   (fill-region-as-paragraph (point-min) (point-max))
        ;;   (buffer-substring (point-min) (point-max)))
        ))))

(defun nvp-hap--docstring (thing &optional prefix)
  (-if-let (doc (nvp-hap-call-backend 'doc-string thing prefix))
      (with-temp-buffer
        (insert doc)
        (nvp-hap--docstring-from-buffer (point-min)))
    (-when-let (buf (nvp-hap-get-doc-buffer prefix))
      (with-current-buffer (car buf)
        (nvp-hap--docstring-from-buffer
         (or (cadr buf) (point-min)) (caddr buf))))))

(defun nvp-hap-show-popup (&optional prefix)
  (when nvp-hap--thingatpt
    (-when-let (doc (nvp-hap--docstring nvp-hap--thingatpt prefix))
      (let ((x-gtk-use-system-tooltips nil))
        (unless (x-hide-tip)
          (pos-tip-show-no-propertize
           doc nil nil nil nvp-hap-popup-timeout
           (pos-tip-tooltip-width (window-width) (frame-char-width))))))))


;; -------------------------------------------------------------------
;;; Backends

(defvar-local nvp-hap-company-backend 'company-capf)
(defvar-local nvp-hap-syntax-table nil
  "When non-nil, syntax table to use when determining thing at point.")
(defvar-local nvp-hap--disabled-backends nil)
(defvar-local nvp-hap--treesit-p nil)

(defconst nvp-hap--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-info-face)
    (4 . (:foreground "#61ACBB" :weight bold))));; ,compilation-info-face

(defun nvp-hap--propertize (type str)
  (propertize str 'face (alist-get type nvp-hap--message-type-face)))

(defun nvp-hap-message (type msg &rest args)
  (when nvp-hap-verbose
    (let ((backend (cond ((null nvp-hap-backend))
                         ((symbolp nvp-hap-backend)
                          (symbol-name nvp-hap-backend))
                         ((plistp nvp-hap-backend)
                          (symbol-name (plist-get nvp-hap-backend :backend)))
                         (t (format "%S" nvp-hap-backend)))))
      (apply #'message
             (concat (nvp-hap--propertize type (or backend "[hap]")) ": " msg)
             args))))

(defun nvp-hap-call-backend (&rest args)
  (apply (if (functionp nvp-hap-backend) nvp-hap-backend
           (plist-get nvp-hap-backend :backend))
         args))

(defun nvp-hap-cancel ()
  (setq nvp-hap-backend nil
        nvp-hap--thingatpt nil
        nvp-hap--doc-buffer nil)
  (nvp-hap-uninstall-keymap 'kill-map))

;; same as `company-init-backend' pretty much
(defun nvp-hap--init-backend (backend)
  (and (symbolp backend)
       (not (fboundp backend))
       (ignore-errors (require backend nil t)))
  (cond
   ((symbolp backend)
    (condition-case err
        (progn (funcall backend 'init)
               (put backend 'hap-init t))
      (error
       (put backend 'hap-init 'failed)
       (nvp-hap-message 1 "Hap backend '%s' failed to initialize:\n%s"
                        backend (error-message-string err))
       (push backend nvp-hap--disabled-backends)
       nil)))
   ((functionp backend) t)
   (t
    (cl-dolist (b backend)
      (unless (keywordp b)
        (nvp-hap-init-backend b))))))

(defun nvp-hap-init-backend (backend)
  (unless (member backend nvp-hap--disabled-backends)
    (if (plistp backend)
        (and (nvp-hap-init-backend (plist-get backend :backend))
             (nvp-hap-treesit-local-active-p backend))
      (or (and (symbolp backend)
               (eq t (get backend 'hap-init)))
          (unless (get backend 'hap-init)
            (nvp-hap--init-backend backend))))))

;; passes prefix argument to `nvp-hap-show-*' functions in case
;; prompt was forced to find symbol other than the one at point
(defun nvp-hap--begin (&optional prefix)
  (let (sym)
    (cl-dolist (backend (if nvp-hap-backend (list nvp-hap-backend)
                          nvp-help-at-point-functions))
      (condition-case-unless-debug err
          (when (and
                 (nvp-hap-init-backend backend)
                 (setq sym (let ((nvp-hap-backend backend))
                             (if nvp-hap-syntax-table
                                 (with-syntax-table nvp-hap-syntax-table
                                   (nvp-hap-call-backend 'thingatpt prefix))
                               (nvp-hap-call-backend 'thingatpt prefix)))))
            (setq nvp-hap-backend backend
                  nvp-hap--thingatpt sym
                  nvp-hap--doc-buffer nil)
            (nvp-hap-message 4 "found %S" sym)
            (when (or (nvp-hap-show-popup prefix)
                      (nvp-hap-show-doc-buffer prefix))
              (cl-return sym)))
        (error (let ((nvp-hap-backend backend))
                 (nvp-hap-message 1 "%s" (error-message-string err)))
               (setq nvp-hap-backend nil
                     nvp-hap--thingatpt nil))))))

(defsubst nvp-hap--backend-sym (sym)
  (if (plistp sym) (plist-get sym :backend) sym))

(defsubst nvp-hap--treesit-maybe-init (&optional backends)
  (and (fboundp 'nvp-hap-treesit-init)
       (nvp-hap-treesit-init backends)))

(defun nvp-hap-choose-backend ()
  "Prompt for backend to use."
  (--when-let (completing-read "Backend: "
                (--map (replace-regexp-in-string
                        "nvp-hap-\\(.+\\)" "\\1"
                        (symbol-name (nvp-hap--backend-sym it)))
                       nvp-help-at-point-functions))
    (intern (concat "nvp-hap-" it))))

;;;###autoload
(defun nvp-help-at-point (&optional prefix)
  "Show help for thing at point in a popup tooltip or help buffer."
  (interactive "P")
  (nvp-hap-cancel)
  (nvp-hap--treesit-maybe-init)
  (let ((nvp-help-at-point-functions
         (cond ((eq 64 (prefix-numeric-value prefix))
                (list (nvp-hap-choose-backend)))
               (t nvp-help-at-point-functions))))
    (condition-case-unless-debug err
        (if (nvp-hap--begin prefix)
            (nvp-hap-install-keymap)
          (user-error "Nothing interesting here"))
      (user-error (nvp-hap-cancel)
                  (user-error (error-message-string err)))
      (error (nvp-hap-cancel)
             (nvp-hap-message 1 "%s" (error-message-string err)))
      (quit (nvp-hap-cancel)))))

;;;###autoload
(defun nvp-hap-company (command &optional arg &rest _args)
  (-when-let (company-backend nvp-hap-company-backend)
    (cl-case command
      (thingatpt (nvp-hap-thing-at-point arg nil "Company: "))
      (doc-buffer (when (company-call-backend 'candidates arg "")
                    (nvp-with-no-window
                      (when-let* ((buf (company-call-backend 'doc-buffer arg)))
                        (sit-for 0.1)
                        (list buf))))))))
;; (when (eq company-backend 'company-cmake)
;;   ;; cmake needs to construct help arguments for candidate
;;   ;; prior to call to cmake
;;   (company-call-backend 'candidates arg))
;; (list (company-call-backend 'doc-buffer arg))

;;;###autoload
(defun nvp-hap-info (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (-when-let (mode (info-lookup-select-mode))
                 (ignore-errors
                   (save-excursion
                     (skip-syntax-forward "w_")
                     (info-lookup-guess-default 'symbol mode)))))
    (doc-buffer (save-window-excursion
                  (nvp-with-no-window :actions (same-frame)
                    (let ((info-lookup-other-window-flag nil))
                      (info-lookup-symbol arg))
                    (list (current-buffer) (pos-bol) nil))))))

;;;###autoload
(defun nvp-hap-elisp (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (let ((sym (symbol-at-point)))
                 (and (or (fboundp sym)
                          (boundp sym))
                      sym)))
    (doc-buffer (with-current-buffer (help-buffer)
                  (cl-letf (((symbol-function #'message)
                             (symbol-function #'ignore)))
                    (nvp-with-no-window
                      (if (fboundp arg)
                          (describe-function arg)
                        (describe-variable arg))
                      (list (current-buffer) (point-min) nil)))))))

;;;###autoload
(defun nvp-hap-local (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (help-at-pt-kbd-string))
    (doc-string arg)
    (doc-buffer)))

(provide 'nvp-hap)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap.el ends here
