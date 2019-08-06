;;; nvp-hap.el --- help-at-point functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Help at point:
;; - display help for context around point in popup window
;; - use transient bindings to execute actions from there
;; popup.el/pos-tip.el/quickhelp.el to truncate pop-tips
;; #<marker at 84732 in etags.el.gz>
;; #<marker at 8769 in cl-generic.el.gz>
;;
;; Notes:
;; - doc-buffer: returns list (buffer start end)

;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'nvp)
(require 'pos-tip)
(require 'company)

;;; Toggle
;;;###autoload
(defun nvp-company-quickhelp-toggle ()
  "Toggle pos-tip help on/off."
  (interactive)
  (let ((x-gtk-use-system-tooltips nil))
    ;; tell `company-quickhelp--manual-begin' to start the timer
    ;; An alternative is calling `company-quickhelp--cancel-timer' instead, but
    ;; I find it buggy when toggling quickly (the popup stops working until
    ;; moving on to another candidate - I believe breaking the `while-no-input'
    ;; cycle, but not really sure)
    ;; #<marker at 8895 in company-quickhelp.el>
    (nvp-toggled-if
      (if-let ((fn (bound-and-true-p nvp-doc-toggle-fn)))
          (funcall fn 'company)
        (company-quickhelp-manual-begin))
      :this-cmd 'company-quickhelp-manual-begin
      (x-hide-tip))))


;;; Help-at-point

(defvar nvp-hap-popup-max-lines 25 "Max lines to display in popup.")
(defvar nvp-hap-popup-timeout 60)

(defvar nvp-hap-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-v")   #'scroll-other-window)
    (define-key map (kbd "M-S-v") #'scroll-other-window-down)
    (define-key map "h"           #'nvp-hap-pop-to-buffer)
    (define-key map (kbd "C-h")   #'nvp-hap-show-doc-buffer)
    map))

(defvar nvp-hap--doc-buffer nil)
(defvar nvp-hap--saved-window-configuration ())
(defvar nvp-hap--electric-commands
  '(scroll-other-window scroll-other-window-down mwheel-scroll))

(defun nvp-hap-install-keymap ()
  (nvp-indicate-cursor-pre)
  (set-transient-map
   nvp-hap-keymap t
   (lambda ()
     (nvp-indicate-cursor-post)
     (setq nvp-hap--doc-buffer nil)
     (nvp-hap--electric-restore-windows))))

(defsubst nvp-hap--call (&rest args)
  (let ((result
         (apply #'run-hook-with-args-until-success
                'nvp-help-at-point-functions args)))
    (and result (eq (car args) 'doc-buffer)
         (setq nvp-hap--doc-buffer result))
    result))

(defun nvp-hap--electric-restore-windows ()
  (when (and nvp-hap--saved-window-configuration
             (not (memq this-command nvp-hap--electric-commands)))
    (set-window-configuration nvp-hap--saved-window-configuration)
    (setq nvp-hap--saved-window-configuration nil)))

;; `company--electric-do'
(defmacro nvp-hap--electric-do (&rest body)
  (declare (indent 0) (debug t))
  `(progn
     (setq nvp-hap--saved-window-configuration (current-window-configuration))
     ,@body))

;;; Help Buffer

(defun nvp-hap-pop-to-buffer (thing)
  (interactive (list (nvp-hap--call 'thingatpt)))
  (-when-let (buff (or nvp-hap--doc-buffer
                       (nvp-hap--call 'doc-buffer thing)))
    (setq nvp-hap--saved-window-configuration nil)
    (pop-to-buffer (car buff))
    (goto-char (or (cadr buff) (point-min)))))

(defun nvp-hap-show-doc-buffer (thing)
  (interactive (list (nvp-hap--call 'thingatpt)))
  (setq nvp-hap--saved-window-configuration nil)
  (let (other-window-scroll-buffer)
    (nvp-hap--electric-do
      (-when-let (buff (or nvp-hap--doc-buffer
                           (nvp-hap--call 'doc-buffer thing)))
        (setq other-window-scroll-buffer (get-buffer (car buff)))
        (let ((win (display-buffer (car buff) t)))
          (set-window-start win (or (cadr buff) (point-min))))))))


;;; Popup

(defsubst nvp-hap--skip-footers ()
  (beginning-of-line)
  (while (and (not (bobp))
              (looking-at-p "\\[back\\]\\|\\[source\\]\\|^\\s-*$"))
    (forward-line -1)))

(defun nvp-hap--docstring-from-buffer (beg &optional end)
  (goto-char beg)
  (and end (narrow-to-region beg end))
  (let* ((end (progn
                (forward-line nvp-hap-popup-max-lines)
                (point-at-eol)))
         (truncated (> (point-max) end)))
    (nvp-hap--skip-footers)
    (if truncated
        (concat (buffer-substring-no-properties beg (point)) "\n\n[...]")
      (buffer-substring-no-properties beg (point)))))

(defun nvp-hap--docstring (thing)
  (-if-let (doc (nvp-hap--call 'doc-string thing))
      (with-temp-buffer
        (insert doc)
        (nvp-hap--docstring-from-buffer (point-min)))
    (-when-let (buff (or nvp-hap--doc-buffer
                         (nvp-hap--call 'doc-buffer thing)))
      (with-current-buffer (car buff)
        (nvp-hap--docstring-from-buffer
         (or (cadr buff) (point-min)) (caddr buff))))))

(defun nvp-hap-show-popup (thing)
  (interactive (list (nvp-hap--call 'thingatpt)))
  (-when-let (doc (nvp-hap--docstring thing))
    (let ((x-gtk-use-system-tooltips nil))
      (unless (x-hide-tip)
        (pos-tip-show doc nil nil nil nvp-hap-popup-timeout)))))


;;; Backends

;;;###autoload
(defun nvp-hap-info (command &optional arg)
  (cl-case command
    (thingatpt
     (-when-let (mode (info-lookup-select-mode))
       (info-lookup-guess-default 'symbol mode)))
    (doc-buffer
     (save-window-excursion
       (let ((display-buffer-overriding-action
              '(nil . ((inhibit-switch-frame . t))))
             (info-lookup-other-window-flag nil))
         (ignore-errors
           (info-lookup-symbol arg)
           (list (current-buffer) (point) nil)))))))

;;;###autoload
(defun nvp-hap-elisp (command &optional arg)
  (cl-case command
    (thingatpt (symbol-at-point))
    ;; (doc-string (if (fboundp arg) (documentation arg)
    ;;               (documentation-property arg 'variable-documentation)))
    (doc-buffer
     (save-window-excursion
       (let ((display-buffer-overriding-action
              '(nil . ((inhibit-switch-frame . t)))))
         (when (cond
                ((fboundp arg) (describe-function arg))
                ((boundp arg) (describe-variable arg))
                (t nil))
           (list (help-buffer) (point-min) nil)))))))

;;;###autoload
(defun nvp-help-at-point (thing)
  (interactive (list (nvp-hap--call 'thingatpt)))
  (nvp-hap-install-keymap)
  (or (nvp-hap-show-popup thing)
      (nvp-hap-show-doc-buffer thing)))

(provide 'nvp-hap)
;;; nvp-hap.el ends here
