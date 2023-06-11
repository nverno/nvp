;;; nvp-hap.el --- help-at-point functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TODO: call same backend on subsequent callbacks
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
;; TODO: man
;;
;; Refs:
;; popup.el/pos-tip.el/quickhelp.el to truncate pop-tips
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'pos-tip)
(require 'company)
(require 'company-quickhelp)
(nvp:decls)
(nvp:auto "info-look" 'info-lookup-select-mode 'info-lookup-guess-default)
(defvar info-lookup-other-window-flag)

;; local override function to get doc for quickhelp-toggle
(defvar nvp-quickhelp-toggle-function #'company-quickhelp-manual-begin)

;;; Quickhelp

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
    (nvp:toggled-if (funcall nvp-quickhelp-toggle-function)
      ;; :this-cmd 'nvp-sh-quickhelp-toggle
      :this-cmd 'company-quickhelp-manual-begin
      (x-hide-tip))))


;;; Help-at-point

;; see `company--group-lighter' for idea to display current backend in mode-line
(defvar nvp-hap-popup-max-lines 25 "Max lines to display in popup.")
(defvar nvp-hap-popup-timeout 60)

;; cache doc-buffer to jump from popup
(defvar nvp-hap--doc-buffer nil)
(defvar nvp-hap--saved-window-configuration ())
(defvar nvp-hap--electric-commands
  '(scroll-other-window scroll-other-window-down mwheel-scroll))

;; get first result from `nvp-help-at-point-functions'
(defsubst nvp-hap--call (&rest args)
  (let ((result (apply #'run-hook-with-args-until-success
                       'nvp-help-at-point-functions args)))
    (and result (eq (car args) 'doc-buffer)
         (setq nvp-hap--doc-buffer result))
    result))

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
    map))

;; Manage the transient map. On exit, reset the cached doc-buffer and
;; restore prior window configuration.
(defun nvp-hap-uninstall-keymap (&optional kill-map)
  (nvp-indicate-cursor-post)
  (setq nvp-hap--doc-buffer nil)
  (nvp-hap--electric-restore-windows)
  (x-hide-tip)
  (and kill-map (setq overriding-terminal-local-map nil)))

(defun nvp-hap-install-keymap ()
  (nvp-indicate-cursor-pre)
  (set-transient-map nvp-hap-keymap t #'nvp-hap-uninstall-keymap))


;; -------------------------------------------------------------------
;;; Help Buffer

(defun nvp-hap-doc-buffer (&optional string)
  (with-current-buffer (get-buffer-create "*hap-documentation*")
    (erase-buffer)
    (when string
      (save-excursion (insert string)))
    (current-buffer)))

(defun nvp-hap-pop-to-buffer (thing &optional arg)
  (interactive (list (nvp-hap--call 'thingatpt current-prefix-arg)))
  (-when-let (buff (or nvp-hap--doc-buffer
                       (nvp-hap--call 'doc-buffer thing arg)))
    (setq nvp-hap--saved-window-configuration nil)
    (x-hide-tip)
    (pop-to-buffer (car buff))
    (unless (eq (cadr buff) :set)
      (goto-char (or (cadr buff) (point-min))))))

(defun nvp-hap-show-doc-buffer (thing &optional arg)
  (interactive (list (nvp-hap--call 'thingatpt current-prefix-arg)))
  (setq nvp-hap--saved-window-configuration nil)
  (let (other-window-scroll-buffer)
    (nvp-hap--electric-do
      (-when-let (buff (or nvp-hap--doc-buffer
                           (nvp-hap--call 'doc-buffer thing arg)))
        (setq other-window-scroll-buffer (get-buffer (car buff)))
        (let ((win (display-buffer (car buff) t)))
          (set-window-start
           win (if (eq (cadr buff) :set)
                   (window-point win)
                 (or (cadr buff) (point-min)))))))))

;; -------------------------------------------------------------------
;;; Popup 
;; popup looks for (1) doc-string then (2) doc-buffer

;; From pos-tip.el
;; (1/25/23) `propertize' messes up popup for some reason on emacs v30 build
;;;###autoload
(defun nvp-pos-tip-show
  (string &optional tip-color pos window timeout width frame-coordinates dx dy)
  "Show STRING in a tooltip, which is a small X window, at POS in WINDOW
using frame's default font with TIP-COLOR.

Return pixel position of tooltip relative to top left corner of frame as
a cons cell like (X . Y).

TIP-COLOR is a face or a cons cell like (FOREGROUND-COLOR . BACKGROUND-COLOR)
used to specify *only* foreground-color and background-color of tooltip. If
omitted, use `pos-tip-foreground-color' and `pos-tip-background-color' or the
foreground and background color of the `tooltip' face instead.

Omitting POS and WINDOW means use current position and selected window,
respectively.

Automatically hide the tooltip after TIMEOUT seconds. Omitting TIMEOUT means
use the default timeout of 5 seconds. Non-positive TIMEOUT means don't hide
tooltip automatically.

WIDTH, if non-nil, specifies the width of filling each paragraph.

If FRAME-COORDINATES is omitted or nil, automatically obtain the absolute
coordinates of the top left corner of frame which WINDOW is on. Here,
`top left corner of frame' represents the origin of `window-pixel-edges'
and its coordinates are essential for calculating the absolute coordinates
of the tooltip. If a cons cell like (LEFT . TOP), specifies the frame
absolute location and makes the calculation slightly faster, but can be
used only when it's clear that frame is in the specified position. Users
can get the latest values of frame coordinates for using in the next call
by referring the variable `pos-tip-saved-frame-coordinates' just after
calling this function. Otherwise, FRAME-COORDINATES `relative' means use
the pixel coordinates relative to the top left corner of the frame for
displaying the tooltip. This is the same effect as
`pos-tip-use-relative-coordinates' is non-nil.

DX specifies horizontal offset in pixel.

DY specifies vertical offset in pixel. This makes the calculations done
without considering the height of object at POS, so the object might be
hidden by the tooltip.

See also `pos-tip-show-no-propertize'."
  (unless window
    (setq window (selected-window)))
  (let* ((frame (window-frame window))
	 (max-width (pos-tip-x-display-width frame))
	 (max-height (pos-tip-x-display-height frame))
	 (w-h (pos-tip-string-width-height string))
         ;; (fg (pos-tip-compute-foreground-color tip-color))
         ;; (bg (pos-tip-compute-background-color tip-color))
         ;; (frame-font (find-font (font-spec :name (frame-parameter frame 'font))))
         ;; (tip-face-attrs (list :font frame-font :foreground fg :background bg))
         )
    (cond
     ((and width
	   (> (car w-h) width))
      (setq string (pos-tip-fill-string string width nil 'none nil max-height)
	    w-h (pos-tip-string-width-height string)))
     ((or (> (car w-h) max-width)
	  (> (cdr w-h) max-height))
      (setq string (pos-tip-truncate-string string max-width max-height)
	    w-h (pos-tip-string-width-height string))))
    (pos-tip-show-no-propertize
     string
     ;; (propertize string 'face tip-face-attrs)
     tip-color pos window timeout
     (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
     (pos-tip-tooltip-height (cdr w-h) (frame-char-height frame) frame)
     frame-coordinates dx dy)))

(defsubst nvp-hap--skip-footers ()
  (beginning-of-line)
  (while (and (not (bobp))
              (looking-at-p "\\[back\\]\\|\\[source\\]\\|^\\s-*$"))
    (forward-line -1))
  (end-of-line))

(defun nvp-hap--docstring-from-buffer (beg &optional end)
  (goto-char beg)
  (and end (narrow-to-region beg end))
  (let* ((end (progn
                (forward-line nvp-hap-popup-max-lines)
                (pos-eol)))
         (truncated (> (point-max) end)))
    (nvp-hap--skip-footers)
    (unless (eq beg (point))
      (if truncated
          (concat (buffer-substring-no-properties beg (point)) "\n\n[...]")
        (buffer-substring-no-properties beg (point))))))

(defun nvp-hap--docstring (thing &optional arg)
  (-if-let (doc (nvp-hap--call 'doc-string thing arg))
      (with-temp-buffer
        (insert doc)
        (nvp-hap--docstring-from-buffer (point-min)))
    (-when-let (buff (or nvp-hap--doc-buffer
                         (nvp-hap--call 'doc-buffer thing arg)))
      (with-current-buffer (car buff)
        (nvp-hap--docstring-from-buffer
         (or (cadr buff) (point-min)) (caddr buff))))))

(defun nvp-hap-show-popup (thing &optional arg)
  (interactive
   (list (nvp-hap--call 'thingatpt current-prefix-arg) current-prefix-arg))
  (-when-let (doc (nvp-hap--docstring thing arg))
    (let ((x-gtk-use-system-tooltips nil))
      (unless (x-hide-tip)
        (nvp-pos-tip-show doc nil nil nil nvp-hap-popup-timeout)))))


;; -------------------------------------------------------------------
;;; Backends

(defvar-local nvp-hap-company-backend nil)

;;;###autoload
(defun nvp-hap-company (command &optional arg &rest _args)
  (-when-let (company-backend nvp-hap-company-backend)
    (cl-case command
      (thingatpt (symbol-at-point))
      (doc-buffer
       (unless (stringp arg) (setq arg (symbol-name arg)))
       (when (eq company-backend 'company-cmake)
         ;; cmake needs to construct help arguments for candidate prior to call
         ;; to cmake
         (company-call-backend 'candidates arg))
       (list (company-call-backend 'doc-buffer arg))))))

;;;###autoload
(defun nvp-hap-info (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (-when-let (mode (info-lookup-select-mode))
                 (ignore-errors (info-lookup-guess-default 'symbol mode))))
    (doc-buffer
     (save-window-excursion
       (let ((display-buffer-overriding-action '(nil . ((inhibit-switch-frame . t))))
             (info-lookup-other-window-flag nil))
         (ignore-errors
           (info-lookup-symbol arg)
           (list (current-buffer) (pos-bol) nil)))))))

;;;###autoload
(defun nvp-hap-elisp (command &optional arg &rest _args)
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
(defun nvp-help-at-point (thing &optional prefix)
  "Show help for THING at point in a popup tooltip or help buffer."
  (interactive
   (list (nvp-hap--call 'thingatpt current-prefix-arg) current-prefix-arg))
  (unless thing
    (user-error "Nothing found at point"))
  (nvp-hap-install-keymap)
  (or (nvp-hap-show-popup thing prefix)
      (nvp-hap-show-doc-buffer thing prefix)
      (progn
        (nvp-hap-uninstall-keymap 'kill-map)
        (user-error "No help found for %s" thing))))

(provide 'nvp-hap)
;;; nvp-hap.el ends here
