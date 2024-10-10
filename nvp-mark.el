;;; nvp-mark.el --- Minor mode for quick xrefs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Minor mode to create xrefs from marker text on the fly
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'xref)
(autoload 'find-library-name "find-func")

(eval-and-compile
  (defvar nvp-mark--regex "#<marker at \\([^ \t\n]+\\) in \\([-a-zA-Z0-9./]+\\)>"))

(defvar-local nvp-mark--fontified-p nil
  "Non-nil if marks in buffer are currently fontified")

(defun nvp-mark-at-point (&optional point)
  (or point (setq point (point)))
  (and (or (derived-mode-p 'text-mode)
           (nvp:ppss 'cmt nil point))
       (get-text-property (point) 'file-place)))

(defun nvp-mark--collect-marks ()
  (let (res)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward nvp-mark--regex nil 'move)
        (push (cons (match-beginning 0) (match-end 0)) res)))
    res))

;; -------------------------------------------------------------------
;;; Xref

;; #<marker at 1681 in nvp-mark.el>
(defun nvp-mark--xref-backend ()
  (and (nvp-mark-at-point) 'nvp-mark))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'nvp-mark)))
  nil)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'nvp-mark)))
  (-when-let ((file . place) (nvp-mark-at-point))
    (let ((ident (format "%s:%s" file place)))
      (add-text-properties
       0 (length ident) `(file-place ,(cons file place)) ident)
      ident)))

(cl-defstruct (xref-nvp-mark-location
               (:constructor xref-make-nvp-mark-location (file pos)))
  "Location of mark."
  file pos)

(cl-defmethod xref-backend-definitions ((_backend (eql 'nvp-mark)) ident)
  (-when-let ((file . place) (get-text-property 0 'file-place ident))
    (let ((loc (xref-make-nvp-mark-location file place)))
      (list (xref-make (substring-no-properties ident) loc)))))

(defun nvp-mark--filename (file)
  (or (ignore-errors (find-library-name (file-name-base file)))
      (if (file-name-absolute-p file) file
        (or (--> (expand-file-name file default-directory)
                 (and (file-exists-p it) it))
            (expand-file-name file user-emacs-directory)))))

(defun nvp-mark--position (place)
  (if (numberp place) place
    (let ((pt (string-to-number place)))
      (when (and (zerop pt)
                 (progn
                   (goto-char (point-min))
                   (re-search-forward
                    (format "^(\\(?:cl-\\)?def[-a-z]+ %s" place) nil t)))
        (setq pt (point)))
      pt)))

(cl-defmethod xref-location-marker ((l xref-nvp-mark-location))
  (pcase-let (((cl-struct xref-nvp-mark-location file pos) l))
    (let* ((file (nvp-mark--filename file))
           (buf (and (file-exists-p file)
                     (find-file-noselect file))))
      (when buf
        (with-current-buffer buf
          (save-excursion
            (goto-char (nvp-mark--position pos))
            (point-marker)))))))

;; -------------------------------------------------------------------
;; Commands

;;;###autoload
(defun nvp-mark-goto-marker-at-point ()
  "Jump to the marker at point. A marker is of the form
'#<marker at [point/function-name] in [library/relative filename]>'."
  (interactive)
  (unless (bound-and-true-p nvp-mark-minor-mode)
    (nvp-mark-minor-mode))
  (if (nvp-mark-at-point)
      (let* ((xref-backend-functions '(nvp-mark--xref-backend t))
             (ident (xref-backend-identifier-at-point 'nvp-mark)))
        (xref-find-definitions-other-window ident))
    (user-error "Not at mark.")))

;;;###autoload
(defun nvp-mark-kill-point ()
  "Insert `point-marker' in kill-ring."
  (interactive)
  (kill-new (format "%s" (point-marker))))

(defun nvp-mark-next (&optional previous)
  "Move to the next nvp-mark.
If PREVIOUS is non-nil, move to the previous nvp-mark."
  (interactive)
  (let ((curr (point)))
    (beginning-of-line)
    (skip-syntax-forward " <")
    (and (looking-at-p nvp-mark--regex)
         (ignore-errors (forward-line (if previous -1 1))))
    (condition-case nil  ;if no match return point
        (progn
          (funcall
           (if previous 're-search-backward 're-search-forward) nvp-mark--regex)
          (goto-char (match-beginning 0)))
      (error (progn (message
                     (format "No %s marks" (if previous "previous" "next")))
                    (goto-char curr))))))

(defun nvp-mark-previous ()
  "Move to the previous nvp-mark."
  (interactive)
  (nvp-mark-next 'previous))

;; -------------------------------------------------------------------
;;; Font-lock

(defvar nvp-mark-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" #'xref-find-definitions)
    (define-key map (kbd "C-c C-j") #'xref-find-definitions)
    map))

(defvar nvp-mark-font-lock-keywords
  `((,nvp-mark--regex
     (0 (prog1 ()
          (let* ((place (match-string-no-properties 1))
                 (file (match-string-no-properties 2)))
            (with-silent-modifications
              (add-text-properties
               (match-beginning 0) (match-end 0)
               `(display ,(format "#%s<%s>" (file-name-base file) place)
                         printed-value ,(match-string 0)
                         keymap ,nvp-mark-keymap
                         read-only t
                         file-place ,(cons file place)))))))
     (0 'font-lock-type-face t))))

;; remove special mark display when disabling fontification
(defun nvp-mark--remove-display ()
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward nvp-mark--regex nil 'move)
          (remove-text-properties
           (match-beginning 0) (match-end 0)
           '(display keymap read-only file-place printed-value)))))))

;;;###autoload
(defun nvp-mark-toggle-fontification ()
  "Toggle fontification of nvp-marks in buffer."
  (interactive)
  (if (setq nvp-mark--fontified-p (not nvp-mark--fontified-p))
      (progn
        (nvp-mark--remove-display)
        (font-lock-refresh-defaults))
    (font-lock-add-keywords nil nvp-mark-font-lock-keywords)
    (font-lock-flush)
    (font-lock-ensure)))

(defvar nvp-mark-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "M-s-p") #'nvp-mark-previous)
    (define-key km (kbd "M-s-n") #'nvp-mark-next)
    (define-key km [mouse-1] #'xref-find-definitions)
    (easy-menu-define nil km nil
      '("NMark"
        ["Previous" nvp-mark-previous t]
        ["Next" nvp-mark-next t]))
    km)
  "Keymap used in `nvp-mark-minor-mode'.")

;;;###autoload
(define-minor-mode nvp-mark-minor-mode "NvpMark"
  :lighter " NMark"
  :keymap nvp-mark-mode-map
  (if nvp-mark-minor-mode
      (progn
        (add-hook 'xref-backend-functions #'nvp-mark--xref-backend nil t)
        (font-lock-add-keywords nil nvp-mark-font-lock-keywords))
    (nvp-mark--remove-display)
    (remove-hook 'xref-backend-functions #'nvp-mark--xref-backend t)
    (font-lock-remove-keywords nil nvp-mark-font-lock-keywords))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'nvp-mark)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-mark.el ends here
