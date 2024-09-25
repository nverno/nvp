;;; nvp-repl-eval.el --- Eval code in Repl -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; References:
;; - racket-show :: #<marker at 4057 in racket-show.el>
;; - inf-ruby :: #<marker at 19542 in inf-ruby.el>
;; - shell :: `shell-eval-command' #<marker at 64025 in shell.el.gz>
;;
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-repl))
(require 'comint)
(nvp:decls)

(defcustom nvp-repl-eval-result :overlay
  "Method to display eval result."
  :type '(choice (const :overlay :tag "Show results in overlay")
                 (const :message :tag "Show results in message"))
  :group 'nvp-repl)

(defface nvp-repl-eval-overlay-face
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "yellow"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "black")))
  "Face used to display evaluation results at the end of line."
  :group 'nvp-repl)

(defun nvp-repl-eval-insert (res)
  (let ((standard-output (current-buffer)))
    (princ res)))

;;;###autoload
(defun nvp-repl-show-result (&optional res insert)
  "Print the result of the last evaluation in the current buffer."
  ;; (message "nvp-repl-show-result: %S %S" res insert)
  (unless res (setq res (nvp-repl-eval-result-value)))
  (if insert
      (nvp-repl-eval-insert res)
    (nvp-repl-eval-show-result res)))

;;; TODO(08/05/24): `nvp-repl-eval-string' and `nvp-repl-eval-sexp' no longer
;;  necessary
;;;###autoload
(defun nvp-repl-eval-string (str &optional insert)
  "Eval STR, displaying result in overlay or INSERTing with prefix."
  (interactive "sEval: \nP")
  (nvp-with-repl (eval-string)
    (let ((res (if eval-string
                   (funcall eval-string str insert)
                 (prog1 nil (nvp-repl-send-string str)))))
      (nvp-repl-show-result res insert))))

;;;###autoload
(defun nvp-repl-eval-sexp (&optional insert)
  "Eval sexp before point.
With prefix, INSERT result at point."
  (interactive "P")
  (nvp-with-repl (eval-sexp)
    (let (res)
      (if eval-sexp
          (setq res (funcall-interactively eval-sexp insert))
       (save-excursion
         (skip-syntax-backward " ")
         (nvp-repl-send-sexp))
       (nvp-repl-show-result res insert)))))

;;;###autoload
(defun nvp-repl-eval-result-value ()
  "Get result of last eval from repl."
  (nvp-with-repl (repl-buf repl-proc eval-output-filter)
    (with-current-buffer repl-buf
      (while (not (and comint-last-prompt
                       (goto-char (car comint-last-prompt))
                       (looking-at comint-prompt-regexp)))
        (accept-process-output repl-proc))
      (let* ((end (point))
             (beg (and (re-search-backward comint-prompt-regexp)
                       (match-end 0)))
             (res (string-trim (buffer-substring-no-properties beg end))))
        (while (string-match comint-prompt-regexp res)
          (setq res (replace-match "" t t res)))
        (comint-goto-process-mark)
        (if eval-output-filter
            (funcall eval-output-filter res)
          res)
        ;; (or (re-search-forward "[ \n]=> " (car comint-last-prompt) t)
        ;;     ;; Evaluation seems to have failed.
        ;;     ;; Try to extract the error string.
        ;;     (let* ((inhibit-field-text-motion t)
        ;;            (s (buffer-substring-no-properties (point) (line-end-position))))
        ;;       (while (string-match comint-prompt-regexp s)
        ;;         (setq s (replace-match "" t t s)))
        ;;       (error "%s" s)))
        ;; (if (looking-at " *$")
        ;;     (progn
        ;;       (goto-char (1+ (match-end 0)))
        ;;       (replace-regexp-in-string
        ;;        "\n +" " "
        ;;        (buffer-substring-no-properties
        ;;         (point)
        ;;         (progn
        ;;           (forward-sexp)
        ;;           (point)))))
        ;;   (buffer-substring-no-properties (point) (line-end-position)))
        ))))

;;;###autoload
(defun nvp-repl-eval-show-result (res)
  "Show RES according to `nvp-repl-eval-result'."
  (pcase nvp-repl-eval-result
    (':overlay (nvp-repl--eval-overlay res))
    (_ (message "=> %s" res))))

(defun nvp-repl--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.
TYPE is a symbol put on the overlay\\='s category property.  It is
used to easily remove all overlays from a region with:
    (remove-overlays start end \\='category TYPE)
PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'category type)
    (overlay-put o 'nvp-repl-temporary t)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'nvp-repl--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun nvp-repl--delete-overlay (ov &rest _)
  "Safely delete overlay OV.
Never throws errors, and can be used in an overlay's
modification-hooks."
  (ignore-errors (delete-overlay ov)))

(defun nvp-repl--make-result-overlay (value where duration &rest props)
  "Place an overlay displaying VALUE at the end of line.
VALUE is used as the overlay's after-string property, meaning it
is displayed at the end of the overlay.  The overlay itself is
placed from beginning to end of current line.
Return nil if the overlay was not placed or if it might not be
visible, and return the overlay otherwise.
Return the overlay if it was placed successfully, and nil if it
failed.
All arguments beyond these (PROPS) are properties to be used on
the overlay."
  (let ((format " => %s ")
	(prepend-face 'nvp-repl-eval-overlay-face)
	(type 'result))
    (while (keywordp (car props))
      (setq props (cddr props)))
    ;; If the marker points to a dead buffer, don't do anything.
    (let ((buffer (cond
                   ((markerp where) (marker-buffer where))
                   ((markerp (car-safe where)) (marker-buffer (car where)))
                   (t (current-buffer)))))
      (with-current-buffer buffer
	(save-excursion
          (when (number-or-marker-p where)
            (goto-char where))
          ;; Make sure the overlay is actually at the end of the sexp.
          (skip-chars-backward "\r\n[:blank:]")
          (let* ((beg (if (consp where)
                          (car where)
			(save-excursion
                          (backward-sexp 1)
                          (point))))
		 (end (if (consp where)
                          (cdr where)
			(line-end-position)))
		 (display-string (format format value))
		 (o nil))
            (remove-overlays beg end 'category type)
            (funcall #'put-text-property
                     0 (length display-string)
                     'face prepend-face
                     display-string)
            ;; ;; If the display spans multiple lines or is very long, display it at
            ;; ;; the beginning of the next line.
            ;; (when (or (string-match "\n." display-string)
            ;;           (> (string-width display-string)
            ;;              (- (window-width) (current-column))))
            ;;   (setq display-string (concat " \n" display-string)))
            ;; Put the cursor property only once we're done manipulating the
            ;; string, since we want it to be at the first char.
            (put-text-property 0 1 'cursor 0 display-string)
            (when (> (string-width display-string) (* 3 (window-width)))
              (setq display-string
                    (concat (substring display-string 0 (* 3 (window-width)))
                            "...\nResult truncated.")))
            ;; Create the result overlay.
            (setq o (apply #'nvp-repl--make-overlay
                           beg end type
                           'after-string display-string
                           props))
            (pcase duration
              ((pred numberp) (run-at-time duration nil #'nvp-repl--delete-overlay o))
              (`command (if this-command
                            (add-hook 'pre-command-hook
                                      #'nvp-repl--remove-result-overlay
                                      nil 'local)
                          (nvp-repl--remove-result-overlay))))
            (let ((win (get-buffer-window buffer)))
              ;; Left edge is visible.
              (when (and win
			 (<= (window-start win) (point))
			 ;; In 24.3 `<=' is still a binary predicate.
			 (<= (point) (window-end win))
			 ;; Right edge is visible. This is a little conservative
			 ;; if the overlay contains line breaks.
			 (or (< (+ (current-column) (string-width value))
				(window-width win))
                             (not truncate-lines)))
		o))))))))

(defun nvp-repl--remove-result-overlay ()
  "Remove result overlay from current buffer.
This function also removes itself from `pre-command-hook'."
  (remove-hook 'pre-command-hook #'nvp-repl--remove-result-overlay 'local)
  (remove-overlays nil nil 'category 'result))

(defun nvp-repl--eval-overlay (value)
  "Make overlay for VALUE at POINT."
  (nvp-repl--make-result-overlay value (point) 'command)
  value)

(provide 'nvp-repl-eval)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl-eval.el ends here
