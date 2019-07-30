;;; nvp-hippie.el --- general hippie expanders -*- lexical-binding: t; -*-

;;; Commentary:
;; Base hippie expansion functions
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'company)
(require 'hippie-exp)

;; args: active position, match-beg, match-end
(defvar nvp-he-weight-function company-occurrence-weight-function
  "How to weight expansion candidates when sorting.")

(defvar nvp-he-min-length 3
  "Minimum candidate length in, eg. `nvp-he-buffer-matches'.")

(defvar nvp-he-time-limit 0.1
  "Max searching time before timeout in, eg. `nvp-he-buffer-matches'.")

;;;###autoload
(defun nvp-he-local (&rest he-funcs)
  (unless (local-variable-p 'hippie-expand-try-functions-list)
    (make-local-variable 'hippie-expand-try-functions-list))
  (cl-remove-duplicates (append he-funcs hippie-expand-try-functions-list)))

(defmacro nvp-he:lazy-completion-table (var fun &optional test)
  "See `lazy-completion-table'."
  (declare (debug (symbolp lambda-expr)))
  (let ((str (make-symbol "string")))
    `(nvp-he-completion-table
      (lambda (,str)
        (when (functionp ,var)
          (setq ,var (funcall #',fun)))
        ,var)
      ,test)))

;; modified `company-dabbrev--time-limit-while'
(defmacro nvp-he:timed-while (test &optional start-time limit freq &rest body)
  "If START-TIME is non-nil do BODY while TEST is non-nil and running time
doesn't exceed LIMIT."
  (declare (indent 4) (debug t))
  (if (null start-time)
      `(while ,test
         ,@body)
    (let ((counter (make-symbol "counter")))
      `(let ((,counter 0))
         (catch 'done
           (while ,test
             ,@body
             (and ,limit
                  (= (cl-incf ,counter) ,freq)
                  (setq ,counter 0)
                  (> (float-time (time-since ,start-time)) ,limit)
                  (throw 'done 'nvp-he-timeout))))))))

(defun nvp-he-completion-table (fun &optional test no-switch)
  "Like `completion-table-with-cache' for hippie expansions.
Wrap FUN to only recompute the candidates it's not equal to last prefix.
TEST is applied to last candidate and current to determine equality.
See `completion-table-dynamic' for NO-SWITCH."
  (unless test (setq test #'string=))
  (let* (last-arg
         last-result
         (new-fun
          (lambda (arg)
            (if (and last-arg (funcall test last-arg arg))
                last-result
              (with-current-buffer (if no-switch (current-buffer)
                                     (let ((win (minibuffer-selected-window)))
                                       (if (window-live-p win) (window-buffer win)
                                         (current-buffer))))
                (prog1
                    (setq last-result (funcall fun arg))
                  (setq last-arg arg)))))))
    new-fun))


;;; Regexp Matches

(defun nvp-he--search-buffer (regexp &optional result pos ignore-weights
                                     start-time limit ignore-comments)
  (or pos (setq pos (point-min)))
  (save-excursion
    (goto-char (point-min))
    (nvp-he:timed-while (and (not (input-pending-p))
                             (re-search-forward regexp nil t))
        start-time limit 25
      (if (and ignore-comments (save-match-data (nvp-ppss 'soc)))
          (re-search-forward "\\s>\\|\\s!\\|\\s\"" nil t)
        (let ((match (match-string-no-properties 0)))
          (when (>= (length match) nvp-he-min-length)
            (if ignore-weights
                (push match result)  
              (push (cons match
                          (funcall nvp-he-weight-function
                                   pos
                                   (match-beginning 0)
                                   (match-end 0)))
                    result)))))))
  result)

;;; TODO
(defun nvp-he-buffer-matches (regexp &optional ignore-weights ignore-comments)
  (let ((res (nvp-he--search-buffer
              regexp nil (point) ignore-weights (current-time)
              nvp-he-time-limit ignore-comments)))
    (if ignore-weights
        res
      res
      (mapcan #'car (sort res (lambda (e1 e2) (<= (cdr e1) (cdr e2)))))
      )))


;;; Dabbrevs
;; FIXME: refactor this

;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-hippie.el
(defvar nvp-he-search-loc-forward (make-marker))
(defvar nvp-he-search-loc-backward (make-marker))

;;;###autoload
(defun nvp-he-try-expand-dabbrev-closest-first (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let (expansion)
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (set-marker nvp-he-search-loc-backward he-string-beg)
      (set-marker nvp-he-search-loc-forward he-string-end))
    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))
            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)
              ;; search backward
              (goto-char nvp-he-search-loc-backward)
              (setq expansion (he-dabbrev-search he-search-string t))
              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))
              ;; search forward
              (goto-char nvp-he-search-loc-forward)
              (setq expansion (he-dabbrev-search he-search-string nil))
              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))
              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance)
                                 :forward :backward))
                            (forward-point :forward)
                            (backward-point :backward)))
              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker nvp-he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker nvp-he-search-loc-backward backward-point))))))
    (if (not expansion)
        (progn
          (if old (he-reset-string))
          nil)
      (progn
        (he-substitute-string expansion t)
        t))))

;;;###autoload
(defun nvp-he-try-expand-line-closest-first (old)
  "Try to complete the current line to an entire line in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ())
        (strip-prompt (and (get-buffer-process (current-buffer))
                           comint-use-prompt-regexp
                           comint-prompt-regexp)))
    (unless old
      (he-init-string (he-line-beg strip-prompt) (point))
      (set-marker nvp-he-search-loc-backward he-string-beg)
      (set-marker nvp-he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char nvp-he-search-loc-backward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char nvp-he-search-loc-forward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance)
                                 :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker nvp-he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker nvp-he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

;;;###autoload
(defun nvp-hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list 
	 '(nvp-he-try-expand-line-closest-first
	   try-expand-line-all-buffers)))
    (end-of-line)
    (hippie-expand nil)))

;;;###autoload
(defun nvp-hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand nil)))

(provide 'nvp-hippie)
;;; nvp-hippie.el ends here
