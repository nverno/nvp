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
(defvar nvp-he-weight-function #'company-occurrence-prefer-any-closest
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

;;; Completion Tables

(defmacro nvp-he-lazy-completion-table (var fun &optional test)
  "See `lazy-completion-table'."
  (declare (debug (symbolp lambda-expr)))
  (let ((str (make-symbol "string")))
    `(nvp-he-completion-table
      (lambda (,str)
        (when (functionp ,var)
          (setq ,var (funcall #',fun)))
        ,var)
      ,test)))

(defun nvp-he-completion-table (fun &optional test no-switch)
  "Like `completion-table-with-cache' for hippie expansions.
Wrap FUN to only recompute the candidates when arg not equal to last prefix.
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
                (prog1 (setq last-result (funcall fun arg))
                  (setq last-arg arg)))))))
    new-fun))


;;; Regexp Matches

(eval-when-compile
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
                    (throw 'done 'nvp-he-timeout)))))))))

;; collect matches for REGEXP in current buffer into RESULT
;; When IGNORE-WEIGHTS is non-nil, NVP-HE-WEIGHT-FUNCTION is used to weight
;; matches by their position and START-POS
;; START-TIME and LIMIT, when non-nil, specify max time before timeout
;; non-nil IGNORE-COMMENTS ignores matches in strings/comments
(defun nvp-he--search-buffer (regexp &optional result start-pos ignore-weights
                                     start-time limit ignore-comments)
  (or start-pos (setq start-pos (point-min)))
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
                                   start-pos
                                   (match-beginning 0)
                                   (match-end 0)))
                    result)))))))
  result)

(defun nvp-he-buffer-matches (regexp &optional ignore-weights ignore-comments)
  (let ((res (nvp-he--search-buffer
              regexp nil (point) ignore-weights (current-time)
              nvp-he-time-limit ignore-comments)))
    (cl-remove-duplicates
     (if ignore-weights
         res
       (--map (car it) (sort res (lambda (e1 e2) (<= (cdr e1) (cdr e2))))))
     :test #'equal)))


;;; Flex

;; function called with current string prefix to produce regexp to find candidates
(defvar nvp-he-flex-matcher #'nvp-he-flex-lisp)

;; function to find beginning of candidate at point
;; - lisp: slime-symbol-start-pos, slime-symbol-end-pos
;; - elisp: he-lisp-symbol-beg
(defvar nvp-he-flex-symbol-beg #'he-lisp-symbol-beg)

;; create regexp from STR matching expansions around hypens, eg
;; r-r => "\\br\\w*-r[A-Za-z0-9-]*\\b"
;; so it matches replace-regexp-in-string, for example
(defun nvp-he-flex-lisp (str)
  (concat
   "\\b" (replace-regexp-in-string "[-:]" "\\\\w*-" str) "[:A-Za-z0-9-]*\\b"))

;; only recompute `nvp-he-buffer-matches' when prefix has changed
(defvar nvp-he-flex-completion-table
  (nvp-he-completion-table
   (lambda (arg) (nvp-he-buffer-matches (funcall nvp-he-flex-matcher arg)))))

;;;###autoload
(defun nvp-he-try-expand-flex (old)
  "Try to complete symbols from current buffer using fuzzy matching.
Fuzzy matches are created by applying `nvp-he-flex-matcher' to prefix."
  (unless old
    (he-init-string (funcall nvp-he-flex-symbol-beg) (point))
    (unless (he-string-member he-search-string he-tried-table)
      (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list                ;build expansion list
          (and (not (equal "" he-search-string))
               (funcall nvp-he-flex-completion-table he-search-string))))
  (while (and he-expand-list            ;remove candidates already found
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (prog1 (not (null he-expand-list))    ;return t if expansion is found
    (if he-expand-list (he-substitute-string (pop he-expand-list))
      (and old (he-reset-string)))))


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
