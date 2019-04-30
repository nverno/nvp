;;; nvp-hippie.el --- general hippie expanders -*- lexical-binding: t; -*-

;;; Commentary:
;; Base hippie expansion functions
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'hippie-exp)

;;;###autoload
(defun nvp-he-local (&rest he-funcs)
  (unless (local-variable-p 'hippie-expand-try-functions-list)
    (make-local-variable 'hippie-expand-try-functions-list))
  (dolist (func (reverse he-funcs))
    (cl-pushnew func hippie-expand-try-functions-list)))

;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-hippie.el
(defvar nvp-he-search-loc-backward (make-marker))
(defvar nvp-he-search-loc-forward (make-marker))

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
