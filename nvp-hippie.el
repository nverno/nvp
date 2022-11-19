;;; nvp-hippie.el --- hippie expansion functions  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains hippie-expanders that are generally useful in any
;; mode, as well as some tools to parse files and create completion tables.
;;
;; Flex expansion converts buffer symbols to regexps that can be fuzzily matched
;; against for hippie expansion. So, with the defaults for elisp, for expample,
;; hippie prefixes are broken by [-:.] and converted to regexps matching the
;; first letter of each component, so 'r-r' or 'r.r' or 'r:r' are all converted
;; to "\\br\\w*-r[[:alnum:]:-]*\\b", allowing 'r-r' to expand to
;; `replace-regexp-in-string' or `read-regexp', for example.
;; 
;; Candidates are chosen in the buffer by the `nvp-he-weight-function', and only
;; those with a minimum length of `nvp-he-min-length' are considered.
;; Additionally, the search will timeout after `nvp-he-time-limit'. Conversion
;; from hippie prefixes, eg. 'r-r', to regexps are customizable via
;; `nvp-he-flex-prefix-from-re' and `nvp-he-flex-prefix-to-re' and a matcher function,
;; `nvp-he-flex-matcher' that does the actually conversion from a prefix to a
;; regexp. `nvp-he-flex-symbol-beg' is used to find the beginning of candidates.
;;
;; The flex completion table is only recomputed when the hippie expansion
;; prefix changes.
;;
;; Default flex configs for some language types:
;; - *Lisp-like*:
;;  + case insensitive
;;  + expansion candidates match prefixes of hyphen separated symbols
;;  + prefixes can be separated by [.-:], eg. r.r == r-R == R:r, all of which
;;  + would be converted to match "\\br\\w*-r[[:alnum:]:-]*\\b"
;;  + symbol beginnings are found by `he-lisp-symbol-beg'
;;  + regexps created by `nvp-he-flex-lisp'
;;
;; - *camelCased.method(), or snake_cased.chained() types*:
;;  + case sensitive
;;  + hippie expansion prefixes treat PREFIX-RE chars as being attached to the
;;    subsequent char, and fuzzy match between lower/upper case and intervening
;;    PREFIX-RE matches, eg. with the defaults
;;    so 't.sS' => '\\bt[[:alnum:]]*\\.s[[:alnum:]]*S[[:alnum:]._]*\\b'
;;    and look for matches in the buffer.
;;
;; Helper functions
;; - cached completion tables
;; - sort candidates by weighting function
;;
;; Hippie expanders:
;; - nvp-try-expand-flex => fuzzy matching
;; - dabbrevs, modified to use closest first
;; - tags (etags/gtags) => no tag libraries are loaded, if one is already
;;   loaded, then an expand function is substituted at the nvp-try-expand-tags
;;   location in the `hippie-expand-try-functions-list', if neither library
;;   is active, then `nvp-try-expand-tags' removes itself from the buffer-local
;;   list after first call
;; - try expand whole lines: closest first modification + using case-fold-search
;;
;;; TODO:
;;; - allow flex matching in additional scopes: frame/global/visible
;;; - make flex sorting / removal of duplicates more efficient
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'company)
(require 'hippie-exp)
(nvp:decls :f (tags-completion-table ggtags-try-complete-tag ggtags-mode))

;; *Note*: cl-remove-duplicates is currently used _after_ sorting candidates
;; which is pretty inefficient -- but also, need to remove dupes using :from-end t
;; in order to maintain the proper sorted order.
;; It might be quite a bit more efficient to hash candidates, or at least track
;; nearness to original point during search and remove duplicates prior to sorting.
;;
;; args: active position, match-beg, match-end
(defvar nvp-he-weight-function #'company-occurrence-prefer-any-closest
  "How to weight expansion candidates when sorting.")

(defvar nvp-he-min-length 3
  "Minimum candidate length in, eg. `nvp-he-buffer-matches'.")

(defvar nvp-he-time-limit 0.1
  "Max searching time before timeout in, eg. `nvp-he-buffer-matches'.")

(defvar nvp-he-case-fold t
  "Non-nil if flex matching should ignore case.")

(defvar nvp-he-window-scope 'visible
  "Limits additional scopage for expanders: can be \\='visible, \\='global, or \\='frame.")

;; function called with current string prefix to produce regexp to find candidates
(defvar nvp-he-flex-matcher #'nvp-he-flex-camel/snake)

;; function to find beginning of candidate at point
;; - lisp: slime-symbol-start-pos, slime-symbol-end-pos
;; - elisp: he-lisp-symbol-beg
(defvar nvp-he-flex-symbol-beg #'nvp-he-chained-symbol-beg)

;; Lisp config
;; (defvar nvp-he-flex-matcher #'nvp-he-flex-lisp)
;; (defvar nvp-he-flex-symbol-beg #'he-lisp-symbol-beg)

;; create regexp from STR matching expansions around hypens, eg
;; r-r => "\\br\\w*-r[A-Za-z0-9-]*\\b"
;; so it matches replace-regexp-in-string, for example
(defvar nvp-he-flex-prefix-from-re "[-:./]")
(defvar nvp-he-flex-prefix-to-re "\\\\w*-")

;; -------------------------------------------------------------------
;;; Matchers

;; default lisp matcher: convert STR (prefix before point) to regexp to match
(defun nvp-he-flex-lisp (str)
  (concat "\\b" (replace-regexp-in-string
                 nvp-he-flex-prefix-from-re nvp-he-flex-prefix-to-re str)
          "[+:A-Za-z0-9-]*\\b"))

;; Create matching function that splits prefixes by case
;; any PRE-RE are considered part of the following char, eg. if
;; PRE-RE is (default) "[._]?" and JOIN-RE is (default) "[[:alnum:]]*" then:
;; t.sS => \\bt[[:alnum:]]*\\.s[[:alnum:]]*.S[[:alnum:]]*[[:alnum:]._]*\\b
;; so it would match "this.setState"
(defun nvp-he-make-flex-camel-matcher (&optional pre-re join-re after-re)
  (nvp:defq pre-re "[-/_.]" join-re "[[:alnum:]]*" after-re "[[:alnum:]._]*\\b")
  `(lambda (s)
     (let ((case-fold-search))
       (concat
        "\\b"
        (replace-regexp-in-string
         " " ,join-re
         (replace-regexp-in-string
          ,(concat pre-re "[[:alnum:]]") ,(concat join-re "\\&")
          (replace-regexp-in-string
           "\\([[:lower:]0-9]\\)\\([[:upper:]]\\)" "\\1 \\2"
           (replace-regexp-in-string
            "\\([[:upper:]]\\)\\([[:upper:]][[:lower:]0-9]\\)" "\\1 \\2"
            s)))
         nil 'literal)
        ,after-re))))

;; Default matcher for camel/snake-cased strings:
;; can use with `nvp-he-flex-symbol-beg' set to `nvp-he-chained-symbol-beg'
;; eg. t.sS => this.setState
(defalias 'nvp-he-flex-camel/snake (nvp-he-make-flex-camel-matcher))

;; grab possible "." chained symbol
(defun nvp-he-chained-symbol-beg ()
  (save-excursion
    (while (not (or (zerop (skip-syntax-backward "w_"))
                    (zerop (skip-chars-backward ".")))))
    (point)))

;; -------------------------------------------------------------------
;;; Language setups
;; defaults should work for most languages

;;;###autoload
(defun nvp-he-flex-lisp-setup (&optional beg)
  (setq-local nvp-he-flex-matcher #'nvp-he-flex-lisp)
  (setq-local nvp-he-flex-symbol-beg (or beg #'he-lisp-symbol-beg))
  (setq-local nvp-he-flex-prefix-from-re "[-:./]")
  (setq-local nvp-he-flex-prefix-to-re "\\\\w*-"))

;; -------------------------------------------------------------------
;;; Completion Tables

(defmacro nvp-he-lazy-completion-table (var fun &optional test)
  "See `lazy-completion-table'."
  (declare (indent defun) (debug (symbolp lambda-expr)))
  (let ((str (make-symbol "string")))
    `(nvp-he-completion-table
      (lambda (,str)
        (when (functionp ,var)
          (setq ,var (funcall #',fun)))
        ,var)
      ,test)))

(defun nvp-he-completion-table (fun &optional test)
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
              (prog1 (setq last-result (funcall fun arg))
                (setq last-arg arg))))))
    new-fun))

;; -------------------------------------------------------------------
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
  (nvp:defq start-pos (point-min))
  (save-excursion
    (goto-char (point-min))
    (nvp-he:timed-while (and (not (input-pending-p))
                             (re-search-forward regexp nil t))
        start-time limit 25
      (if (and ignore-comments (save-match-data (nvp:ppss 'soc)))
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
  (let* ((case-fold-search nvp-he-case-fold)
         (res (nvp-he--search-buffer
              regexp nil (point) ignore-weights (current-time)
              nvp-he-time-limit ignore-comments)))
    ;; XXX: optimization => this is sorting prior to removing all duplicates
    ;; very inefficient, but don't want to remove highest weighted candidates
    ;; prior to sort -- should modify the regexp searching to expand away from
    ;; the initial candidate? Or hash the matches, compute the weights when
    ;; they are found and keep the best weight for duplicates
    (cl-remove-duplicates
     (if ignore-weights
         res
       (--map (car it) (sort res (lambda (e1 e2) (<= (cdr e1) (cdr e2))))))
     :test #'equal
     :from-end t)))

;;; TODO: use this function to allow flex-expansion in other visible windows
;;; as well
;; refs: `aw-window-list', `he--all-buffers', `next-window'
(defun nvp-he-interesting-buffers ()
  "Return list of interesting buffers. This respects `hippie-expand-only-buffers',
`hippie-expand-ignore-buffers' and `nvp-he-window-scope'."
  (let ((only-buffers hippie-expand-only-buffers) ; usually buffer-local
        (ignore-buffers hippie-expand-ignore-buffers))
    (mapcar
     #'window-buffer
     (cl-remove-if
      (lambda (w)
        (let ((f (window-frame w))
              (buff (window-buffer w)))
          (or (not (and (frame-live-p f)
                        (frame-visible-p f)))
              (string= "initial_terminal" (terminal-name f))
              ;; check if buffer is accepted by other hippie variables
              (progn
                (set-buffer buff)
                (if only-buffers
                    (not (he-buffer-member only-buffers))
                  (he-buffer-member ignore-buffers))))))
      (cl-ecase nvp-he-window-scope
        (visible (cl-mapcan #'window-list (visible-frame-list)))
        (global (cl-mapcan #'window-list (frame-list)))
        (frame (window-list)))))))


;; -------------------------------------------------------------------
;;; Flex

;; (add-function :before-until (local 'nvp-he-flex-completion-table) #'...)
;; only recompute `nvp-he-buffer-matches' when prefix has changed
(defvar nvp-he-flex-completion-table
  (nvp-he-completion-table
   (lambda (arg)
     (nvp-he-buffer-matches (funcall nvp-he-flex-matcher arg)))))

;;;###autoload
(defun nvp-try-expand-flex (old)
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


;; -------------------------------------------------------------------
;;; Tags

;; NON-destructively replace OLD-ELEM with NEW-ELEM in expanders list
(defsubst nvp-he--replace-in-list (old-elem new-elem)
  (setq hippie-expand-try-functions-list
        (copy-sequence hippie-expand-try-functions-list))
  (--when-let (memq old-elem hippie-expand-try-functions-list)
    (setcar it new-elem)))

;;;###autoload
(defun nvp-try-expand-tags (_old)
  "Placeholder for tag expansion function.  
If active tags are found, it replaces itself in
`hippie-expand-try-functions-list' with the active backend, otherwise it
removes itself."
  (cond
   ((bound-and-true-p ggtags-mode)
    (nvp-he--replace-in-list 'nvp-try-expand-tags 'ggtags-try-complete-tag))
   ((bound-and-true-p tags-file-name)
    (nvp-he--replace-in-list 'nvp-try-expand-tags 'nvp-try-expand-etags))
   (t (setq hippie-expand-try-functions-list
            (remove 'nvp-try-expand-tags hippie-expand-try-functions-list))))
  nil)

;; `tags-completion-table' caches a local table
(defvar-local nvp-he-etags-completion-table
  (nvp-he-completion-table
   (lambda (arg)
     (all-completions arg (tags-completion-table (current-buffer))))))

(defun nvp-try-expand-etags (old)
  "Try expansions from TAGS file."
  (unless old
    (he-init-string (or (car (find-tag-default-bounds)) (point))
                    (point))
    (setq he-expand-list
          (and (not (equal he-search-string ""))
               (funcall nvp-he-etags-completion-table he-search-string))))
  (if (null he-expand-list)
      (progn
        (and old (he-reset-string))
        nil)
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))


;; -------------------------------------------------------------------
;;; Dabbrevs
;; TODO: refactor this

;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-hippie.el
(defvar nvp-he-search-loc-forward (make-marker))
(defvar nvp-he-search-loc-backward (make-marker))

;;;###autoload
(defun nvp-try-expand-dabbrev-closest-first (old)
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
(defun nvp-try-expand-line-closest-first (old)
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


;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list 
	 '(nvp-try-expand-line-closest-first
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

;; Local Variables:
;; byte-compile-warnings: (not redefine)
;; End:
