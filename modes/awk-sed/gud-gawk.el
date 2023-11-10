;;; gud-gawk.el --- GUD support for gawk -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; TODO: properly set `gud-last-frame' and output from
;; `gud-gawkdb-marker-regexp'
;;
;;; Code:

(eval-when-compile (require 'nvp-macro))
(nvp:decls)
(require 'gud)

(defcustom gud-gawkdb-command-name "gawk -D -f %f"
  "Command that executes the Gawk debugger."
  :type 'string
  :group 'gud)

(defvar gud-gawkdb-history nil)

;; Last group is for return value, e.g. "> test.py(2)foo()->None"
;; Either file or function name may be omitted: "> <string>(0)?()"
;;
;; We use [:graph:] to be very allowing with regards to which
;; characters we match in the file name shown in the prompt.
;; (Of course, this matches the "<string>" case too.)
(defvar gud-gawkdb-marker-regexp
  (concat "^> \\([[:graph:] \\]*\\)(\\([0-9]+\\))\\([a-zA-Z0-9_]*\\|\\?\\|"
          "<\\(?:module\\|listcomp\\|dictcomp\\|setcomp\\|genexpr\\|lambda\\|\\)>"
          "\\)()\\(->[^\n\r]*\\)?[\n\r]"))

(defvar gud-gawkdb-marker-regexp-file-group 1)
(defvar gud-gawkdb-marker-regexp-line-group 2)
(defvar gud-gawkdb-marker-regexp-fnname-group 3)

(defvar gud-gawkdb-marker-regexp-start "^> ")

(defvar gud-gawkdb-repeat-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,key . ,cmd) '(("n" . gud-next)
                                    ("s" . gud-step)
                                    ("c" . gud-cont)
                                    ("l" . gud-refresh)
                                    ("f" . gud-finish)
                                    ("<" . gud-up)
                                    (">" . gud-down)))
      (define-key map key cmd))
    map)
  "Keymap to repeat `gawkdb' stepping instructions \\`C-x C-a C-n n n'.
Used in `repeat-mode'.")

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-gawkdb-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match "^\\([0-9]+\\).*" gud-marker-acc)
      (setq gud-last-frame
            (cons gud-target-name (string-to-number (match-string 1)))

            output (concat output (substring gud-marker-acc 0 (match-end 0)))
            gud-marker-acc (substring gud-marker-acc (match-end 0)))
       ;; Extract the frame position from the marker.
            ;; gud-last-frame
       ;; (let ((file (match-string gud-gawkdb-marker-regexp-file-group
       ;;  			 gud-marker-acc))
       ;;       (line (string-to-number
       ;;  	    (match-string gud-gawkdb-marker-regexp-line-group
       ;;  			  gud-marker-acc))))
       ;;   (if (string-equal file "<string>")
       ;;       gud-last-frame
       ;;     (cons file line)))

       ;; Output everything instead of the below
      ;; output (concat output (substring gud-marker-acc 0 (match-end 0)))
       ;;	  ;; Append any text before the marker to the output we're going
       ;;	  ;; to return - we don't include the marker in this text.
       ;;	  output (concat output
       ;;		      (substring gud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.

      ;; gud-marker-acc (substring gud-marker-acc (match-end 0)))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
      ;; (if (string-match gud-gawkdb-marker-regexp-start gud-marker-acc)
      ;;   (progn
      ;;     ;; Everything before the potential marker start can be output.
      ;;     (setq output (concat output (substring gud-marker-acc
      ;;   					 0 (match-beginning 0))))

      ;;     ;; Everything after, we save, to combine with later input.
      ;;     (setq gud-marker-acc
      ;;   	(substring gud-marker-acc (match-beginning 0))))

      ;; (setq output (concat output gud-marker-acc)
      ;;       gud-marker-acc ""))

    output)))

;; Each entry must define the following at startup:
;;
;;<name>
;; comint-prompt-regexp
;; gud-<name>-massage-args
;; gud-<name>-marker-filter
;; gud-<name>-find-file
;;
;; The job of the massage-args method is to modify the given list of
;; debugger arguments before running the debugger.
;;
;; The job of the marker-filter method is to detect file/line markers in
;; strings and set the global gud-last-frame to indicate what display
;; action (if any) should be triggered by the marker.  Note that only
;; whatever the method *returns* is displayed in the buffer; thus, you
;; can filter the debugger's output, interpreting some and passing on
;; the rest.
;;
;;; XXX: what is `gud-tag-frame'? Not found anywhere anymo
;;
;; The job of the find-file method is to visit and return the buffer indicated
;; by the car of gud-tag-frame.  This may be a file name, a tag name, or
;; something else.

;;;###autoload
(defun gawkdb (command-line)
  "Run COMMAND-LINE in the `*gud-FILE*' buffer to debug Awk programs.

COMMAND-LINE should include the gawk executable
name (`gud-gawkdb-command-name') and the file to be debugged.

If called interactively, the command line will be prompted for.

The directory containing this file becomes the initial working
directory and source-file directory for your debugger."
  (interactive
   (list (gud-query-cmdline 'gawkdb)))

  (gud-common-init command-line nil 'gud-gawkdb-marker-filter)
  (setq-local gud-minor-mode 'gawkdb)

  (gud-def gud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "clear %f:%l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "step"         "\C-s" "Step one source line with display.")
  (gud-def gud-next   "next"         "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "continue"     "\C-r" "Continue with display.")
  (gud-def gud-finish "return"       "\C-f" "Finish executing current function.")
  (gud-def gud-up     "up"           "<" "Up one stack frame.")
  (gud-def gud-down   "down"         ">" "Down one stack frame.")
  (gud-def gud-print  "p %e"         "\C-p" "Evaluate Python expression at point.")
  (gud-def gud-statement "!%e"      "\C-e" "Execute Python statement at point.")
  
  (gud-set-repeat-map-property 'gud-gawkdb-repeat-map)

  (setq comint-prompt-regexp "^gawk> *")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'gawkdb-mode-hook))

(provide 'gud-gawk)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; gud-gawk.el ends here
