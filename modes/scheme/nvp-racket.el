;;; nvp-racket.el --- racket -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'racket-mode nil t)
(require 'nvp-scheme)
(nvp:decls :p ("racket"))
(nvp:auto "nvp-hap" nvp-hap-thing-at-point)


(with-eval-after-load 'nvp-repl
  (require 'nvp-racket-repl))

(defun nvp-racket-expand (&optional choose)
  "Expand dwim without macro hiding.
If region is active, expand the region.
Else if prefix CHOOSE, select thing to expand.
Else if point is at the end of a list expand the previous sexp.
Otherwise expand the list containing point."
  (interactive "P")
  (let ((fn (cond
             ((region-active-p) #'racket-expand-region)
             (choose (nvp:read-char-case "Expand: " 'verbose
                       (?s "[s]sexp" #'racket-expand-last-sexp)
                       (?d "[d]ef" #'racket-expand-definition)
                       (?f "[f]ile" #'racket-expand-file)))
             ((eq ?\) (char-before)) #'racket-expand-last-sexp)
             (t nil))))
    (if fn (funcall fn 'no-hiding)
      (racket-stepper--expand-text
       t (lambda () (bounds-of-thing-at-point 'list))))))


;; -------------------------------------------------------------------
;;; Tests

(eval-when-compile (require 'compile))
(define-compilation-mode raco-test-mode "Raco"
  "Compilation mode for raco test."
  :abbrev-table nil
  (setq-local compilation-error-regexp-alist '(raco))
  (setq-local compilation-error-regexp-alist-alist
              '((raco "location:[ \t]+\\([^ \n:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                      1 2 3))))

(defun nvp-racket-test (&optional read-command)
  "Run raco test on associated test files."
  (interactive "P")
  (--when-let (nvp-project-locate-root nil 'local "info.rkt")
    (let* ((default-directory it)
           (file (f-base (buffer-file-name)))
           (compile-command (concat "racket -l raco test **/" file "*")))
      (when read-command
        (setq compile-command (compilation-read-command compile-command)))
      (compilation-start compile-command #'raco-test-mode))))


;; -------------------------------------------------------------------
;;; Help

;; XXX: when is this better than using `racket--xp-make-company-doc-buffer-proc'
(defun nvp-hap--racket-describe (thing args)
  (let ((how (if args (racket--buffer-file-name)
               (pcase (get-text-property (point) 'racket-xp-doc)
                 (`(,path ,anchor) `(,path . ,anchor))
                 (_                (racket--buffer-file-name))))))
    (save-window-excursion
      (let ((display-buffer-overriding-action
             ;; FIXME: still flashes buffer
             ;; '(display-buffer-no-window)
             '(nil . ((inhibit-switch-frame . t)))))
        (racket--do-describe how nil thing)
        (let (buf)
          (while (or (not (setq buf (get-buffer (format "*Racket Describe <%s>*"
                                                        (racket-back-end-name)))))
                     (with-current-buffer buf (zerop (buffer-size))))
            (sit-for 0.1))
          (with-current-buffer buf
            (let ((beg (or
                        ;; use `racket--describe-goto' to locate symbol doc
                        (and (consp how)
                             (cdr how)
                             (cdr (racket--describe-goto (cdr how))))
                        (when (re-search-forward thing nil t 1)
                          (goto-char (line-beginning-position -1))
                          (point))
                        (point-min)))
                  (end (save-excursion
                         (forward-line 1)
                         (when (re-search-forward "\\s-*---+" nil t 1)
                           (line-beginning-position)))))
              (list (current-buffer) beg end))))))))

;; `nvp-hap-backend' help-at-point function for Racket using `racket-mode' to
;; parse online documentation
(defun nvp-hap-racket (command &optional arg &rest args)
  (cl-case command
    (init (require 'racket-mode nil t))
    (thingatpt (nvp-hap-thing-at-point arg nil "Racket: " (racket--describe-terms)))
    (doc-buffer                         ; args has prefix
     (or (--when-let (funcall (racket--xp-make-company-doc-buffer-proc) arg)
           (list it (point-min)))
         (nvp-hap--racket-describe arg args)))
    (search-remote (format "https://docs.racket-lang.org/search/index.html?q=%s"
                           (url-hexify-string arg)))
    (search-local (racket--search-doc-locally arg))))


;; -------------------------------------------------------------------
;;; Abbrevs

(defun nvp-racket-abbrev-string (str &rest args)
  "Convert STR to abbrev."
  ;; "->" => "t" in abbrevs
  (apply #'nvp-abbrev-from-splitters (replace-regexp-in-string "->" "-t-" str) args))

(cl-defmethod nvp-parse-functions ((_mode (eql racket-mode)) &rest args)
  "Return list of functions defined in buffer."
  (require 'nvp-parse)
  (when-let ((defs (ignore-errors (cl-call-next-method nil args))))
    (--filter
     (not (string-match-p
           (rx (or
                (seq "." (+ digit) eos) ; some-fun some-fun.1 some-fun.2 etc.
                ":"                     ; XXX: do anything with struct:a-struct?
                ))
           it))
     defs)))

(cl-defmethod nvp-abbrevd-make-args ((mode (eql racket-mode)) &rest _)
  "Arguments for `nvp-abbrevd--make-abbrevs'."
  (list :objects (seq-uniq (nvp-parse-functions mode))
        :transformer #'nvp-racket-abbrev-string))

(cl-defmethod nvp-abbrevd-table-props
  ((_mode (eql racket-mode)) &rest args &key type &allow-other-keys)
  "Abbrev table props."
  (let ((props (apply #'cl-call-next-method nil :type type args)))
    (plist-put props :enable-function (if (eq 'variables type)
                                          #'nvp-scheme-abbrev-var-expand-p
                                        #'nvp-scheme-abbrev-fun-expand-p))))

(provide 'nvp-racket)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-racket.el ends here
