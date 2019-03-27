;;; nvp-macs.el --- coding related macros -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-27 15:26:08>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 27 March 2019

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'subr-x)

;; cc-defs `c-point'
(defmacro nvp-point (position &optional point escape)
  "Return relative POSITION to POINT (default current point).
ESCAPE can be a string to match escaped newlines (default '\\').
POSITION can be one of the following symbols:

`bol'   -- beginning of line
`eol'   -- end of line
`eoll'  -- end of logical line (i.e. without escaped NL)
`bod'   -- beginning of defun
`eod'   -- end of defun
`boi'   -- beginning of indentation
`ionl'  -- indentation of next line
`iopl'  -- indentation of previous line
`bonl'  -- beginning of next line
`eonl'  -- end of next line
`bopl'  -- beginning of previous line
`eopl'  -- end of previous line
`bosws' -- beginning of syntactic whitespace
`eosws' -- end of syntactic whitespace

If the referenced position doesn't exist, the closest accessible point
to it is returned.  This function does not modify the point or the mark."
  (or escape (setq escape "\\\\"))

  (if (eq (car-safe position) 'quote)
      (let ((position (eval position)))
	(cond

	 ((eq position 'bol)
	  (if (not point)
	      '(line-beginning-position)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (beginning-of-line)
	       (point))))

	 ((eq position 'eol)
	  (if (not point)
	      '(line-end-position)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (end-of-line)
	       (point))))

	 ((eq position 'eoll)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (while (progn
		      (end-of-line)
		      (prog1 (eq (logand 1 (skip-chars-backward escape)) 1)))
	       (beginning-of-line 2))
	     (end-of-line)
	     (point)))

	 ((eq position 'boi)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (back-to-indentation)
	     (point)))

	 ((eq position 'bod)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
             (beginning-of-defun)
             (and defun-prompt-regexp
                  (looking-at defun-prompt-regexp)
                  (goto-char (match-end 0)))
	     (point)))

	 ((eq position 'eod)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
             (end-of-defun)
	     (point)))

	 ((eq position 'bopl)
	  (if (not point)
	      '(line-beginning-position 0)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line -1)
	       (point))))

	 ((eq position 'bonl)
	  (if (not point)
	      '(line-beginning-position 2)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line 1)
	       (point))))

	 ((eq position 'eopl)
	  (if (not point)
	      '(line-end-position 0)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (beginning-of-line)
	       (or (bobp) (backward-char))
	       (point))))

	 ((eq position 'eonl)
	  (if (not point)
	      '(line-end-position 2)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line 1)
	       (end-of-line)
	       (point))))

	 ((eq position 'iopl)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (forward-line -1)
	     (back-to-indentation)
	     (point)))

	 ((eq position 'ionl)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (forward-line 1)
	     (back-to-indentation)
	     (point)))

         ;; FIXME:
	 ((eq position 'bosws)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-backward-syntactic-ws)
	     (point)))

	 ((eq position 'eosws)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-forward-syntactic-ws)
	     (point)))

	 (t (error "Unknown buffer position requested: %s" position))))))

;; paredit splicing reindent doesn't account for prompts
(defmacro nvp-preserving-column (&rest body)
  "Preserve point in column after executing BODY.
`paredit-preserving-column' doesn't properly account for minibuffer prompts."
  (declare (indent defun) (debug body))
  (let ((orig-indent (make-symbol "indentation"))
        (orig-col (make-symbol "column")))
    `(let ((,orig-col (if (eq major-mode 'minibuffer-inactive-mode)
                          (- (current-column) (minibuffer-prompt-width))
                        (current-column)))
           (,orig-indent (current-indentation)))
       (unwind-protect
           (progn ,@body)
         (let ((ci (current-indentation)))
           (goto-char
            (+ (point-at-bol)
               (cond ((not (< ,orig-col ,orig-indent))
                      (+ ,orig-col (- ci ,orig-indent)))
                     ((<= ci ,orig-col) ci)
                     (t ,orig-col)))))))))

;; -------------------------------------------------------------------
;;; Save/Restore envs 

(defmacro nvp-with-preserved-vars (vars &rest body)
  "Let bind VARS then execute BODY, so VARS maintain their original values.
VARS should be either a symbol or list or symbols."
  (declare (indent 1) (debug (form body)))
  `(cl-progv ,(if (listp vars) `,vars `(list ',vars)) nil
     (unwind-protect
         ,@body)))

;; from yasnippet #<marker at 126339 in yasnippet.el>
(defmacro nvp-letenv (env &rest body)
  "Evaluate BODY with bindings from ENV.
ENV is a lisp expression evaluating to list of (VAR FORM), where
VAR is a symbol and FORM is evaluated."
  (declare (indent 1) (debug (form body)))
  (let ((envvar (make-symbol "envvar")))
    `(let ((,envvar ,env))
       (cl-progv
           (mapcar #'car ,envvar)
           (mapcar (lambda (k-v) (eval (cadr k-v))) ,envvar)
         ,@body))))

;; -------------------------------------------------------------------
;;; Folding

;;-- Hideshow
(defmacro nvp-hs-blocks (start end)
  `(progn
     (setq hs-block-start-regexp ,start)
     (setq hs-block-end-regexp ,end)))

(defmacro nvp-with-hs-block (start end &rest body)
  "Do hideshow with START and END regexps."
  (declare (indent defun))
  `(progn
     (require 'hideshow)
     (unless hs-minor-mode (hs-minor-mode))
     (let ((hs-block-start-regexp ,start)
           (hs-block-end-regexp ,end))
       ,@(or body (list '(hs-toggle-hiding))))))


;; -------------------------------------------------------------------
;;; Tim

(provide 'nvp-macs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs.el ends here
