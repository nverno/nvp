;;; nvp-macro --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  2 November 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
(require 'cl-lib)

;;--- OS -------------------------------------------------------------

(defmacro nvp-program (name)
  `(eval-when-compile
     (if (eq system-type 'windows-nt)
         (bound-and-true-p
          ,(intern (concat "nvp-" name "-program")))
       (let ((local (expand-file-name ,name "/usr/local/bin")))
         (if (file-exists-p local) local ,name)))))

(defmacro nvp-with-w32 (&rest body)
  (declare (indent 0) (debug t))
  (when (eq system-type 'windows-nt)
    `(progn ,@body)))

(defmacro nvp-with-gnu (&rest body)
  (declare (indent 0) (debug t))
  (when (not (eq system-type 'windows-nt))
    `(progn ,@body)))

(defmacro nvp-with-gnu/w32 (gnu w32)
  (declare (indent 2) (indent 1) (debug t))
  (if (eq system-type 'windows-nt)
      `,@w32
    `,@gnu))

;;--- Config ---------------------------------------------------------

(defmacro nvp-bindings (mode &optional feature &rest bindings)
  (declare (indent defun))
  (let ((modemap (intern (concat mode "-map"))))
    `(eval-after-load ,(or feature `',(intern mode))
       '(progn
          ,@(cl-loop for (k . b) in bindings
               collect `(define-key ,modemap (kbd ,k) ',b))))))

;; FIXME: eval
(defmacro nvp-common-bindings (modes &rest bindings)
  (declare (indent defun))
  (macroexp-progn
   (cl-loop for mode in (eval modes)
      collect `(nvp-bindings ,mode ,@bindings))))

(defmacro nvp-mode (mode)
  `(expand-file-name (concat "nvp-" ,mode)
                     (bound-and-true-p nvp/mode)))

;;--- Time -----------------------------------------------------------

(defmacro nvp-file-older-than-days (file days)
  "non-nil if FILE last modification was more than DAYS ago."
  (declare (indent defun) (debug t))
  `(< (time-to-seconds
       (time-subtract (current-time)
                      (nth 5 (file-attributes ,file))))
      (* 60 60 24 ,days)))

;; Measure and return the running time of the code block.
;; https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el#L83
(defmacro nvp-measure-time (&rest body)
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

;;--- Regexp ---------------------------------------------------------

(defmacro nvp-re-opt (opts)
  `(eval-when-compile
     (concat "\\_<" (regexp-opt ,opts t) "\\_>")))

;;--- Process --------------------------------------------------------

(defmacro nvp-with-process-log (process &optional on-error &rest body)
  "Log output in log buffer, if on-error is :pop-on-error, pop to log
if process exit status isn't 0."
  (declare (indent defun))
  (let ((err (if (and (symbolp on-error)
                      (equal on-error :pop-on-error))
                 '(pop-to-buffer "*nvp-install*")
               on-error)))
    `(set-process-sentinel
      ,process
      #'(lambda (p m)
          (nvp-log "%s: %s" nil (process-name p) m)
          (if (not (zerop (process-exit-status p)))
              ,err
            ,@body)))))


;;--- Interactive Functions ------------------------------------------

;;; Newline

;; Newline and indents for PAIRS, extends comment region with
;; COMMENT-START when inside COMMENT-RE.
(cl-defmacro nvp-newline (name &optional description
                               &key pairs comment-re comment-start)
  (declare (indent defun))
  (let ((fn (if (symbolp name) name (intern name)))
        (conds
         (cons 'or
               (cl-loop
                  for (open close) in pairs
                  collect `(and (looking-back ,open
                                              (line-beginning-position))
                                (looking-at ,close)))))
        (start-re (when comment-re (car comment-re)))
        (end-re (when comment-re (cdr comment-re))))
    `(defun ,fn ()
       ,(or description "Newline dwim.")
       (interactive)
       (let (,@(when pairs `((p ,conds)))
             ,@(when comment-re '((ppss (syntax-ppss)))))
         (cond
          ,@(when comment-re
              `(((and (nth 4 ppss)
                      (save-excursion
                        (forward-line 0)
                        (looking-at-p ,start-re)))
                 (when (save-excursion
                         (end-of-line)
                         (looking-back ,end-re (line-beginning-position)))
                   (save-excursion
                     (newline-and-indent)))
                 (newline)
                 (insert ,comment-start)
                 (indent-according-to-mode))))
          (t
           (newline)
           ,@(when pairs
               '((when p
                   (save-excursion
                     (newline-and-indent)))))
           (indent-according-to-mode)))))))

;;; Compile

;; Create compile function, check for makefiles/cmake first, otherwise
;; execute BODY. Prefix argument executes PROMPT-ACTION, and its
;; result is bound to ARGS, which can be used in the body.
(cl-defmacro nvp-make-or-compile-fn
    (name 
     (&key
      (doc "Compile using make or cmake if found, otherwise execute body.")
      (make-action
       '(let ((compile-command (or args "make -k")))
          (nvp-compile-basic)))
      (cmake-action
       '(nvp-compile-cmake args))
      (default-prompt
        '(read-from-minibuffer "Compile args: "))
      (prompt-action
       `((cond
          ,@(and cmake-action
                 '((have-cmake
                    (read-from-minibuffer "CMake args: "))))
          ,@(and make-action
                 '((have-make
                    (format "make %s" (read-from-minibuffer "Make args: ")))))
          (t ,default-prompt)))))
     &rest body)
  (declare (indent defun))
  (let ((fn (if (symbolp name) name (intern name))))
    `(defun ,fn (&optional arg)
       ,doc
       (interactive "P")
       (let* (,@(and make-action
                     '((have-make
                        (memq t (mapcar #'file-exists-p
                                        '("Makefile" "makefile" "GNUMakefile"))))))
              ,@(and cmake-action
                     '((have-cmake (file-exists-p "CMakeLists.txt"))))
              (args (and arg ,@(or prompt-action
                                   '((read-from-minibuffer "Compile args: "))))))
         (cond
          ,@(and cmake-action `((have-cmake ,cmake-action)))
          ,@(and make-action `((have-make ,make-action)))
          (t ,@body))))))

;;; Align

;; Create alignment functions
(defmacro nvp-align-fn (name doc regex &optional ignore-string)
  (declare (indent defun))
  (let ((fn (intern (if (symbolp name)
                        (symbol-name name)
                      name))))
    `(progn
       ,(and (buffer-file-name)
             `(autoload ',name ,(buffer-file-name)))
       (defun ,fn (start end)
         ,doc
         (interactive "r")
         (align-regexp start end ,regex)))))

;;; Wrap

;; Create function to wrap region, inserting BEGIN at beginning,
;; AFTER at the end.
(defmacro nvp-wrap-fn (name doc begin end &optional interact-p)
  (declare (indent defun))
  (let ((fn (intern (if (symbolp name)
                        (symbol-name name)
                      name))))
    `(defun ,fn (start end)
       ,doc
       ,@(when interact-p
           '(interactive "r"))
       (save-excursion
         (goto-char (region-beginning))
         (insert ,begin))
       (goto-char (region-end))
       (insert ,end))))


;; Wrap items in list between DELIM, default wrapping with WRAP
;; Create list wrapping functions, wrapping items between DELIMS with
;; WRAP by default, prompting for wrapping string with prefix.  IGNORE
;; is regexp to ignore in list, ie commas and spaces and MATCH is regex
;; to capture items.
(cl-defmacro nvp-wrap-list-items (name
                                  &key
                                  (delims '("(" . ")"))
                                  (match "[^)(, \n\t\r]+")
                                  (ignore "[, \n\t\r]*")
                                  (wrap '("\"" . "\"")))
  (declare (debug defun)
           (indent defun))
  (let ((fn (intern (concat "nvp-list-wrap-" (symbol-name name))))
        (doc (format
              (concat "Wrap items of list in selected region between "
                      "%s...%s with items with %s..%s by default or "
                      "string ARG, prompting with prefix.")
              (car delims) (cdr delims) (car wrap) (cdr wrap)))
        (delim-re (concat ignore "\\(" match "\\)")))
    `(defun ,fn (start end &optional arg)
       ,doc
       (interactive "r\nP")
       (let* ((wrap (or (and arg
                             (car
                              (read-from-string
                               (read-from-minibuffer
                                "Wrap items with(a . b): "))))
                        ',wrap))
              (str (buffer-substring-no-properties start end))
              res)
         (delete-region start end)
         (insert
          (with-temp-buffer
            (insert str)
            (goto-char (point-min))
            (re-search-forward ,(regexp-quote (car delims)) nil t)
            (while (re-search-forward ,delim-re nil t)
              (replace-match (concat (car wrap)
                                     (match-string-no-properties 1)
                                     (cdr wrap))
                             t nil nil 1))
            (buffer-string)))))))

(provide 'nvp-macro)
;;; nvp-macro.el ends here
