;;; nvp-fixme.el --- simple fixme minor-mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Minor mode to highlight and navigate b/w fixmes and todos
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'compile)

(nvp:auto "nvp-rg" nvp-rg-todos)

(defvar nvp-fixme-danger-keywords '("FIXME" "BUG" "ERROR"))
(defvar nvp-fixme-warning-keywords '("HACK" "WARNING" "WARN" "FIX"))
(defvar nvp-fixme-todo-keywords '("TODO" "WIP"))
(defvar nvp-fixme-note-keywords '("NOTE" "XXX" "INFO" "DOCS" "PERF" "TEST"))

(defconst nvp-fixme-font-lock-keywords
  (cl-flet ((reg-it (kws face)
              (let ((re (rx-to-string
                         `(seq (group-n 1 bow (regexp ,(nvp:rx-or kws)) eow)
                               (? (group-n 3 "(")
                                  (group-n 5 (* (not (or ")" "\n"))))
                                  (group-n 4 ")"))
                               (* white)
                               (group-n 2 ":")))))
                (list re `(1 ',face t)
                      '(2 'font-lock-delimiter-face t)
                      '(3 'font-lock-bracket-face t t)
                      '(4 'font-lock-bracket-face t t)
                      '(5 'font-lock-constant-face t t)))))
    `((,@(reg-it nvp-fixme-danger-keywords compilation-error-face))
      (,@(reg-it nvp-fixme-warning-keywords compilation-warning-face))
      (,@(reg-it nvp-fixme-todo-keywords compilation-warning-face))
      (,@(reg-it nvp-fixme-note-keywords compilation-info-face)))))

(defvar nvp-fixme-re
  (rx-to-string
   `(seq bow (or ,@nvp-fixme-danger-keywords ,@nvp-fixme-note-keywords
                 ,@nvp-fixme-todo-keywords ,@nvp-fixme-warning-keywords)
         eow (? "(" (* (not (or ")" "\n"))) ")") (* white) ":")))

;; collect occurences of fixme keywords in buffer
(nvp:define-cache nvp-fixme-collect-occurences ()
  :local t
  :predicate (not (buffer-modified-p))
  (save-excursion
    (goto-char (point-min))
    (let (case-fold-search res)
      (while (re-search-forward nvp-fixme-re nil 'move)
        ;; only store locations inside of comments
        (and (nth 4 (syntax-ppss))
             (push (line-number-at-pos) res)))
      res)))

;; -------------------------------------------------------------------
;;; Commands 

(defun nvp-fixme-search (&optional back)
  (let ((case-fold-search t)
        (message-log-max nil)
        (search-fn (if back 're-search-backward 're-search-forward))
        (beg (point)))
    (condition-case nil
        (progn (when (looking-at-p nvp-fixme-re)
                 (forward-line (and back -1)))
               (funcall search-fn nvp-fixme-re)
               (goto-char (match-beginning 0)))
      (error (message "No more fixmes") (goto-char beg)))))

(defun nvp-fixme-next ()
  (interactive)
  (nvp-fixme-search))

(defun nvp-fixme-previous ()
  (interactive)
  (nvp-fixme-search 'back))

(defun nvp-fixme-occur ()
  (interactive)
  (occur nvp-fixme-re))

;; -------------------------------------------------------------------
;;; Mode 

(defvar-keymap nvp-fixme-mode-map
  :doc "Fixme keymap"
  "M-s-n" #'nvp-fixme-next
  "M-s-p" #'nvp-fixme-previous
  "M-s-o" #'nvp-fixme-occur
  "M-s-l" #'nvp-rg-todos)

(defvar-keymap nvp-repeat-fixme-map
  :repeat t
  "n" #'nvp-fixme-next
  "p" #'nvp-fixme-previous)

(easy-menu-define nvp-fixme-mode-menu nvp-fixme-mode-map
  "Fixme menu."
  '("Fixme"
    ["Next" nvp-fixme-next t]
    ["Previous" nvp-fixme-previous t]
    ["Occur" nvp-fixme-occur t]
    ["Project" nvp-rg-todos t]
    "---"
    ["Turn off" nvp-fixme-mode t]))

;;;###autoload
(define-minor-mode nvp-fixme-mode "Fixme"
  :lighter " 🛠️"
  :keymap nvp-fixme-mode-map
  (if nvp-fixme-mode
      (font-lock-add-keywords nil nvp-fixme-font-lock-keywords)
    (font-lock-remove-keywords nil nvp-fixme-font-lock-keywords))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'nvp-fixme)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-fixme.el ends here
