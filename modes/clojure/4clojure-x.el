;;; 4clojure-x.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require '4clojure nil t)
(require 'request)
(nvp:decls)
(autoload 'nvp-netrc "nvp-conf")

(defconst 4clojure-x-save-file (expand-file-name "4clojure-place.txt" nvp/data)
  "Store current 4clojure problem.")

(defvar 4clojure-x-logged-in nil)

(defvar 4clojure-x-creds (nvp-netrc "4clojure.com"))

;; -------------------------------------------------------------------
;;; Bindings

(nvp:bindings "clojure-mode" nil
  ("<f2> 4 o" . 4clojure-open-question)
  ("<f2> 4 n" . 4clojure-next-question)
  ("<f2> 4 p" . 4clojure-previous-question)
  ("<f2> 4 c" . 4clojure-check-answers)
  ("<f2> 4 l" . 4clojure-x-login))

;; ------------------------------------------------------------
;;; Util

;;; Manage cached data
(eval-when-compile
  ;; Load cached problem data.
  (defmacro 4clojure-x-load-cached (file)
    `(let ((str (with-temp-buffer
                  (insert-file-contents ,file)
                  (buffer-string))))
       (car (split-string str "\n" t))))

  ;; Get last 4clojure problem number.
  (defmacro 4clojure-x-last-project (file)
    `(if (file-exists-p ,file)
         (car ,4clojure-x-save-file)
       "1"))

  ;; Store current 4clojure problem number.
  (defmacro 4clojure-x-store-problem-number (num)
    `(with-temp-file ,4clojure-x-save-file
       (insert (int-to-string ,num)))))

;; -------------------------------------------------------------------
;;; Commands

;; Log in to 4clojure and pick up on previous problem.
;;;###autoload
(defun 4clojure-x-start-session ()
  (interactive)
  (or 4clojure-x-logged-in
      (progn
        (4clojure-x-login)
        (setq 4clojure-x-logged-in t)))
  (4clojure-open-question
   (4clojure-x-load-cached 4clojure-x-save-file)))

;;@@FIXME:
;; Log in to http://www.4clojure.com.
(defun 4clojure-x-login (&optional creds)
  (interactive)
  (let ((creds (or creds 4clojure-x-creds)))
    (message "%s"
             (request
              "http://www.4clojure.com/login?location=%%2F"
              :type "POST"
              :data `(("user" . ,(car creds))
                      ("pwd" . ,(cadr creds)))))))

;; Nice stuff for improving interface, jacked from:
;; https://github.com/howardabrams/dot-files/blob/master/emacs-clojure.org
;; Check the answer and show the next question if it worked.
(defun 4clojure-x-check-and-proceed ()
  (interactive)
  (unless
      (save-excursion
        ;; Find last sexp (the answer).
        (goto-char (point-max))
        (forward-sexp -1)
        ;; Check the answer.
        (cl-letf ((answer
                   (buffer-substring (point) (point-max)))
                  ;; Preserve buffer contents, in case you failed.
                  ((buffer-string)))
          (goto-char (point-min))
          (while (search-forward "__" nil t)
            (replace-match answer))
          (string-match "failed." (4clojure-check-answers))))
    (4clojure-next-question)))

;; ------------------------------------------------------------
;;; Advices

(defadvice 4clojure-open-question (around 4clojure-open-question-around)
  "Start a cider/nREPL connection if one hasn't already been started when 
opening a 4clojure question."
  ad-do-it
  (unless cider-current-clojure-buffer
    (cider-jack-in)))

(defadvice 4clojure/start-new-problem
    (after 4clojure-x/start-new-problem-advice () activate)
  ;; Prettify the 4clojure buffer.
  (goto-char (point-min))
  (forward-line 2)
  (forward-char 3)
  (fill-paragraph)
  ;; Position point for the answer
  (goto-char (point-max))
  (insert "\n\n\n")
  (forward-char -1)
  ;; Define our key.
  (local-set-key (kbd "M-j") #'4clojure-x-check-and-proceed))

;; Store project number with loading 4clojure projects
(defadvice 4clojure-next-question (after 4clojure-next-question)
  "Save the place for each question you progress to."
  (4clojure-x-store-problem-number
   (4clojure/problem-number-of-current-buffer)))

(defadvice 4clojure-open-question (after 4clojure-next-question)
  (4clojure-x-store-problem-number
   (4clojure/problem-number-of-current-buffer)))

(ad-activate '4clojure-next-question)
(ad-activate '4clojure-open-question)

(provide '4clojure-x)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; 4clojure-x.el ends here
