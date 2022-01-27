;;; nvp-leet.el --- leetcode helpers -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'let-alist))
(nvp-decls :v (leetcode--description-buffer-name
               leetcode--testcase-buffer-name leetcode--result-buffer-name
               leetcode--User-Agent leetcode--url-login
               leetcode--X-CSRFToken leetcode--api-graphql
               url-http-end-of-headers)
           :f (leetcode-try
               leetcode-submit leetcode-show-problem leetcode--start-coding
               leetcode--get-code-buffer-name leetcode--referer
               leetcode--maybe-csrf-token))

;;; Test cases

(defun nvp-leet--collect-examples ()
  "Collect example inputs from problem description."
  (-when-let (buf (get-buffer leetcode--description-buffer-name))
    (let (inputs)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^Input:[^=]+= *" nil t)
            (let ((lim (point-at-eol))
                  cur)
              (condition-case nil
                  (progn
                    (while (< (point) lim)
                      (let ((beg (point))
                            (end (progn (forward-sexp) (point))))
                        (when (> end beg)
                          (push (buffer-substring beg end) cur)))
                      (re-search-forward "[^=]+= *" lim)))
                (error (progn
                         (setq cur (nreverse cur))
                         (push cur inputs)
                         (goto-char lim))))))))
      inputs)))

(defun nvp-leet-add-examples ()
  "Add all examples from description to tests."
  (interactive)
  (-when-let (inputs (nvp-leet--collect-examples))
    (with-current-buffer (get-buffer-create leetcode--testcase-buffer-name)
      (save-excursion
        (goto-char (point-max))
        (cl-loop for input in inputs
                 do (progn
                      (delete-horizontal-space)
                      (unless (bolp) (insert "\n"))
                      (insert (mapconcat 'identity input "\n"))))))))

;;; Window Configuration

(defvar-local nvp-leet-window-configuration nil)

(defun nvp-leet-reset-layout ()
  "Reset window layout to initial solving layout."
  (interactive)
  (when nvp-leet-window-configuration
    (set-window-configuration nvp-leet-window-configuration)))

(defun nvp-leet-result-layout ()
  (when nvp-leet-window-configuration
    (set-window-configuration nvp-leet-window-configuration))
  (save-current-buffer
    (with-selected-window (display-buffer leetcode--result-buffer-name)
      (enlarge-window 25))))

(nvp:advise-commands #'nvp-leet-result-layout :before '(leetcode-try leetcode-submit))

;;; Question of the Day

(defconst nvp-leet--daily-challenge-query
  (nvp:concat
   "query questionOfToday { activeDailyCodingChallengeQuestion {"
   " link question { status title titleSlug qid: questionFrontendId } } }"))

(defun nvp-leet--lookup-daily-question ()
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(,leetcode--User-Agent
            ("Content-Type" . "application/json")
            ,(leetcode--referer leetcode--url-login) 
            ,(cons leetcode--X-CSRFToken (leetcode--maybe-csrf-token))))
         (url-request-data
          (json-encode
           `((operationName . "questionOfToday")
             (query . ,nvp-leet--daily-challenge-query))))
         (result (url-retrieve-synchronously leetcode--api-graphql)))
    (with-current-buffer result
      (goto-char url-http-end-of-headers)
      (let-alist (json-read)
        .data.activeDailyCodingChallengeQuestion.question.qid))))

(defun nvp-leet-daily ()
  "Open the daily challenge."
  (interactive)
  (--when-let (nvp-leet--lookup-daily-question)
    (leetcode-show-problem (string-to-number it))))

;; -------------------------------------------------------------------
;;; Minor mode

(nvp-bindings nvp-leet-mode nil
  :create t
  :prefix-key "<f2>L"
  ("e" . nvp-leet-add-examples)
  ("a" . nvp-leet-add-examples)
  ("t" . leetcode-try)
  ("s" . leetcode-submit)
  ("r" . nvp-leet-reset-layout)
  ("d" . nvp-leet-daily))
(define-key nvp-leet-mode-map (kbd "C-c C-c") #'leetcode-try)

(defun nvp@leet-maybe-setup (orig-fn problem problem-info)
  (let ((title (plist-get problem-info :title)))
    (funcall orig-fn problem problem-info)
    (let* ((buf-name (leetcode--get-code-buffer-name title))
           (buf (and buf-name (get-buffer buf-name))))
      (when buf
        (with-current-buffer (get-buffer buf-name)
          (nvp-leet-mode 1))))))
(advice-add #'leetcode--start-coding :around #'nvp@leet-maybe-setup)

(define-minor-mode nvp-leet-mode "Leetcode minor mode."
  :lighter " LC"
  :keymap nvp-leet-mode-map
  (unless nvp-leet-window-configuration
    (setq nvp-leet-window-configuration (current-window-configuration))))

(provide 'nvp-leet)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-leet.el ends here
