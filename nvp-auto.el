;;; nvp-auto.el --- some autoloads -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Random autoloads
;; - some movement functions
;; - some other randoms
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :f (auth-source-search))
(nvp:auto "calendar" calendar-read-date calendar-current-date calendar-date-string)
(nvp:auto "nvp-read" nvp-read-mode-config)

;; -------------------------------------------------------------------
;;; Install

;;;###autoload
(defun nvp-install-mode (mode &optional reinstall)
  (interactive (list (nvp-read-mode-config) current-prefix-arg))
  (let* ((path (nvp:mode-config-path mode))
         (elc (concat path "c")))
    (when (and reinstall (file-exists-p elc))
      (delete-file elc))
    (load path)))

;;;###autoload
(defun nvp-install-modes (modes)
  "Install packages for list of MODES."
  (mapc #'nvp-install-mode modes))

;; -------------------------------------------------------------------
;;; Movement

;;;###autoload
(defun nvp-move-next-matching-char (&optional char)
  "Jump to next matching CHAR."
  (interactive (nvp:repeat-args (char)
                 (list (char-to-string (read-char "Char: " t)))))
  (nvp:push-mark 'nvp-move-next-matching-char)
  (let ((case-fold-search t))
    (condition-case nil
        (search-forward char)
      (error (let ((pt (point)))
               (goto-char (point-min))
               (or (search-forward char)
                   (goto-char pt))))))
  (nvp:repeat-this-command char))

;; see `paragraph-start' and `paragraph-separate' to extend
;;;###autoload
(defun nvp-move-forward-paragraph (&optional arg)
  (interactive "^p")
  (or arg (setq arg 1))
  (if (bolp)
      (progn
        (and (< arg 1) (forward-line -1))
        (forward-paragraph arg)
        (forward-line 1))
    (line-move arg)))

;;;###autoload
(defun nvp-move-backward-paragraph (&optional arg)
  (interactive "^p")
  (or arg (setq arg 1))
  (nvp-move-forward-paragraph (- arg)))


;; -------------------------------------------------------------------
;;; Assorted
(nvp:decl thing-at-point-url-at-point web-mode)

;;;###autoload
(defun nvp-browse-url-contents ()
  "Open a new buffer containing the contents of a URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
	 (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (xml-mode))
	  ((search-forward "<html" nil t) (web-mode)))))

;;;###autoload
(defun nvp-mark-header-region ()
  "Mark current header region."
  (interactive)
  (condition-case nil
      ;; mark successive header
      (if (use-region-p)
          (set-mark
           (save-excursion
             (goto-char (mark))
             (nvp-move-forward-heading)
             (point)))
        ;; first marking
        (forward-line 0)
        (or (looking-at (nvp-mode-header-regex))
            (nvp-move-previous-heading 'error))
        ;; headers are known at this point
        (save-excursion
         (or (ignore-errors (nvp-move-forward-heading))
             (prog1 (goto-char (point-max))
               (message "Marked to end of buffer (no more headings)")))
         (push-mark (point) nil 'activate)))
    (error (user-error "Can't find header region to mark."))))

;;;###autoload
(defun nvp-kill-emacs ()
  (interactive)
  (save-some-buffers 'no-ask)
  (kill-emacs))

;;;###autoload
(defun nvp-insert-date (date)
  "Insert DATE string, defaulting to current date.
With prefix, prompts for DATE."
  (interactive
   (list (nvp:prefix '>1 (calendar-read-date) (calendar-current-date))))
  (insert (calendar-date-string date)))

;;;###autoload
(defun nvp-comment-timestamped (date)
  "Insert comment with date. Prompt for date with prefix."
  (interactive
   (list (nvp:prefix '>1 (calendar-read-date) (calendar-current-date))))
  (call-interactively (pcase major-mode
                        (`org-mode #'org-comment-dwim)
                        (_ (if (bound-and-true-p paredit-mode)
                               #'paredit-comment-dwim
                             #'comment-dwim))))
  (insert "(" (calendar-date-string date) ")"))

;;;###autoload
(defun nvp-lookup-password (host user port)
  "Get password from `auth-sources'."
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret" user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

(nvp:decl count-words-region count-lines-page)
;;;###autoload
(defun nvp-count-lines-or-region (arg)
  (interactive "P")
  (if arg (funcall-interactively #'count-words-region (region-beginning) (region-end))
    (call-interactively #'count-lines-page)))

;;;###autoload
(defun nvp-insert-unicode ()
  "Quick insert unicode character."
  (interactive)
  (insert
   (nvp:read-char-case "Char: " 'verbose
     (?l "[l]ambda" "λ")
     (?c "[c]heck" "✓")
     (?d "[d]down" "⬎")
     (?i "[i]n" "∈")
     (?s "[s]mile" "☻")
     (?p "[p]rev" "└─"))))

(provide 'nvp-auto)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings redefine)
;; End:
;;; nvp-auto.el ends here
