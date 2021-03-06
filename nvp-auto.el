;;; nvp-auto.el --- some autoloads -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Random autoloads
;; - some movement functions
;; - hydras: yank/pop, goto-line
;; - some other randoms
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls :f (auth-source-search))
(nvp-auto "calendar" calendar-read-date calendar-current-date calendar-date-string)
(nvp-auto "nvp-outline" nvp-outline-hydra/body)

;; -------------------------------------------------------------------
;;; Movement

;; jump to next char on this line. if matching char,
;; pressing the same key again jumps to the next one, etc.
;;;###autoload
(defun nvp-move-char-this-line (&optional char)
  (interactive (list (char-to-string (read-char "Char: " t))))
  (or (eq last-command this-command) (region-active-p) (push-mark))
  (let ((case-fold-search t))
    (condition-case nil
        (search-forward char (nvp-point 'eol))
      (error (let ((pt (point)))
               (beginning-of-line)
               (or (search-forward char (nvp-point 'eol))
                   (goto-char pt))))))
  (nvp-repeat-command nil nil nil char))

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
;;; Hydras

;;;###autoload(autoload 'nvp-hydra-page/backward-page "nvp-auto")
;;;###autoload(autoload 'nvp-hydra-page/forward-page "nvp-auto")
(defhydra nvp-hydra-page () "page"
  ("[" forward-page)
  ("]" backward-page)
  ("o" nvp-outline-hydra/body :exit t))

;;;###autoload(autoload 'nvp-hydra-goto-line/goto-line "nvp-auto")
(nvp-hydra-set-property 'nvp-hydra-goto-line)
(defhydra nvp-hydra-goto-line (goto-map) "line"
  ("g" goto-line "go")
  ("b" (push-mark (car mark-ring) nil 'activate) "mark to start" :bind nil)
  ("m" set-mark-command "mark" :bind nil)
  ("p" (set-mark-command 1) "pop" :bind nil)
  ("e" exchange-point-and-mark "exchange" :bind nil)
  ("q" nil "quit" :bind nil))

;; Yank / Pop
(declare-function helm-show-kill-ring "")
;;;###autoload(autoload 'nvp-hydra-yank-pop/yank "nvp-auto")
;;;###autoload(autoload 'nvp-hydra-yank-pop/yank-pop "nvp-auto")
(defhydra nvp-hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev"))
  ;; ("l" helm-show-kill-ring "list" :color blue)


;; -------------------------------------------------------------------
;;; Web
(eval-when-compile
  (defvar nvp-webjump-sites)
  (defvar webjump-sites))
(nvp-decl thing-at-point-url-at-point webjump-builtin webjump-url-fix web-mode)

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
(defun nvp-browse-webjump (&optional arg)
  "Jump to website."
  (interactive "P")
  (require 'webjump)
  (require 'nvp-vars)                   ;nvp-webjump-sites
  (let* ((completion-ignore-case t)
         (locals (or (and arg (read-from-minibuffer "URI: "))
                     (and (bound-and-true-p nvp-local-uris)
                          (y-or-n-p "Use local uris?")
                          nvp-local-uris)))
         (sites (or locals (append nvp-webjump-sites webjump-sites)))
         (item (or (and arg (cons nil locals))
                   (assoc-string
                    (nvp-completing-read "WebJump to site: "
                                         (mapcar #'car sites) nil t)
                    sites t)))
         (name (car item))
         (expr (cdr item)))
    (browse-url (webjump-url-fix
                 (cond ((not expr) "")
                       ((stringp expr) expr)
                       ((vectorp expr) (webjump-builtin expr name))
                       ((listp expr) (eval expr))
                       ((symbolp expr)
                        (if (fboundp expr)
                            (funcall expr name)
                          (error "WebJump URL function \"%s\" undefined"
                                 expr)))
                       (t (error "WebJump URL expression for \"%s\" invalid"
                                 name)))))))


;; -------------------------------------------------------------------
;;; Assorted

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
   (list (nvp-prefix 1 (calendar-read-date) :test '> (calendar-current-date))))
  (insert (calendar-date-string date)))

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

(nvp-decl count-words-region count-lines-page)
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
   (nvp-read-char-case "Char: " 'verbose
     (?c "[c]heck" "✓")
     (?i "[i]n" "∈")
     (?s "[s]mile" "☻")
     (?p "[p]rev" "└─"))))

(provide 'nvp-auto)
;;; nvp-auto.el ends here
