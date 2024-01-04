;;; nvp-httpd.el --- Serve buffers/directories -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls :p (httpd impatient skewer))
(autoload 'imp-visit-buffer "impatient-mode")

;; -------------------------------------------------------------------
;;; Serve buffer interactively

(defun nvp-httpd--current-url ()
  (format "http://%s:%d" (cl-case httpd-host
                           ((nil) "0.0.0.0")
                           ((local) "localhost")
                           (otherwise httpd-host))
          httpd-port))

(defun nvp-httpd-browse-current ()
  (interactive)
  (browse-url (nvp-httpd--current-url)))

(defun nvp-httpd--maybe-read-config ()
  (if (and (httpd-running-p)
           (not (y-or-n-p (format "Server running on %s, reconfigure? "
                                  (nvp-httpd--current-url)))))
      (list httpd-root httpd-port)
    (list (read-directory-name "Root directory: " default-directory nil t)
          (read-number "Port: " 8017))))

;; `imp-visit-buffer' doesn't seem to work for me
;; is `system-name' supposed to be handled in the browser correctly?
;;;###autoload
(defun nvp-httpd-browse-buffer (&optional arg)
  "Server buffer live with `httpd' and `impatient-mode'.
With prefix ARG, browse buffer listing instead."
  (interactive "P")
  (or (httpd-running-p)
      (apply #'nvp-httpd-start-here (nvp-httpd--maybe-read-config)))
  (funcall #'imp-visit-buffer arg))

;;;###autoload
(defun nvp-httpd-start-here (directory port &optional open)
  "Start httpd server in DIRECTORY on PORT.
If OPEN, browse on localhost."
  (interactive (append (nvp-httpd--maybe-read-config) '(t)))
  (setq httpd-root directory
        httpd-port port
        httpd-host 'local)
  (httpd-start)
  (and open (browse-url (nvp-httpd--current-url))))

;; -------------------------------------------------------------------
;;; Menu

;;;###autoload(autoload 'nvp-httpd-menu "nvp-httpd")
(transient-define-prefix nvp-httpd-menu ()
  "Serve buffer/directory"
  ["Httpd"
   ("s" "Start with dir/port and open" nvp-httpd-start-here)
   ("q" "Stop server" httpd-stop :if (lambda () (httpd-running-p)))
   ("o" "Open current" nvp-httpd-browse-current :if (lambda () (httpd-running-p)))]
  [ :if (lambda () (featurep 'impatient-mode))
    "Impatient"
    (":h" "Toggle htmlize" imp-toggle-htmlize)
    ("l" "Server buffer live" nvp-httpd-browse-buffer)]
  (interactive)
  (require 'impatient-mode nil t)
  (transient-setup 'nvp-httpd-menu))

(provide 'nvp-httpd)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-httpd.el ends here
