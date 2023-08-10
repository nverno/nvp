;;; nvp-net.el --- networking utils -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)
(nvp:auto "net-utils" net-utils-run-simple)

(defvar nvp-net-port-history '())
(defvar nvp-net-options-history '())
(defvar nvp-lsof-program "lsof")
(defvar nvp-lsof-program-options '("-i"))

;;;###autoload
(defun nvp-lsof (args)
  (interactive
   (list
    (if current-prefix-arg
        (split-string
         (read-string
          "Args: "
          (mapconcat 'identity nvp-lsof-program-options " ") 'nvp-net-options-history)
         nil t)
      (append nvp-lsof-program-options
              (list (concat ":" (read-string "Port: " nil 'nvp-net-port-history)))))))
  (net-utils-run-simple (format "*%s*" nvp-lsof-program) nvp-lsof-program args))

(provide 'nvp-net)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-net.el ends here
