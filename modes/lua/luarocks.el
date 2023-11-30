;;; luarocks.el --- Luarocks -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)

(defcustom luarocks-command "luarocks"
  "Command to run luarocks executable."
  :type 'path
  :group 'luarocks)

(transient-define-prefix luarocks-menu ()
  [""])
(provide 'luarocks)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; luarocks.el ends here
