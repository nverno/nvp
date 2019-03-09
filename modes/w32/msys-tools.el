;;; msys-utils --- 
(eval-when-compile
  (require 'cl-lib))
(require 'w32-tools)

;; ------------------------------------------------------------
;;* Link

(cl-defun msys-utils-link (&optional
                           &key link args description target-path icon wd)
  (w32-tools-create-link :link (or link (expand-file-name "msys.lnk"
                                                          w32-tools--dir))
                         :args (or args "-i /msys2.ico /usr/bin/bash --login")
                         :description (or description "msys2 shell console")
                         :target-path (or target-path
                                          (expand-file-name "usr/bin/mintty.exe"
                                                            (getenv "MSYS_HOME")))
                         :icon (or icon (expand-file-name "msys2.ico"
                                                          (getenv "MSYS_HOME")))
                         :wd (or wd (getenv "HOME"))))


(provide 'msys-utils)
;;; msys-utils.el ends here
