;;; nvp-json.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'json-mode nil t)
(nvp:decls :p (json))

(with-eval-after-load 'json-mode
  (defconst nvp-json-syntax-table
    (let ((tab (copy-syntax-table json-mode-syntax-table)))
      (modify-syntax-entry ?. "_" tab)
      tab))

  ;; expand x.y => "x" : "y"
  (defun nvp-json-expand-dot (bnds)
    (interactive
     (list (with-syntax-table nvp-json-syntax-table
             (bounds-of-thing-at-point 'symbol))))
    (and bnds
         (cl-destructuring-bind (a b)
             (split-string (buffer-substring-no-properties (car bnds) (cdr bnds))
                           "[.]" 'omit " ")
           (delete-region (car bnds) (cdr bnds))
           (insert (format "\"%s\": \"%s\"" a b))))))

;; From
;; https://isamert.net/2022/01/04/dealing-with-apis-jsons-and-databases-in-org-mode.html
;; Note: In :node blocks, 'it' is bound to the json source, eg.
;;   #+begin_src json :node it.title.toUpperCase()
(defun org-babel-execute:json (body params)
  "Adds :jq and :node exectutors to PARAMS in json source blocks.
With the :node executor, \"it\" is bound to the BODY json."
  (let ((jq (cdr (assoc :jq params)))
        (node (cdr (assoc :node params))))
    (cond
     (jq
      (with-temp-buffer
        ;; Insert the JSON into the temp buffer
        (insert body)
        ;; Run jq command on the whole buffer, and replace the buffer contents
        ;; with the result returned from jq
        (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" jq) nil 't)
        ;; Return the contents of the temp buffer as the result
        (buffer-string)))
     (node
      (with-temp-buffer
        (insert (format "const it = %s;" body))
        (insert node)
        (shell-command-on-region (point-min) (point-max) "node -p" nil 't)
        (buffer-string))))))

(provide 'nvp-json)
;;; nvp-json.el ends here
