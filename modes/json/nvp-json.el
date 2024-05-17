;;; nvp-json.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'json-ts-mode nil t)
(nvp:decls :p (json jsons))

(defvar nvp-json-syntax-table)
(with-eval-after-load 'json-ts-mode
  (setq nvp-json-syntax-table
        (let ((tab (copy-syntax-table json-ts-mode--syntax-table)))
          (modify-syntax-entry ?. "_" tab)
          tab)))

;; Simple expansions:
;;  x.y => "x": "y"
;;  x.y.z => "x" : { "y": "z" }
;;  ...
(defun nvp-json-expand-dot ()
  "Expand snippet preceding point."
  (interactive)
  (pcase-let ((`(,start . ,end) (with-syntax-table nvp-json-syntax-table
                                  (bounds-of-thing-at-point 'symbol))))
    (when (and start end)
      (cl-labels
          ((insert-kvs (len parts &optional indent)
             (or indent (setq indent 0))
             (let ((indent-str (make-string (or indent 0) ? )))
               (when (nvp:line-empty-p)
                 (beginning-of-line 1)
                 (insert indent-str))
               (pcase len
                 (1 (insert (car parts)))
                 (2 (insert (format "\"%s\": \"%s\"" (car parts) (cadr parts))))
                 (_ (insert (format "\"%s\": {\n" (car parts)))
                    (insert-kvs
                     (1- len) (cdr parts) (+ indent json-ts-mode-indent-offset))
                    (insert (format "\n%s}" indent-str)))))))
        (let ((kvs (split-string (buffer-substring start end) "[.]" t)))
          (delete-region start end)
          (insert-kvs (length kvs) kvs (current-indentation)))))))

(defun nvp-json-format-buffer (start end)
  "Format buffer between START and END."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((name (file-name-nondirectory (buffer-name))))
    (pcase (file-name-nondirectory (buffer-file-name))
      ("package.json"
       (shell-command-on-region start end "npx sort-package-json"))
      (_ (if (fboundp 'json-mode-beautify)
             (json-mode-beautify start end)
           (user-error "unimplemented for '%s'" name))))))

;; From
;; https://isamert.net/2022/01/04/dealing-with-apis-jsons-and-databases-in-org-mode.html
;; Note: In :node blocks, 'it' is bound to the json source, eg.
;;   #+begin_src json :node it.title.toUpperCase()
(defun org-babel-execute:json (body params)
  "Adds :jq and :node exectutors to PARAMS in json source blocks.
With the :node executor, \"it\" is bound to the BODY json."
  (let ((jq (cdr (assoc :jq params)))
        (node (cdr (assoc :node params))))
    (cond (jq (with-temp-buffer
                (insert body)           ; Insert the JSON into the temp buffer
                ;; Run jq command on the whole buffer, and replace the buffer
                ;; contents with the result returned from jq
                (shell-command-on-region
                 (point-min) (point-max) (format "jq -r \"%s\"" jq) nil 't)
                (buffer-string)))
          (node (with-temp-buffer
                  (insert (format "const it = %s;" body))
                  (insert node)
                  (shell-command-on-region
                   (point-min) (point-max) "node -p" nil 't)
                  (buffer-string))))))

(nvp:auto "nvp-hap" nvp-hap-doc-buffer)
(nvp:auto "json-snatcher" jsons-get-path)
(defun nvp-hap-json (command &optional _arg &rest _args)
  (cl-case command
    (thingatpt (jsons-get-path))
    (doc-buffer
     (let* ((py-path (jsons-print-path-python))
            (jq-path (jsons-print-path-jq)))
       (list (nvp-hap-doc-buffer
              (format "jq: %s\npython: %s" py-path jq-path)))))))

(provide 'nvp-json)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-json.el ends here
