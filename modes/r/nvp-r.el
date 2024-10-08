;;; nvp-r.el --- r helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ess-site nil t)
(require 'ess-inf nil t)
(nvp:decls :p (ess) :v (inferior-R-program))
(nvp:auto "s" 's-matched-positions-all)


(with-eval-after-load 'nvp-repl
  (require 'nvp-r-repl))

;; Add comment continuation when in roxy block
(nvp:defmethod nvp-newline-dwim-comment (_syntax arg)
  :modes (ess-r-mode r-ts-mode)
  (save-match-data
    (--if-let (and nvp-newline-comment-continue
                   (save-excursion
                     (beginning-of-line)
                     (and (looking-at "^\\(#+'\\)")
                          (match-string 1))))
        (dotimes (_ (or arg 1))
          (insert ?\n it))
      (newline-and-indent arg))))

(defun nvp-r-roxy ()
  "Convert regular comments to roxygen."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (while (re-search-forward "^#+\'?" (region-end) 'move)
          (replace-match "##\'"))
      (goto-char (point-min))
      (while (re-search-forward "^#+\'?" nil t)
        (replace-match "##\'")))))

(defun nvp-r-roxy-preview (type)
  (interactive (list (nvp-completing-read "Preview: " '("Rd" "HTML" "text"))))
  (funcall-interactively (intern (concat "ess-roxy-preview-" type))))

;;; Tables
;; Guess where the column breaks are located (assumes right-justified).
(defun nvp-r-guess-column-breaks (str)
  (--> (--map
        (--map (cdr it) (s-matched-positions-all "\'.*?\'\\|[-0-9.A-Za-z]+" it))
        (split-string str "\n"))
       (cl-remove-if-not (lambda (x) (> (length x) 0)) it)
       (nreverse (nvp:list-intersection it))))

(defun nvp-r-table-insert-commas (str &optional beg end)
  "Insert commas into space separated table (assume right-justified)."
  (interactive
   (-let (((beg . end) (nvp:tap 'bdwim 'paragraph :pulse t)))
     (list (and beg end (buffer-substring-no-properties beg end))
           beg end t)))
  (let* ((pos (nvp-r-guess-column-breaks str))
         (table
          (--> (--map (with-temp-buffer
                        (insert it)
                        (mapc (lambda (y) (goto-char (+ y (nth (1- y) pos)))
                                (insert ","))
                              (number-sequence 1 (length pos)))
                        (buffer-string))
                      (split-string str "\n" 'omit "\t "))
               (mapconcat 'identity it "\n"))))
    (if (and beg end)
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert table))
      table)))

(defvar nvp-r-datetime-regex
  (nvp:concat "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} "
              "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"))

(defun nvp-r-table-quote-datetime (&optional str beg end)
  "Quote date times to use with \\='read.table."
  (interactive (list nil (nvp:tap 'bdwim 'paragraph :pulse t)))
  (if str (replace-regexp-in-string nvp-r-datetime-regex "\'\\1\'" str)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward nvp-r-datetime-regex end t)
        (replace-match "\'\\1\'")))))


;;;###autoload
(defun nvp-r-spin-file (file)
  "Spin yarn from FILE and show result in browser or open if it is a pdf.
Use ess-process when available, otherwise try rscript."
  (interactive
   (list (or (and (eq major-mode 'ess-r-mode) buffer-file-name)
             (read-file-name "File to spin: "))))
  (let ((proc (and (fboundp 'ess-get-process)
                   (ignore-errors (ess-get-process nil t))))
        (cmd (concat "local({ "
                     "out <- rmarkdown::render(\"%s\", \"all\");"
                     "if (tools::file_ext(out) == \"html\") {"
                     "    browseURL(out);"
                     "} else {"
                     "    out;"
                     "}"
                     "})\n")))
    (if proc
        (let ((res (ess-get-words-from-vector (format cmd file))))
          (pcase (file-name-extension (car res))
            (`"pdf" (find-file-other-window (car res)))
            (_ (browse-url-file-url (car res)))))
      ;; FIXME: not tested, doesn't open or return pdf
      (start-process-shell-command
       "rscript" nil
       (concat "Rscript -e \"" (format cmd file) "\"")))))

(provide 'nvp-r)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-r.el ends here
