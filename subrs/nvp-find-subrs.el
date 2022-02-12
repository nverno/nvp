;;; nvp-find-subrs.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; - shared b/w ag and rg
;; - Defaults for searching with grep/ag/rg
;;; Code:
(require 'nvp-macro)
(require 'nvp)
(nvp:decls)

(defsubst nvp:find-symbol (query &optional regex)
  (if regex query (format "\\b%s\\b" query)))

;;; Ag/rg 
(defsubst nvp:match-grouped-filename (file-column-re grouped-re)
  "Match grouped filename in compilation output, not relying on escape codes."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (while (and (not (bobp))
                  (looking-at-p file-column-re))
        (forward-line -1))
      (and (looking-at grouped-re)
           (list (match-string 1))))))

;; -------------------------------------------------------------------
;;; Search defaults for ag/rg/rgrep

;; get thing to search for: region, symbol-at-point, or prompt
(defsubst nvp:find-search-term (search-prompt &optional force-prompt)
  (let ((search-term
         (--if-let (nvp:tap 'dwim)
             (if force-prompt
                 (read-from-minibuffer
                  (format (concat search-prompt " (%s): ") it) it
                  nil nil nil 'nvp-search-history)
               (prog1 it (add-to-history 'nvp-search-history it)))
           (read-from-minibuffer
            (concat search-prompt ": ") nil nil nil 'nvp-search-history))))
    search-term))

;; determine the search root directory
(defsubst nvp:find-seach-root (&optional root prompt)
  (or root
      (and prompt (read-directory-name "Search root: "))
      (or (nvp-project-root)
          (read-directory-name
           (format "Search root ('%s'):"
                   (file-name-directory default-directory))))))

;; return (search-root search-term regexp-p)
(defsubst nvp:find-defaults (arg search-prompt &optional root)
  (let ((prompt (equal '(16) arg)))
    (list (nvp:find-seach-root root prompt)
          (nvp:find-search-term search-prompt prompt)
          (if prompt (y-or-n-p "Use regex?") (equal '(4) arg)))))

(provide 'nvp-find-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-find-subrs.el ends here
