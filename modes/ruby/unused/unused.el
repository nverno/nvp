;; https://github.com/syohex/emacs-progutil/blob/master/progutil-ruby.el
(defun progutil-ruby-insert-bar ()
  (interactive)
  (if (looking-back "\\(?:do\\s-+\\|{\\)" (line-beginning-position))
      (progn
        (insert "||")
        (backward-char 1))
    (insert "|")))

(defun progutil-ruby-beginning-of-defun (&optional arg)
  (interactive "p")
  (and (re-search-backward (concat "^\\s-+\\(" ruby-block-beg-re "\\)\\_>")
                           nil 'move arg)
       (progn (back-to-indentation) t)))

(defun progutil-ruby-end-of-defun (&optional arg)
  (interactive "p")
  (and (re-search-forward (concat "^\\s-+\\(" ruby-block-end-re "\\)\\($\\|\\b[^_]\\)")
                          nil 'move arg)
       (progn (beginning-of-line) t))
  (forward-line 1)
  (back-to-indentation))
