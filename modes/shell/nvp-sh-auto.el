;;; nvp-sh-auto.el --- autoload lesser used things -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'sh-script)
(require 'nvp)

;; -------------------------------------------------------------------
;;; Here docs

;; FIXME: does sh-script have builtin functions to get bounds of here-docs?
;; true if point is in heredoc
(defsubst nvp-sh--here-doc-p (point)
  (eq (get-text-property point 'face) 'sh-heredoc))

;; get the here doc marker for block
(defsubst nvp-sh--here-doc-marker (&optional point)
  (let ((ppss (syntax-ppss point)))
    (when (eq t (nth 3 ppss))
      (get-text-property (nth 8 ppss) 'sh-here-doc-marker))))

;; position at beginning of first line of here-doc if point is
;; currently in a here-doc
(defun nvp-sh-here-doc-start-pos (point)
  (save-excursion
    (goto-char point)
    (back-to-indentation)
    (when (looking-back "[^<]<<.*" (line-beginning-position)) ;skip opening line
      (forward-line))
    (let ((in-heredoc (nvp-sh--here-doc-p (point))))
      (when in-heredoc                      ;search back until no marker
        (while (and (not (bobp))
                    (nvp-sh--here-doc-p (point)))
          (forward-line -1)
          (back-to-indentation))
        (point)))))

;; -------------------------------------------------------------------
;;; Commands

;; FIXME: broken
;;;###autoload
(defun nvp-sh-toggle-here-doc-indent (point)
  "Toggles here-doc indentation.
Open with '<<-' and use only leading tabs for indentation."
  (interactive "d")
  (let ((start-pos (nvp-sh-here-doc-start-pos point)))
    (when start-pos
      (save-excursion
        (goto-char start-pos)
        (search-forward-regexp "[^<]<<" (line-end-position) 'move)
        (let ((indent (not (eq ?- (char-after))))
              marker)
          (if indent                    ;toggle preceding '-'
              (insert "-")
            (delete-char 1))
          (forward-to-indentation)      ;skip past opening line
          (setq marker (nvp-sh--here-doc-marker))
          (while (and (nvp-sh--here-doc-p (point))
                      (not (looking-at-p marker)))
            (delete-horizontal-space)
            (and indent                  ;toggle indentation
                 (insert "\t"))
            (forward-to-indentation)))))))

(provide 'nvp-sh-auto)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sh-auto.el ends here
