;;; nvp-treesit.el --- tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'treesit nil t)
(nvp:decls :p ("treesit-auto"))

;; Setup treesitter sources
(defun nvp-treesit--setup-sources ()
  (when (require 'treesit-auto nil t)
    (unless treesit-language-source-alist
      (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))))

;; maybe load sources
(cl-eval-when (load)
  (nvp-treesit--setup-sources))

;;;###autoload
(defun nvp-treesit-install ()
  "Install tree-sitter grammar."
  (interactive)
  (nvp-treesit--setup-sources)
  ;; Notes:
  ;; To remap mode to tree-sitter mode
  ;; (cl-pushnew '(<major-mode> . <treesit-mode>) major-mode-remap-alist :test #'equal)
  ;; May need to restart emacs for shared grammar library to be loaded?
  (call-interactively #'treesit-install-language-grammar))

;; -------------------------------------------------------------------
;;; Errors / missing nodes

(defface nvp-treesit-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t :underline t :inherit error))
  "Face for tree sitter errors / missing nodes.")

(defvar-local nvp-treesit--errors-query nil
  "Tree sitter query to match errors and missing identifiers")

(defun nvp-treesit--query-compile ()
  "Compile error/missing quiery."
  (when (treesit-available-p)
    (and-let* ((lang (treesit-language-at (point))))
      (setq nvp-treesit--errors-query
            (treesit-query-compile lang '(((identifier) @id (:equal "" @id))
                                          (ERROR) @err))))))

(defun nvp-treesit--make-error-overlay (beg end)
  "Make error overlay from BEG to END."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'nvp-treesit-error)
    (overlay-put ov 'nvp-treesit t)))

(defun nvp-treesit--update-errors ()
  "Update error/missing node overlays."
  (when nvp-treesit--errors-query
    (and-let* ((lang (treesit-language-at (point))))
      (if-let ((ranges
                (treesit-query-range
                 lang nvp-treesit--errors-query (point-min) (point-max))))
          (prog1 t
            (dolist (range ranges)
              (nvp-treesit--make-error-overlay (car range) (1+ (cdr range)))))
        (remove-overlays (point-min) (point-max) 'nvp-treesit t)
        nil))))

(defun nvp-treesit-toggle-errors ()
  "Toggle tree sitter error overlays in buffer."
  (interactive)
  (when nvp-treesit--errors-query
    (if (memq #'nvp-treesit--update-errors after-change-functions)
        (remove-hook 'after-change-functions #'nvp-treesit--update-errors t)
      (add-hook 'after-change-functions #'nvp-treesit--update-errors nil t))))

;; -------------------------------------------------------------------
;;; Dev Minor Mode

(defun nvp-treesit-validate (lang query)
  (interactive
   (list
    (or (treesit-language-at (point))
        (completing-read "Language: " (mapcar #'car treesit-language-source-alist)))
    (read-from-minibuffer "Query: " (sexp-at-point))))
  (treesit-query-validate lang query))

(defvar-keymap nvp-treesit-minor-mode-map
  :doc "T/S keymap"
  "C-c C-v" #'nvp-treesit-validate
  "C-c C-s" #'nvp-treesit-toggle-errors
  "C-M-?"   #'treesit-inspect-node-at-point)

(easy-menu-define nvp-treesit-minor-mode-menu nvp-treesit-minor-mode-map
  "T/S menu."
  '("T/S"
    ["Inspect" treesit-inspect-node-at-point t]
    ["Validate" nvp-treesit-validate t]
    ["Toggle errors" nvp-treesit-toggle-errors t]))

;;;###autoload
(define-minor-mode nvp-treesit-minor-mode
  "Treesit development minor mode."
  :lighter " T/S"
  :keymap nvp-treesit-minor-mode-map
  ;; XXX: powerline doesn't display `mode-line-misc-info' where
  ;; `treesit-inspect-mode' wants to put the node info
  ;; (powerline-revert)
  (if nvp-treesit-minor-mode
      (when (treesit-language-at (point))
        (and (nvp-treesit--query-compile)
             (add-hook 'after-change-functions #'nvp-treesit--update-errors nil t))
        (powerline-revert)
        (treesit-inspect-mode)
        (treesit-explore-mode))
    (remove-hook 'after-change-functions #'nvp-treesit--update-errors t)
    (treesit-inspect-mode -1)
    (treesit-explore-mode -1)
    (nvp-theme-switch)))

(provide 'nvp-treesit)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-treesit.el ends here
