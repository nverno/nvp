;;; sml-interaction.el --- Show sml results in overlay -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/sml-utils
;; Last modified: <2019-01-25 23:07:49>
;; Created: 6 September 2016
;; Package-Requires ((emacs "24.1") (cl-lib "0.5") (sml-mode "6.4"))

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Show results from evaluating next paragraph in SML process as overlays in
;; current emacs buffer.  Like overlays in from `cider'.

;; Install: 

;; Just require this file or autoload `sml-interaction-eval'.  And bind to a key,

;; ```lisp
;; ;; (autoload 'sml-interaction-eval "path/to/this/file")
;; (require 'sml-interaction)
;; (define-key sml-mode-map (kbd "C-c C-t") #'sml-interaction-eval)
;; ```

;; Example:

;; ![overlay](ex/overlay.png)

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'comint)
(require 'sml-mode)

(defvar sml-interaction-buffer-name "*sml*")

;; ------------------------------------------------------------
;;* Overlays to show results: see cider-overlays.el

(defface sml-interaction-result-overlay-face
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "yellow"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "black")
     :foreground "#d6a522"))
  "Face used to display evaluation results at the end of line."
  :group 'sml)

(defun sml-interaction--remove-overlay ()
  "Remove result overlays from buffer."
  (remove-hook 'post-command-hook
               #'sml-interaction--remove-overlay 'local)
  (remove-overlays nil nil 'category 'result))

(defun sml-interaction--remove-overlay-after-command ()
  "Add hook to remove overlays after next input command."
  (remove-hook 'post-command-hook
               #'sml-interaction--remove-overlay-after-command 'local)
  (add-hook 'post-command-hook
            #'sml-interaction--remove-overlay nil 'local))

(defun sml-interaction--make-overlay (l r &rest props)
  "Make overlay b/w L and R."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'category 'result)
    (while props (overlay-put o (pop props) (pop props)))
    o))

(defun sml-interaction--make-result-overlay (value where &rest props)
  "Make overlays to display results."
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char where)
        (skip-chars-backward "\r\n[:blank:]")
        (let* ((beg where)
               (end (line-end-position))
               (display-string (concat
                                (mapconcat 'identity value "\n") "\n")))
          (remove-overlays beg end 'category 'result)
          (font-lock-prepend-text-property
           0 (length display-string) 'face 'sml-interaction-result-overlay-face
           display-string)
          (when (or (string-match "\n." display-string)
                    (> (string-width display-string)
                       (- (window-width) (current-column))))
            (setq display-string (concat " \n" display-string)))
          (put-text-property 0 1 'cursor 0 display-string)
          (apply #'sml-interaction--make-overlay beg end 'after-string
                 display-string props)
          (if this-command
              (add-hook 'post-command-hook
                        #'sml-interaction--remove-overlay-after-command
                        nil 'local)
            (sml-interaction--remove-overlay-after-command)))))))

;; ------------------------------------------------------------
;;* Show result of evaluating last paragraph

;;;###autoload
(defun sml-interaction-eval ()
  "Evaluate next paragraph as marked by `sml-mark-function', 
or region if currently active."
  (interactive)
  (let (start end)
    (if (not (region-active-p))
        (save-mark-and-excursion
         (sml-mark-function)
         (setq start (progn
                       (goto-char (region-beginning))
                       (comment-forward (point-max))
                       (point))
               end (region-end)))
      (setq start (region-beginning) end (region-end)))
    (sml-interaction--make-result-overlay
     (sml-interaction-send-region start end) start)))

(defun sml-interaction-send-region (start end)
  "Send region to sml process."
  (let ((_proc (sml-prog-proc-proc))
        (tmp (make-temp-file "emacs-region")))
    (write-region start end tmp nil 'silently)
    (when sml-prog-proc--tmp-file
      (ignore-errors (delete-file (car sml-prog-proc--tmp-file)))
      (set-marker (cdr sml-prog-proc--tmp-file) nil))
    (setq sml-prog-proc--tmp-file (cons tmp (copy-marker start)))
    (sml-interaction-output tmp)))

(defun sml-interaction-output (tmp-file)
  "Get results from sml process."
  (with-current-buffer (sml-interaction-buffer
                        sml-interaction-buffer-name)
    (let* ((string-buffer "")
           (comint-input-filter (lambda (_input) nil))
           (comint-output-filter-functions
            (cons (lambda (text) (setq string-buffer (concat string-buffer
                                                        text)))
                  comint-output-filter-functions))
           (proc (get-buffer-process (current-buffer))))
      (sml-prog-proc-send-string
       proc (sml-prog-proc--call load-cmd tmp-file))
      (accept-process-output proc)
      (cl-remove-if (lambda (x) (or (string-prefix-p "val it =" x)
                               (string-prefix-p "-" x)))
                    (split-string
                     (replace-regexp-in-string "\\[opening.*" ""
                                               string-buffer) "\n"
                     t "\\s-")))))

(defun sml-interaction-buffer (buffer)
  "Get sml process buffer, or start one."
  (or (get-buffer buffer)
      (save-window-excursion
        (call-interactively 'run-sml)
        (sleep-for 0.25)
        (current-buffer))))

(provide 'sml-interaction)

;;; sml-interaction.el ends here
