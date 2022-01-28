;;; inf-julia --- inferior julia -*- lexical-binding: t -*-
;;; Code:
(eval-when-compile (require 'nvp-macro))
(defvar term-prompt-regexp)
(defvar company-backends)

(autoload 'cygwin-utils-convert-path "cygwin-utils")
(autoload 'inferior-ess "ess-inf" "Run an ESS process.")

(defvar inf-julia-directory)
(defvar inf-julia-args)
(defvar julia-arguments)
(defvar inf-julia-history-file (expand-file-name ".julia_history" nvp/cache))
(defvar inf-julia-program (nvp:program "julia"))
(defvar inf-julia-process-buffer "*julia*")

(when load-file-name 
  (setq inf-julia-directory (file-name-directory load-file-name))
  (when (eq system-type 'windows-nt)
    (let ((init (expand-file-name "repl_init.jl" inf-julia-directory)))
      (setq inf-julia-args (format "-i -L%s" init)
            julia-arguments `("-i" "-L" ,init)))))

;; ------------------------------------------------------------

;; (defun inf-julia-send-string-function (process string visibly)
;;   "Send the Julia STRING to the PROCESS.
;; VISIBLY is not currently used."
;;   (let ((file (concat temporary-file-directory "julia_eval_region.jl")))
;;     (with-temp-file file
;;       (insert string))
;;     (process-send-string process (format ess-load-command file))))

;;;###autoload
(defun inf-julia-term ()
  (interactive)
  (prog1 (ansi-term inf-julia-program "julia-term")
    (setq-local term-prompt-regexp "^julia> ")))

(defun inf-julia-input-sender (proc string)
  (save-current-buffer
    (let* ((help-?-regexp "^ *\\(?:\\(?1: *?\\? *\\)\\(?2:.+\\)\\)")
           (help-?-match (string-match help-?-regexp string)))
      (cond (help-?-match
             (ess-display-help-on-object (match-string 2 string))
             (process-send-string proc "\n"))
            (t ;; normal command
             (inferior-ess-input-sender proc string))))))

;; Run inferior julia process
;; TODO:
;; - input sender: shell, help, regular modes
;; - completion: julia-objects, filenames (in strings?)
;; - change working buffer: #<marker at 19386 in ielm.el>

(defvar inf-julia-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km comint-mode-map)
    (define-key km (kbd "<f2> /") #'inf-julia-ring-complete)
    km))

;; #<marker at inferior-ess-mode in ess-inf.el>
;; #<marker at comint-send-string in comint.el>
;; #<marker at 36584 in term.el>
(define-derived-mode inf-julia-mode comint-mode "Julia"
  "Major mode for interacting with inferior julia process.
Bindings:
\\{inf-julia-mode-map}"
  (setq-local comint-input-sender 'inf-julia-input-sender))

;; ------------------------------------------------------------

(defun inf-julia (&optional start-args)
  (interactive "P")
  (let* ((jl-start-args
          (concat inf-julia-args " "
                  (if start-args
                      (read-string
                       (concat "Starting Args"
                               (if inf-julia-args
                                   (concat " [other than '" inf-julia-args "']"))
                               " ? "))
                    nil)))
         ;; (buff (get-buffer-create inf-julia-process-buffer))
         )

    ;; history
    (when (file-exists-p inf-julia-history-file)
      (setq comint-input-ring-file-name inf-julia-history-file)
      (comint-read-input-ring))

    ;; (set-process-sentinel ())
    (inferior-ess jl-start-args)

    (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
    (add-hook 'completion-at-point-functions 'inf-julia-object-completion nil 'local)
    (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
    (setq comint-input-sender 'inf-julia-input-sender)

    (ess--tb-start)
    ;; (set (make-local-variable 'inf-julia-basic-offset) 4)
    ;; remove ` from julia's logo
    (goto-char (point-min))
    (while (re-search-forward "`" nil t)
      (replace-match "'"))
    (goto-char (point-max))
    ;; --> julia helpers from ../etc/inf-julia.jl :
    (ess--inject-code-from-file
     (expand-file-name "scratch/help.jl" inf-julia-directory))
    
    (with-ess-process-buffer
        nil (run-mode-hooks 'inf-julia-post-run-hook))))

;; ------------------------------------------------------------

(declare-function cygwin-mount-activate "cygwin-mount")
(declare-function fakecygpty-activate "fakecygpty")

;; remove
(declare-function ess-display-help-on-object "ess")
(declare-function inferior-ess-input-sender "ess")
(declare-function ess--tb-start "ess")
(declare-function ess--inject-code-from-file "ess")
(declare-function with-ess-process-buffer "ess")

(provide 'inf-julia)
;;; inf-julia.el ends here
