;;; nvp-julia.el --- julia mode utilities -*- lexical-binding: t -*-
;;; Commentary:
;; TODO:
;; - add company jump to location function
;; - fix macrostep-julia
;; - fix capf w/ ess + latexsubs
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'company)
(require 'julia-mode)
(require 'ess-site)
(require 'ess-julia)
(declare-function macrostep-expand "macrostep")
(declare-function pos-tip-show "pos-tip")

;;; XXX: `julia-latexsub' still called by ESS
(unless (fboundp 'julia-latexsub)
  (defun julia-latexsub (&rest _) nil))

(autoload 'tag-utils-tag-dir "tag-utils")
(autoload 'nvp-log "nvp-log")

;; -------------------------------------------------------------------
;;; Vars 

;; FIXME: remove
(defvar nvp-julia-source-repo "https://github.com/JuliaLang/julia")
(defvar nvp-julia-source-dir
  (expand-file-name "julia" (or (bound-and-true-p tag-utils-source-dir)
                                (getenv "DEVEL"))))

;; ------------------------------------------------------------
;;; Tags

(defun nvp-julia-tag-source (&optional noretry)
  "Tag julia source code. Prefix to force retag."
  (interactive)
  (let* ((tags (expand-file-name "TAGS" nvp-julia-source-dir))
         (no-src (not (file-exists-p nvp-julia-source-dir)))
         (no-tags (not (file-exists-p tags))))
    (unless noretry
      (cond
       (no-src
        (nvp-log "Cloning julia repo: %s" nil nvp-julia-source-repo)
        (nvp-julia-tag-sentinel
         (start-process "jl-tags" "*jl-tags*" "git" "clone"
                        nvp-julia-source-repo
                        nvp-julia-source-dir)
         nil))
       ((or current-prefix-arg no-tags)
        (nvp-log "Creating julia source TAGS")
        ;; nvp-julia-tag-sentinel
        (tag-utils-tag-dir nvp-julia-source-dir
                           :program
                           (or (bound-and-true-p tag-utils-ctags-exe)
                               "ctags")))
       (t (visit-tags-table tags))))))

(defun nvp-julia-tag-sentinel (proc &optional noretry)
  (set-process-sentinel
   proc #'(lambda (p m)
            (nvp-log "%s: %s" nil (process-name p) m)
            (when (eq 0 (process-exit-status p))
              (nvp-julia-tag-source noretry)))))

;; -------------------------------------------------------------------
;;; Yas
(declare-function yas-text "yasnippet")

;; Split a julia argument string into ((name, default)..) tuples
(defun nvp-julia-split-args (arg-string)
  (and arg-string
       (mapcar (lambda (x)
                 (split-string x "[[:blank:]]*=[[:blank:]]*" t))
               (split-string arg-string "[[:blank:]]*[,;][[:blank:]]*" t))))
 
;; return docstring format for the julia arguments in yas-text
(defun nvp-julia-args-to-docstring ()
  (if-let* ((indent (concat "\n" (make-string (current-column) ? )))
            (args (nvp-julia-split-args (yas-text)))
            (max-len
             (if args
                 (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args))
               0))
            (formatted-args
             (mapconcat
              (lambda (x)
                (concat "- "
                        (nth 0 x)
                        (make-string (- max-len (length (nth 0 x))) ? )
                        " : "
                        (if (nth 1 x)
                            (concat "\(default " (nth 1 x) "\)"))))
              args
              indent)))
      (if (string-empty-p formatted-args) ""
        (mapconcat 'identity (list "# Arguments" formatted-args) indent))
    ""))

(provide 'nvp-julia)
;;; nvp-julia.el ends here
