;; ------------------------------------------------------------  -*- lexical-binding: t; -*-
;;; Tags

;; FIXME: remove
;; (defvar nvp-julia-source-repo "https://github.com/JuliaLang/julia")
;; (defvar nvp-julia-source-dir
;;   (expand-file-name "julia" (or (bound-and-true-p tag-utils-source-dir)
;;                                 (getenv "DEVEL"))))

;; (autoload 'tag-utils-tag-dir "tag-utils")
;; (autoload 'nvp-log "nvp-log")

;; (defun nvp-julia-tag-source (&optional noretry)
;;   "Tag julia source code. Prefix to force retag."
;;   (interactive)
;;   (let* ((tags (expand-file-name "TAGS" nvp-julia-source-dir))
;;          (no-src (not (file-exists-p nvp-julia-source-dir)))
;;          (no-tags (not (file-exists-p tags))))
;;     (unless noretry
;;       (cond
;;        (no-src
;;         (nvp-log "Cloning julia repo: %s" nil nvp-julia-source-repo)
;;         (nvp-julia-tag-sentinel
;;          (start-process "jl-tags" "*jl-tags*" "git" "clone"
;;                         nvp-julia-source-repo
;;                         nvp-julia-source-dir)
;;          nil))
;;        ((or current-prefix-arg no-tags)
;;         (nvp-log "Creating julia source TAGS")
;;         ;; nvp-julia-tag-sentinel
;;         (tag-utils-tag-dir nvp-julia-source-dir
;;                            :program
;;                            (or (bound-and-true-p tag-utils-ctags-exe)
;;                                "ctags")))
;;        (t (visit-tags-table tags))))))

;; (defun nvp-julia-tag-sentinel (proc &optional noretry)
;;   (set-process-sentinel
;;    proc #'(lambda (p m)
;;             (nvp-log "%s: %s" nil (process-name p) m)
;;             (when (eq 0 (process-exit-status p))
;;               (nvp-julia-tag-source noretry)))))
