;;; nvp-java.el --- ... -*- lexical-binding: t-*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-06 19:13:28>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/java-tools
;; Package-Requires: 
;; Created: 12 November 2016

;; This file is not part of GNU Emacs.
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

;;; TODO:
;; - abbrevs for includes / file-local things only?

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'eclim)
(require 'eclimd)
(require 'nvp-parse)

(declare-function eclim--maven-pom-path "eclim-maven")
(declare-function eclim--project-read "eclim-project")
(declare-function eclim--project-dir "eclim-common")
(declare-function javadoc-lookup "javadoc-lookup")
(declare-function eclim--completion-candidates "eclim-completion")
(declare-function eclim-java-browse-documentation-at-point "eclim-javadoc")
(declare-function eclim-javadoc-buffer "eclim-javadoc")

(declare-function nvp-maven-compile "nvp-maven")
(declare-function nvp-compile "nvp-compile")
(declare-function nvp-abbrev-expand-p "nvp-abbrev")

(nvp-package-define-root :snippets t)

;; FIXME: remove
(defun nvp-java-eclipse-releases ()
  (interactive)
  (browse-url "https://projects.eclipse.org/releases"))

;; -------------------------------------------------------------------
;;; Utils

;; check that it is a maven project
(defsubst nvp-maven-p ()
  (file-exists-p (string-trim (eclim--maven-pom-path))))

;; FIXME: remove or fix -- these can probably be replaced with eclim
(defmacro nvp-java-method-args ()
  `(save-excursion
     (beginning-of-defun)
     (when (re-search-forward "(\\([^)]*\\))")
       (match-string-no-properties 1))))

;; FIXME: doesn't work 
(defmacro nvp-java-method-name-and-args ()
  `(save-excursion
     (beginning-of-defun)
     (when (re-search-forward "\\([A-Za-z]+\\)\\s-*(")
       (let ((method (match-string-no-properties 1))
             args)
         (while (re-search-forward "\\([A-Za-z]+\\)[\[\]\\s-]*[,)]"
                                   (line-end-position) t)
           (push (match-string-no-properties 1) args))
         (cons method args)))))

;; -------------------------------------------------------------------
;;; Generics
(require 'nvp-parse)

(cl-defmethod nvp-parse-current-function
  (&context (major-mode java-mode) &rest _args)
  (save-excursion
    (beginning-of-defun)
    (search-forward "(")
    (backward-char 2)
    (thing-at-point 'symbol t)))

;; -------------------------------------------------------------------
;;; Commands

;;--- newline
;; FIXME: remove
(nvp-newline nvp-java-newline-dwim nil
  :pairs (("{" "}"))
  :comment-re (" *\\(?:/\\*\\|\\*\\)" . "\\*/ *")
  :comment-start "* ")

;;--- Movement
;; FIXME: convert these to beginning/end-of-defun
;; if outside of class move to methods within instead of just jumping
;; over the whole class
(defun nvp-java-next-defun (&optional arg)
  "Move to next class or method. With ARG, move backwards."
  (interactive)
  (let ((ppss (syntax-ppss)))
    ;; if at start of class, move inside to jump to methods
    (and (= 0 (syntax-ppss-depth ppss))
         (or arg (looking-at-p "\\s-*\\(public\\|private\\|class\\)"))
         (down-list (and arg -1))))
  (beginning-of-defun (and (not arg) -1))
  (recenter))

(defun nvp-java-previous-defun ()
  (interactive)
  (nvp-java-next-defun 'previous))

;;--- Compile

(defun nvp-java-compile ()
  (interactive)
  (cond
   ((nvp-maven-p) (nvp-maven-compile))
   ((bound-and-true-p eclim-mode) (eclim-project-build))
   (t (let ((compile-command
             (format "javac %s && java %s" buffer-file-name
                     (file-name-sans-extension buffer-file-name))))
        (nvp-compile)))))

;;--- Package
;; create package structure in current directory
(defun nvp-java-new-package (root name)
  (interactive (list
                (read-directory-name "Root directory: " default-directory)
                (read-string "Package name: ")))
  (ignore-errors
    (make-directory
     (expand-file-name 
      (concat "src/java/" (replace-regexp-in-string "[.]" "/" name)) root)
     'parents)))

;; -------------------------------------------------------------------
;;; Eclim
;; (when (not (bound-and-true-p eclimd-start))
;;   (defalias 'eclimd-start 'start-eclimd))

;; create new project
;;;###autoload
(defun nvp-java-new-project ()
  (interactive)
  (require 'nvp-java)
  (if (not (process-live-p eclimd-process))
      (let ((eclimd-wait-for-process t))
        (eclimd-start eclimd-default-workspace
                      #'(lambda () (call-interactively 'eclim-project-create))))
    (call-interactively 'eclim-project-create)))

;; FIXME: opening shell in project root should be generic
;; open shell in project root
(autoload 'nvp-ext-terminal-in-dir-p "nvp-ext")
(defun nvp-java-project-shell (project-root)
  (interactive
   (let ((root (ignore-errors (eclim--project-dir))))
     (list (file-name-as-directory (or root (eclim--project-read))))))
  (if (not (file-exists-p project-root))
      (message "No project root directory found")
    (let ((default-directory project-root)
          (bname (or (nvp-ext-terminal-in-dir-p 'name project-root)
                     (concat "*java " (eclim-project-name) "*")))
          (curr (current-buffer)))
      (if (get-buffer bname)
          (switch-to-buffer-other-window bname)
        (shell bname))
      (process-put (get-buffer-process bname) :src-buffer curr))))

;; update and refresh current project
(defun nvp-java-update-current-project (project)
  (interactive (list (eclim-project-name)))
  (and project (eclim-project-update project)))

;; info on current project if in one
(defun nvp-java-project-info (&optional project)
  (interactive (list (eclim-project-name)))
  (if project (eclim-project-info-mode project)
    (call-interactively 'eclim-project-info-mode)))

;; -------------------------------------------------------------------
;;; Info

;; returns the variable and type of variable at point or nil, uses eclim's completion
(defun nvp-java-type-name ()
  (save-excursion
    (when-let* ((bnds (bounds-of-thing-at-point 'symbol))
                (var (progn (goto-char (cdr bnds))
                            (car (eclim--completion-candidates)))))
      (when (string-match-p "[:-]" var)
        (let ((res (split-string var "[:-]" nil " ")))
          (pcase (length res)
            (2 res)
            (3 (list (car res) (caddr res) (cadr res))) ;method invocation
            (_ nil)))))))

;; get the fully qualified name if it is imported: eg. java.util.Queue
;; FIXME: find a better way to get this, eg. from the *java doc* buffer
(defun nvp-java-qualified-type-name ()
  (when-let* ((var (nvp-java-type-name)))
    (save-excursion
      (goto-char (point-min))
      (forward-comment (point-max))
      (and (re-search-forward
            (concat "\\s-*import \\s-*\\([^;\n]+"
                    "\\(?:" (car var) "\\|"
                    (replace-regexp-in-string "\\s-*\\([\[\]]+\\|<+[^>]*>+\\)\\s-*"
                                              "" (cadr var))
                    "\\)\\);")
            nil 'move)
           (match-string-no-properties 1)))))

;; TODO: could use much better formatting
;; Show help in a popup window
(defun nvp-java-popup-help ()
  (interactive)
  (when-let* ((doc (save-window-excursion 
                     (call-interactively
                      'eclim-java-show-documentation-for-current-element)
                     (buffer-string))))
    (nvp-with-toggled-tip doc
      :help-fn #'(lambda () (interactive) (pop-to-buffer (eclim-javadoc-buffer))))))

;; simple for now
(defsubst nvp-java-in-javadoc ()
  (nth 4 (syntax-ppss)))

;; Get help on object at point using a variety of methods
;; default is to show help in a popup window -- this assumes that there is
;; local documentation available that eclipse knows about:
;; eg. sudo apt install default-jdk-doc and set eclipse to know about its location
(defun nvp-java-help-at-point (&optional arg)
  (interactive "P")
  (if (nvp-java-in-javadoc)
      (browse-url "https://dev.liferay.com/participate/javadoc-guidelines")
    (pcase arg
      (`(4) (eclim-java-browse-documentation-at-point))
      (`(16) (if-let* ((type (nvp-java-qualified-type-name)))
                 (javadoc-lookup type)
               (call-interactively 'javadoc-lookup)))
      (`(64) (eclim-java-browse-documentation-at-point 'prompt))
      (_ (nvp-java-popup-help)))))

(provide 'nvp-java)
;;; nvp-java.el ends here
