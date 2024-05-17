;;; nvp-perl-help.el --- help-at-point -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Trying to integrate perldoc - perldoc, Pod::Perldoc, perlpod
;; Would be nice to have a better interface to fuzzy search on metapan,
;; but haven't found a feature like that in cpanm/perlbrew.
;;
;;    perldoc -f BuiltinFunction
;;    perldoc -L it -f BuiltinFunction
;;    perldoc -q FAQ Keyword
;;    perldoc -L fr -q FAQ Keyword
;;    perldoc -v PerlVariable
;;    perldoc -a PerlAPI
;;
;; I also haven't been able to build the perldoc.info files, without them
;; getting all mangled and unusable -- I don't think they are maintained
;; with texi in mind.
;; Section 3.3 - Reference manual lists all the docs, but links don't currently
;; work (3/21/20).
;;
;; TODO:
;; - HAP toggle
;; - metapan lookup
;; - build info manual
;;
;; Refs:
;; - https://github.com/syohex/emacs-metacpan
;; - https://github.com/syohex/emacs-perl-utils/blob/master/perl-utils.el
;; - https://github.com/genehack/emacs/blob/master/etc/perl.el
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-perl)
(nvp:req 'nvp-perl 'subrs)
(require 'cperl-mode)
(require 'man)
(require 'pos-tip)

;; ------------------------------------------------------------
;;; Find Stuff
;; `perl-find-library' stuff
;; XXXX: probably just build module cache using external perl script
;; although this takes barely a second anyway

;; build perl modules paths
(nvp:define-cache-runonce nvp-perl--module-paths ()
  (cl-remove-if-not
   #'(lambda (dir)
       (and (string-match "/" dir)
            (file-exists-p dir)))
   (car
    (read-from-string
     (shell-command-to-string
      (eval-when-compile
        (concat "perl -e '$\"=\"\\\" \\\"\";"
                "print \"(\\\"@INC\\\")\"'")))))))

;; Find all perl modules in directories on @INC, and cache
;; searches for files ending in .pod or .pm and translates
;; file path separators to '::'
(nvp:define-cache-runonce nvp-perl-modules ()
  (cl-mapcan
   (lambda (dir)
     (mapcar
      (lambda (file)
        (nvp-perl-replace-all
         ;; chop suffixes
         (rx (seq "." (| "pod" "pm") string-end))
         ""
         ;; convert file path separators to '::'
         (nvp-perl-replace-all
          "/" "::" (substring file (1+ (length dir))))))
      (find-lisp-find-files
       dir (rx (seq "." (| "pod" "pm") string-end)))))
   (nvp-perl--module-paths)))

;; return path to perl module
(defun nvp-perl-module-path (module)
  (let ((path
         (shell-command-to-string (concat "perldoc -l " module))))
    (and (not (string-match-p "No documentation" path))
         (substring path 0 (1- (length path))))))

;; completing read for installed modules
(defun nvp-perl-read-module (&optional prompt default path)
  (nvp:defq default (thing-at-point 'perl-module t))
  (setq prompt (nvp:prompt-default (or prompt "Module: ") default))
  (let ((module (nvp-completing-read prompt (nvp-perl-modules))))
    (if path (nvp-perl-module-path module)
      path)))

;;;###autoload
(defun nvp-perl-jump-to-module (module)
  "Jump to MODULE in other window."
  (interactive (list (nvp-perl-read-module nil nil 'path)))
  (with-demoted-errors "Error in nvp-perl-jump-to-module: %S"
    (find-file-other-window module)))

;; -------------------------------------------------------------------
;;; Perldoc

;;;###autoload
(defun nvp-perl-help-perldoc (thing &optional online)
  "Lookup perldoc for THING at point, trying variable, module, symbol.
(4)  With single prefix or ONLINE, lookup docs on CPAN.
(16) With double prefix, prompt for module first and look for local docs.
(64) With any higher prefix, prompt for module and lookup online."
  (interactive
   (let* ((arg (prefix-numeric-value current-prefix-arg))
          (local (not (memq arg '(4 64))))
          (thing (or (and (>= arg 16)
                          (nvp-perl-read-module nil nil local))
                    (thing-at-point 'perl-variable)
                    (when-let* ((mod (thing-at-point 'perl-module)))
                      (nvp-perl-read-module nil mod local))
                    (thing-at-point 'symbol)
                    (nvp-perl-read-module nil nil local))))
     (list thing (not local))))
  (if online
      (browse-url (format "https://search.cpan.org/perldoc?%s" thing))
    (cperl-perldoc thing)))

;;; Cpan / Cpanm

;; Search cpan online for query.
;;;###autoload
(defun nvp-perl-help-cpan-online (query)
  (interactive "sQuery: ")
  (browse-url (format "https://search.cpan.org/search?query=%s&mode=all" query)))

;; -------------------------------------------------------------------
;;; TODO: help-at-point

;;;###autoload
(defun nvp-perl-help-at-point (_obj)
  (interactive "P")
  (or (x-hide-tip)
      (let* ((sym (cperl-word-at-point))
             (case-fold-search nil)
             ;; `cperl-perldoc'
             (is-func (and (string-match-p "^[a-z]+$" sym)
                           (string-match-p (concat "^" sym "\\>")
                                           (documentation-property
                                            'cperl-short-docs
                                            'variable-documentation))))
             (Man-switches "")
             (Man-notify-method 'quiet)
             (manual-program (if is-func "perldoc -f" "perldoc")))
        (Man-getpage-in-background sym))))

(provide 'nvp-perl-help)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-perl-help.el ends here
