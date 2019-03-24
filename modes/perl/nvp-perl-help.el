;;; nvp-perl-help.el --- help-at-point -*- lexical-binding: t; -*-

;; Last modified: <2019-03-24 03:25:05>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  9 November 2016

;;; Commentary:

;; TODO:
;; - HAP toggle
;; - metapan lookup

;; Refs:
;; - https://github.com/syohex/emacs-metacpan
;; - https://github.com/syohex/emacs-perl-utils/blob/master/perl-utils.el
;; - https://github.com/genehack/emacs/blob/master/etc/perl.el

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp-perl)
(require 'cperl-mode)
(require 'man)
(require 'pos-tip)

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
      (browse-url (format "http://search.cpan.org/perldoc?%s" thing))
    (cperl-perldoc thing)))

;;; Cpan / Cpanm

;; Search cpan online for query.
;;;###autoload
(defun nvp-perl-help-cpan-online (query)
  (interactive "sQuery: ")
  (browse-url (format "http://search.cpan.org/search?query=%s&mode=all" query)))

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
;;; nvp-perl-help.el ends here
