;;; nvp-perl-repl.el --- Perl repl -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls :p (inf-perl perl cperl))

(when (fboundp 'inf-perl-run)
  (nvp-repl-add '(cperl-mode perl-mode perl-ts-mode)
    :name 'perl
    :modes '(inf-perl-mode)
    :init #'inf-perl-run
    :find-fn #'inf-perl-process))

(provide 'nvp-perl-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-perl-repl.el ends here
