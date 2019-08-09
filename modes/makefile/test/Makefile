include $(CURDIR)/config.mk
-include $(CURDIR)/local.mk
SED = gsed

test: 
	@echo ${AWK}, ${SED}, ${ROOT}
	@echo ${GNUMAKEFLAGS} ${ROOT}


define LOADDEFS_TMPL
;;; ${1}-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$$) (car load-path))))


;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ${1}-autoloads.el ends here
endef
export LOADDEFS_TMPL
#'
