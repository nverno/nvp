;;; nvp-asm.el --- assembly things  -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO:
;; - i don't like the default DWIM, i want two comments on blank lines so the
;;   indentation doesn't get messed up
;; - add smart tab behaviour, eg. `mips-indent-line', to nasm
;; - x86-lookup could find longest common substring of thing-at-point
;;   with the mnemonics since it misses common things like pushq
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls :v (asm-mode-syntax-table))

(defconst nvp-asm-imenu-expression
  '((nil "^\\s-*\\(\\_<[a-zA-Z_?][a-zA-Z0-9_$#@~?]*\\_>\\)\\s-*:" 1)))

(with-eval-after-load 'asm-mode
  (modify-syntax-entry ?% "'" asm-mode-syntax-table))

;; nasm comment-dwim -- modified `nasm-comment'
; (defun nvp-nasm-comment-dwim (&optional arg)
;   "Begin or edit a comment with context-sensitive placement.

; The right-hand comment gutter is far away from the code, so this
; command uses the mark ring to help move back and forth between
; code and the comment gutter.

; * If no comment gutter exists yet, mark the current position and
;   jump to it.
; * If already within the gutter, pop the top mark and return to
;   the code.
; * If on a line with no code, just insert a comment character.
; * If within the indentation, just insert a comment character.
;   This is intended prevent interference when the intention is to
;   comment out the line.

; With a prefix arg, kill the comment on the current line with
; `comment-kill'."
;   (interactive "p")
;   (if (not (eql arg 1))
;       (comment-kill nil)
;     (cond
;      ;; Empty line, or inside a string? Insert.
;      ((nasm--empty-line-p)
;       (insert (comment-)))
;      ((or (nasm--empty-line-p) (nth 3 (syntax-ppss)))
;       (insert ";"))
;      ;; Inside the indentation? Comment out the line.
;      ((nasm--inside-indentation-p)
;       (insert ";"))
;      ;; Currently in a right-side comment? Return.
;      ((and (nasm--line-has-comment-p)
;            (nasm--line-has-non-comment-p)
;            (nth 4 (syntax-ppss)))
;       (setf (point) (mark))
;       (pop-mark))
;      ;; Line has code? Mark and jump to right-side comment.
;      ((nasm--line-has-non-comment-p)
;       (push-mark)
;       (comment-indent))
;      ;; Otherwise insert.
;      ((insert ";")))))

;; -------------------------------------------------------------------
;;; K70 ARM M4-Cortex

;;;###autoload
(defun nvp-k70-help (manual)
  (interactive
   (let ((mans
          '(("K70 schematics" .
             "http://cache.freescale.com/files/32bit/hardware_tools/schematics/TWR-K70F120M-SCH_C.pdf")
            ("Cortex M4" .
             "http://infocenter.arm.com/help/topic/com.arm.doc.ddi0439b/DDI0439B_cortex_m4_r0p0_trm.pdf")
            ("Arm Mv7" .
             "http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0403e.b/index.html")
            ("J-Link" .
             "https://www.segger.com/downloads/jlink/UM08001_JLink.pdf")
            ("J-link Plugin" .
             "https://gnu-mcu-eclipse.github.io/debug/jlink/"))))
     (list (assoc-default (nvp-completing-read "Manual: " mans) mans))))
  (browse-url manual))

(provide 'nvp-asm)
;;; nvp-asm.el ends here
