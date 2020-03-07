
;; -------------------------------------------------------------------
;;; Glyphs 

(defun nvp-font-quote-glyphs ()
  (let ((tbl (make-display-table)))
    (aset tbl 8220 (vector (make-glyph-code ?\" 'default)))
    (aset tbl 8221 (vector (make-glyph-code ?\" 'default)))
    (aset tbl 8216 (vector (make-glyph-code ?\` 'default)))
    (aset tbl 8217 (vector (make-glyph-code ?\' 'default)))
    (setq standard-display-table tbl)))

(defun nvp-font-glyphify (item glyph)
  `((,item
     (0 font-lock-keyword-face t)
     (0 (prog1
            (compose-region (match-beginning 0)
                            (match-end 0)
                            ,glyph)
          nil)))))
