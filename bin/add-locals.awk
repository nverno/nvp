#!/usr/bin/awk -f

# Adds Local Variables: to end of files when not present

BEGIN { has_locals = 0; deleting = 0 }

$0 ~ /^;+[ \t]*(Local Variables)/ {
  if (has_locals) {
    deleting = 1
  } else {
    has_locals = 1
  }
}

!has_locals && $0 ~ /;;; [a-z-]+\.el ends here/ {
  $0 = ";; Local Variables:\n\
;; coding: utf-8\n\
;; indent-tabs-mode: nil\n\
;; End:\n\
" $0
}

!deleting || $0 ~ /;;; [a-z-]+\.el ends here/  { print $0 }
