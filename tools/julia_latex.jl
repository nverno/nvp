#!/usr/bin/env julia

# #<marker at 29340 in julia-mode.el>
function generate_unicode(output, hash)

  if output == "abbrev"
    println("(define-abbrev-table 'julia-latex-abbrev-table")
    println("  '(")
  end

  for (k,v) in sort!(collect(Base.REPLCompletions.latex_symbols), by=x->x[2])
    ks = escape_string(k)
    vs = escape_string(v)

    if output == "hash"
      if ismatch(r"^\\U[0-9A-Fa-f]+$", vs)
        cp = vs[3:end]
        println("(let ((c (decode-char 'ucs #x$cp)))\n",
                "  (if c (puthash \"$ks\" (char-to-string c) $hash)))")
      else
        println("(puthash \"$ks\" \"$vs\" $hash)")
      end

    elseif output == "abbrev"
      if ! ismatch(r"^\\U[0-9A-Fa-f]+$", vs)
        println("    (\"$ks\" \"$vs\" nil :system t)")
      end
    else
        println("\"$ks\" \"$vs\"")
    end
  end

  if output == "abbrev"
    println("    )")
    println("  \"Julia latex abbrevs.\"")
    println("  :regexp \"\\\\(\\\\\\\\[\\\\A-Za-z0-9^]+\\\\)\")")
  end
end

if length(ARGS) < 2
  generate_unicode(ARGS[1], false)
else
  generate_unicode(ARGS[1], ARGS[2])
end
