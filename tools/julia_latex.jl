#!/usr/bin/env julia

# #<marker at 29340 in julia-mode.el>
function generate_unicode(sexp, hash)
  for (k,v) in sort!(collect(Base.REPLCompletions.latex_symbols),
                     by=x->x[2])
    ks = escape_string(k)
    vs = escape_string(v)
    if sexp
      if ismatch(r"^\\U[0-9A-Fa-f]+$", vs)
        cp = vs[3:end]
        println("(let ((c (decode-char 'ucs #x$cp)))\n",
                "  (if c (puthash \"$ks\" (char-to-string c) $hash)))")
      else
        println("(puthash \"$ks\" \"$vs\" $hash)")
      end
    else
      println("\"$ks\" \"$vs\"")
    end
  end
end

if length(ARGS) < 2
  generate_unicode(false, false)
else
  generate_unicode(false, ARGS[2])
end
