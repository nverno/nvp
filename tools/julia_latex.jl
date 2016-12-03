#!/usr/bin/env julia

# #<marker at 29340 in julia-mode.el>
for (k,v) in sort!(collect(Base.REPLCompletions.latex_symbols),
                   by=x->x[2])
  ks = escape_string(k)
  vs = escape_string(v)
  if ismatch(r"^\\U[0-9A-Fa-f]+$", vs)
    cp = vs[3:end]
    println("(let ((c (decode-char 'ucs #x$cp)))\n",
            "  (if c (puthash \"$ks\" (char-to-string c) julia-latexsubs)))")
  else
    println("(puthash \"$ks\" \"$vs\" julia-latexsubs)")
  end
end
