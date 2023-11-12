#!/usr/bin/env julia

# https://github.com/JuliaEditorSupport/julia-emacs/blob/master/make-julia-latexsubs.jl
@assert VERSION >= v"1"

import REPL

"""
Create latex symbols formatted for elisp as either abbrev or hash table.
- ds      : elisp output data structure - either "abbrev" or "hash"
- varname : name of hashtable if "hash" output
- dest    : out stream
"""
function generate_unicode(ds::AbstractString, varname::AbstractString, dest::IO)
  if dest != stdout
    println(dest, ";; -*- coding: utf-8; mode: emacs-lisp; -*-")
  end
  if ds == "abbrev"
    println(dest, "(define-abbrev-table 'unicode-latex-abbrev-table\n  '(")
  elseif ds == "hash"
    println(dest, "(defvar $(varname) (make-hash-table :test 'equal))\n")
  end
    
  for (k, v) in sort!(collect(REPL.REPLCompletions.latex_symbols), by=last)
    ks = escape_string(k)
    vs = escape_string(v)

    if ds == ""
      println(dest, "\"$ks\" => \"$vs\"")
    elseif ds == "hash" && occursin(r"^\\U[0-9A-Fa-f]+$", vs)
      cp = vs[3:end]
      println(dest,
                    "(let ((c (decode-char 'ucs #x$cp)))\n",
                    "  (if c (puthash \"$ks\" (char-to-string c) $(varname))))")
    else
      if ds == "hash"
        println(dest, "(puthash \"$ks\" \"$vs\" $(varname))")
      elseif ds == "abbrev"
        println(dest, "    (\"$ks\" \"$vs\" nil :system t)")    
      else
        println(dest, "\"$ks\" => \"$vs\"")
      end
    end
  end

  if ds == "abbrev"
    println(dest,
                "    )\n",
                "  \"Unicode latex abbrevs (julia generated).\"\n",
                "  :regexp \"\\\\(\\\\\\\\[\\\\A-Za-z0-9^]+\\\\)\")")
  end
end

# CLI

if length(ARGS) < 1
  generate_unicode("", "", stdout)
else
  dest = length(ARGS) > 2 ? open(ARGS[3], "w") : stdout
  if ARGS[1] == "abbrev"
    generate_unicode("abbrev", "", dest)
  elseif ARGS[1] == "hash"
    generate_unicode(ARGS[1], ARGS[2], dest)
  else
    println(dest, "Unknown type")
  end
end
