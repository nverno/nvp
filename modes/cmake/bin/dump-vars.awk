#!/usr/bin/awk -f

function process_vars(cmd, langs, seen) {
  tmp = "cmake " cmd
  lang = ""
  for (l in langs) {
    if (length(lang) > 0)
      lang = lang "\n"
    lang = lang "\\1" langs[l] "\\2"
  }
  while ((tmp | getline var) > 0) {
    if (!seen[var]) {
      v = gensub(/(.*)<LANG>(.*)/, lang, 1, var)
      if (v !~ /<.*>/)
        print v
    }
    seen[var] = 1
  }
  close(tmp)
  return 0
}

BEGIN {
  langs[1] = "C"
  langs[2] = "CXX"

  pos = 0
  while (ARGV[++pos]) {
    # --lang=<lang> to add variables for <lang>, eg. CMAKE_<LANG>_COMPILER
    match(ARGV[pos], /^--lang(=(.*)?)?$/, group)
    if (RSTART) {
      delete ARGV[pos]
      if (group[2])
        langs[length(langs) + 1] = group[2]
      else {
        lang = ARGV[++pos]
        delete ARGV[pos]
      }
      continue
    }
    break
  }

  process_vars("--help-variable-list", langs, seen)
  process_vars("--help-property-list", langs, seen)
}
