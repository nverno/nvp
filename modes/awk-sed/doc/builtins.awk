#!/usr/bin/awk -f

BEGIN {
  print ";; -*- lexical-binding: t -*-"
  print "("
}

/builtInSymbols/ {
  s = 1
  next
}

/\s*name:/ && s {
  match($0, /"[^\"]+"/, a)
  print "(" a[0]
}

/\s*parameters:/ && s {
  pars = substr($0, index($0, ":") + 1)
  gsub(/[ \[\],]/, "", pars)
  print ":param (" pars ")"
}

/\s*description:/ && s {
  gsub(/\\"/, "'", $0)
  match($0, /"[^\"]+"/, a)
  print ":desc", a[0] ")"
}

END { print ")" }
