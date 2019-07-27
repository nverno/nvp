#!/usr/bin/awk -f

BEGIN { print "(" }
f {
  st = index($0, "="); 
  print; 
  f=0
  
}
/^# (environment|makefile)/{f=1}
END { print ")" }
