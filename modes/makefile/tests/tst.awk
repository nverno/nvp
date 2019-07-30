#!/usr/bin/awk -f

BEGIN { }

/^# Variables/ { s = 1 }
/^# Directories/ { s = 2}

s==1 && /^# '?(override|environment|automatic|makefile|default)/ {
    count++;
    ANSI
    vars[$1]
    print;
# f {
#   st = index($0, "="); 
#   print; 
#   f=0
}

# /^# (environment|makefile)/{f=1}
END { print "count =", count }
