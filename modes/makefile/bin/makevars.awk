#!/usr/bin/awk -f

# convert output from 'make -pqrRns' to lisp-readable list:
# ((variables (<varname> [value] <type> [file] [lineno]))
#  (rules ())


BEGIN { print "(" }

/^# Variables/ { s = 1; print "(variables"; }
/^# Directories/ { s = 2; print ")"; }

# variable description (precedes value)
s==1 && match($0, /^# '?(override|environment|automatic|makefile|default)/, m) {
    var = 1; count++;
    file = "nil"; line = "nil"; type = "\""m[1]"\"";

    if (m[1] == "makefile") {
        match($0, /^# makefile \(from '([^']+)', line ([0-9]+)\)/, m);
        if (m[1])
            file = "\""m[1]"\""
        if (m[2])
            line = m[2]
    }
    next;
}

var == 1 {
    var = 0; value = "nil";
    gsub(/"/, "")       # remove quotes
    gsub(/\\/, "\\\\")  # escape escapes

    if (match($0, /^(\S+)\s*:?=\s*(.+)?\s*$/, m)) {
        if (m[2])
            value = "\""m[2]"\"";
        printf "(\"%s\" %s %s %s %s)\n", 
            m[1], value, type, file, line;
    } else {
        print "('!BAD!", "\""$0"\"";
    }

    type = ""; file = ""; line = "";
}

END { 
    print "))"
    print ";; variable count =", count 
}
