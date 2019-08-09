#!/usr/bin/awk -f

# convert output from 'make -pqrRns' to lisp-readable list:
# ((variables (<varname> [value] <type> [file] [lineno]))
#  (rules ())


BEGIN { print "(" }

/^# Variables/ { s = 1; print "(variables"; }
/^# Directories/ { s = 2; print ")"; }
/^# Implicit Rules/ { s = 3; print "(rules"; }

# Variables
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
    gsub(/"/, "")                   # remove quotes
    gsub(/\\/, "\\\\")              # escape escapes

    if (match($0, /^(\S+)\s*:?=\s*(.+)?\s*$/, m)) {
        if (m[2]) {
            sub(/\s*$/, "", m[2]);  # trailing whitespace
            value = "\""m[2]"\"";
        }
        printf "(\"%s\" %s %s %s %s)\n", 
            m[1], value, type, file, line;
    } else if (match($0, /^define (\S+)/, m)) {
        printf "(\"%s\" \"<define>\" %s %s %s)\n",
            m[1], type, file, line;
    } else {
        print "('!BAD!", "\""$0"\"";  # parse error
    }

    type = ""; file = ""; line = "";
}

# Rules
s == 3 && $0 ~ /# Not a target:/ { ignore = 1; next; }
s == 3 && ignore == 1 { ignore = 0; next; }
s == 3 && match($1, /^([^#]+):/, m) {
    in_rule = 1;
    # previous rule had no location info, so print it here
    if (rule)
        printf "(\"%s\" nil nil)\n", rule;
    rule = m[1];
}

in_rule == 1 && match($0, /from '(.+)', line ([0-9]+)/, m) {
    printf "(\"%s\" \"%s\" %s)\n", rule, m[1], m[2];
    rule = ""; in_rule = 0;
}

END { 
    if (rule)
        printf "(\"%s\" nil nil)\n", rule;
    print "))"
    print ";; variable count =", count 
}
