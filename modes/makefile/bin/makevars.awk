#!/usr/bin/awk -f

BEGIN { }

/^# Variables/ { s = 1 }
/^# Directories/ { s = 2 }

s==1 && match($0, /^# '?(override|environment|automatic|makefile|default)/, m) {
    var = 0; count++;
    lasttype = m[1];
    
    if (m[1] == "makefile") {
        match($0, /^# makefile \(from '([^']+)', line ([0-9]+)\)/, m);
        lastref = m[1] m[2];
        # print mvar[m[1],m[2]];
    }

#   st = index($0, "="); 
#   print; 
#   f=0
    var = 1;
    next;
}

var == 1 {
    var = 0;
    if (lasttype == "makefile") {
        if (lastref != "") {
            vars[lasttype, lastref] = $0;
        } else {
            vars[lasttype, $0] = $0;
        }
    } else {
        vars[lasttype, $0];
    }
    var = 0;
    lasttype = "";
    lastref = "";
}

END { 
    print "count =", count 
    for (v in vars) {
        print v, ":", vars[v];
    }
    print "#vars =", length(vars);
}
