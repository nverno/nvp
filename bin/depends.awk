#!/usr/bin/gawk -f

# What a terrible failure.
function wtf(text) {
    print "\33[1m\33[31m" text "\33[22m" > "/dev/stderr"
}

BEGIN {
    pos = 0
    while (ARGV[++pos]) {
        match(ARGV[pos], /^--output(=(.*)?)?$/, group)
        if (RSTART) {
            delete ARGV[pos]
            if (group[2])
                output = group[2]
            else {
                output = ARGV[++pos]
                delete ARGV[pos]
            }
            continue
        }

        match(ARGV[pos], /^--root(=(.*)?)?$/, group)
        if (RSTART) {
            delete ARGV[pos]
            if (group[2])
                root = group[2]
            else {
                root = ARGV[++pos]
                delete ARGV[pos]
            }
            continue
        }

        break
    }

    if (!output || !root) {
        wtf("need root and output")
        exit 1
    }
}

{print}

END {
    print "dumped to " output " (from " root ")"
}
