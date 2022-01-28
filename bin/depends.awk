#!/usr/bin/gawk -f

# Create .depends for *.el files
# only adds dependencies for libraries found in current search,
# the rest are listed, but commented out as non-local depends

# What a terrible failure.
function wtf(text) {
    print "\33[1m\33[31m" text "\33[22m" > "/dev/stderr"
}

BEGIN {
    pos = 0; status = 0
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

    # if (!output || !root) {
    #     wtf("--root and --output required")
    #     status = 1
    #     exit status
    # }
}

BEGINFILE { len = 0 }

/^\s*;/ { next }

match($0, /\(require\s+'([[:alnum:]-]+)\s*("[^"]+")?\)/, l) {
    deps[FILENAME,len++] = l[1]
    next
}

# nvp:req macros: (nvp:req 'feature '[subr|macro|etc])
match($0, /\(nvp:req\s+'?([[:alnum:]-]+)\s*['"]?([[:alpha:]]+)\)/, l) {
    deps[FILENAME,len++] = sprintf("%s%s", l[1], l[2] ? "-" l[2] : "");
    next
}

match($0, /\(provide\s+'([[:alnum:]-]+)\)/, p) {
    libs[p[1]] = FILENAME
    next
}

END {
    if (status) exit status

    for (dep in deps) {
        split(dep, pair, SUBSEP)
        if (libs[deps[dep]])
            print pair[1] "c: " libs[deps[dep]]
        else
            # non-local dependencies
            print "# " pair[1] "c: " deps[dep]
    }
}
