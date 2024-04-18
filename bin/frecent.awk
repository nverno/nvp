#!/usr/bin/gawk -f
# 
# Read frecent directories from z.sh database
#
function frecent(rank, time, current) {
  dx = current - time
  return int(10000 * rank * (3.75/((0.0001 * dx + 1) + 0.25)))
}

BEGIN {
  "date +%s" | getline current
  if (frecency == "") frecency = 9000
}

$2 > 1 {
  if (frecent($2, $3, current) > frecency)
    print $1
}
