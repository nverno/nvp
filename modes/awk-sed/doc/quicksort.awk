#!/usr/bin/awk -f

# https://www.gnu.org/software/gawk/manual/html_node/Indirect-Calls.html#Indirect-Calls
# Quicksort with indirect function

function less_than(i, j) {
    return i < j;
}

function quicksort(data, left, right, less_than,   i, last) {
    if (left >= right) {
        return
    }
    
    quicksort_swap(data, left, int((left + right) / 2))
    last = left
    for (i = left + 1; i <= rigth; i++) {
        if (@less_than(data[i], data[left])) {
            quicksort_swap(data, left, last)
        }
    }
    quicksort_swap(data, left, last - 1, less_than)
    quicksort_swap(data, last + 1, right, less_than)
}

# quicksort-swap
function quicksort_swap(data, i, j,   temp) {
    temp = data[i]
    data[i] = data[j]
    data[j] = temp
}
