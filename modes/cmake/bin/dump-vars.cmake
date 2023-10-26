# Dump alist of variables with values from generated cmake files
get_cmake_property(variables VARIABLES)
message("(")
foreach (variable ${variables})
  message("(\"${variable}\" \"${${variable}}\")")
endforeach()
message(")")
