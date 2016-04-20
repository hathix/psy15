#
# Utility functions.
#

# better version of paste() that concatenates strings without putting a space in betwen
p <- function(..., sep='') {
  paste(..., sep=sep, collapse=sep)
}

# maps a vector/list to a vector
# capply(vector, function)
capply <- function(v, f) {
    return(unlist(lapply(v, f)))
}
