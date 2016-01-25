#================================#

# This function is for creating tables of statistical summary for report

#================================#
smry_format <- function(x, ...) {
  UseMethod("smry_format", x)
}

#================================#
# default
smry_format.default <- function(x) {
  stop("'x' must be either a data.frame")
}


#================================#
# data.frame
smry_format.data.frame <- function(x, format) {
  x <- x
  attrs <- attributes(x)
  x[] <- lapply(1:ncol(x), function(i) roundf(x[, i], digits = attrs$format$digits[i], format = TRUE))

  selected <- sapply(names(x), grepl, x = format)
  selected <- names(x)[selected]
  selected <- selected[order(nchar(selected), decreasing = TRUE)]

  out <- sapply(1:nrow(x), function(i) {
    txt <- format
    for (j in selected) {
      txt <- gsub(j, x[i, j], txt)
    }
    return(txt)
  })
  out <- cbind(label = attrs$row.names, out)
  out <- as.data.frame(out)
  return(out)
}

