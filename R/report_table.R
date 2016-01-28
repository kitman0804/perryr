#================================#

# This function is for creating tables of statistical summary for report

#================================#
report_table <- function(x, ...) {
  UseMethod("report_table", x)
}


#================================#
# default
report_table.default <- function(x) {
  stop("'x' must be either a data.frame")
}


#================================#
# data.frame
report_table.data.frame <- function(x, format, digits = 1, header) {
  # argument
  if (missing(format)) {
    format <- names(x)
    if (missing(header)) {
      header <- names(x)
    }
  } else {
    if (missing(header)) {
      header <- format
    } else if (length(header)!=length(format)) {
      stop("Length of 'header' does not match length of 'format'")
    }
  }
  if (length(digits)!=ncol(x)) {
    digits <- rep(digits[[1]], ncol(x))
  }
  # rounding
  x_f <- x
  x_f[] <- lapply(1:ncol(x), function(i) roundf(x[, i], digits = digits[i], format = TRUE))
  # colnames of input x
  vnames <- names(x)
  vnames <- vnames[order(nchar(vnames), decreasing = TRUE)]
  # replacement
  out <- matrix(format, nrow = nrow(x), ncol = length(format), byrow = TRUE, dimnames = list(NULL, NULL))
  for (i in 1:nrow(out)) {
    for (j in 1:ncol(out)) {
      for (k in vnames) {
        out[i, j] <- gsub(k, x_f[i, k], out[i, j], fixed = TRUE)
      }
    }
  }
  colnames(out) <- header
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  return(out)
}

