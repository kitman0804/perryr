#================================================================#

# This function is a generic function to create format_df

#================================================================#
format_df <- function(x, ...) {
  UseMethod("format_df", x)
}

#================#
# default
format_df.default <- function(x, ..., printed_names, digits) {
  out <- data.frame(x, ...)
  if (missing(printed_names)) {
    printed_names <- list(row = row.names(x), col = names(x))
  }
  if (missing(digits)) {
    digits <- 2 * sapply(x, inherits, "numeric")
  }
  out <- format_df(out, printed_names = printed_names, digits = digits, ...)
  return(out)
}

#================#
# data.frame
format_df.data.frame <- function(x, printed_names, digits, ...) {
  if (missing(printed_names)) {
    printed_names <- list(row = row.names(x), col = names(x))
  }
  if (missing(digits)) {
    digits <- 2 * sapply(x, inherits, "numeric")
  }
  out <- x
  class(out) <- append("format_df", class(out))
  attr(out, "format") <- list(digits = digits,
                              printed_names = printed_names)
  return(out)
}


#================================#
is.format_df <- function(x) {
  inherits(x, "format_df")
}

#================================#
print.format_df <- function(obj) {
  out <- obj
  for (i in 1:length(obj)) {
    out[[i]] <- roundf(out[[i]], attr(obj, "format")$digits[i], format = TRUE)
  }
  out <- do.call(data.frame, out)
  row.names(out) <- attr(obj, "format")$printed_names[[1]]
  names(out) <- attr(obj, "format")$printed_names[[2]]

  print(out)
}


