smrytable <- function(x, ...) {
  require(plyr)
  UseMethod("smrytable", x)
}

smrytable.numeric <- function(x, by, x_name, ...) {
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)
  smry_x <- smry(x, x_name = x_name, ...)
  x_name <- attr(smry_x, "x")$name
  digits <- attr(smry_x, "format")$digits
  out <- smry_x
  out[] <- lapply(1:ncol(smry_x), function(i) roundf(smry_x[, i], digits = digits[i], format = TRUE))

  out <- c(paste0(out$mean, " (", out$sd, ")"), out$"NA's")
  names(out) <- c("mean (sd)", "No. of NA")
  class(out) <- "smrytable"
  attr(out, "x") <- list(name = x_name)
  return(out)
}


smrytable.factor <- function(x, by, valid = FALSE, x_name, ...) {
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)
  smry_x <- smry(x, x_name = x_name, ...)
  x_name <- attr(smry_x, "x")$name
  x_levels <- attr(smry_x, "x")$levels
  digits <- attr(smry_x, "format")$digits
  out <- smry_x
  out[] <- lapply(1:ncol(smry_x), function(i) roundf(smry_x[, i], digits = digits[i], format = TRUE))

  out <- c(paste0(out$count, " (", out$percent, ")"))
  names(out) <- x_levels
  class(out) <- "smrytable"
  attr(out, "x") <- list(name = x_name)
  return(out)
}


smrytable.character <- function(x, ...) {
  smrytable.factor(x, ...)
}


smrytable.integer <- function(x, ...) {
  smrytable.factor(x, ...)
}
