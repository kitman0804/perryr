roundf <- function(x, ...) {
  UseMethod("roundf", x)
}

roundf.default <- function(x, ...) {
  return(x)
}


roundf.numeric <- function(x, digits = 0, format = FALSE, noquote = TRUE, ...) {
  out <- sign(x) * trunc(abs(x) * 10^digits + 0.5) / (10^digits)
  if (format) {
    if (length(digits) == length(x)) {
      out <- sapply(1:length(x), function(i) {formatC(out[i], digits = digits[i], format = "f")})
    } else {
      out <- formatC(out, digits = digits[[1]], format = "f")
    }
    if (noquote) out <- noquote(out)
  }
  return(out)
}


roundf.matrix <- function(x, digits = 0, format = FALSE, noquote = TRUE, ...) {
  if (is.matrix(digits)) {
    if (!identical(dim(x), dim(digits))) stop("Dimension of 'x' amd 'digits' are not identical.")
  }
  out <- sign(x) * trunc(abs(x) * 10^digits + 0.5) / (10^digits)
  if (format) {
    if (length(digits) == length(x)) {
      out <- sapply(1:length(x), function(i) {formatC(out[i], digits = digits[i], format = "f")})
    } else {
      out <- formatC(out, digits = digits[[1]], format = "f")
    }
    out <- unlist(out)
    attributes(out) <- attributes(x)
    if (noquote) out <- noquote(out)
  }
  return(out)
}



