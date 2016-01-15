#================smry_cat================#
print.smry_vec <- function(obj, ...) {
  x_name <- attr(obj, "x")$name
  digits <- attr(obj, "format")$digits
  printed_names <- attr(obj, "format")$printed_names

  out <- obj
  out[] <- lapply(1:ncol(out), function(i) roundf(out[, i], digits = digits[i], format = TRUE))
  out[out=="NA"] <- ""
  class(out) <- "data.frame"
  cat(x_name, "\n")
  print(out)
}


#================smry_cts================#
print.smry_cts <- function(obj, ...) {
  x_name <- attr(obj, "x")$x_name
  digits <- attr(obj, "digits")

  out <- obj
  out <- lapply(out, roundf, digits = digits)
  out <- do.call(rbind, out)
  colnames(out) <- ""
  names(dimnames(out)) <- c(x_name, "")
  print(noquote(out))
}


#================get_gof================#
print.get_gof <- function(obj, ...) {
  out <- matrix(unlist(obj), ncol = 1, dimnames = list(attr(obj, "print_names"), ""))
  class(out) <- "matrix"
  print(out)
}

#================get_coef================#
print.get_coef <- function(obj, ...) {
  coef <- obj$coef
  dispersion <- obj$dispersion
  names(coef) <- attr(coef, "print_names")
  class(coef) <- "data.frame"
  print(coef)
  cat(rep("-", 2^2), "\n", sep = "")
  cat("SIG. CODES: *** p < 0.001, ** p < 0.01, * p < 0.05 \n\n")
  cat(names(dispersion), ":", dispersion[[1]], "\n")
}


#================smry_mod================#
print.smry_mod <- function(obj, ...) {
  print_line <- function(chr, length) cat(rep(chr, length), "\n", sep = "")
  print_line("=", 2^5)
  cat("Model :", deparse(obj$call), "\n")
  print_line("-", 2^4)
  cat("Parameters :", "\n")
  print(obj$coef)
  print_line("-", 2^4)
  cat("Goodness of fit : \n")
  print(obj$gof)
  print_line("=", 2^5)
}

