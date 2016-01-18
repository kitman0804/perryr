#================smry_cts================#
print.smry_cts <- function(obj, ...) {
  x_name <- attr(obj, "x")$name
  digits <- attr(obj, "format")$digits
  printed_names <- attr(obj, "format")$printed_names

  out <- obj
  out[] <- lapply(1:ncol(out), function(i) roundf(out[, i], digits = digits[i], format = TRUE))
  out <- rbind(printed_names, do.call(cbind, out))
  out <- cbind(c("", x_name), out)

  dimnames(out) <- list(rep("", nrow(out)), rep("", ncol(out)))
  out[grepl("^\\s*NA\\s*$", out)] <- ""
  print(noquote(out))
}


#================smry_cat================#
print.smry_cat <- function(obj, ...) {
  x_name <- attr(obj, "x")$name
  x_levels <- attr(obj, "x")$levels
  digits <- attr(obj, "format")$digits
  printed_names <- attr(obj, "format")$printed_names

  out <- obj
  out[] <- lapply(1:ncol(out), function(i) roundf(out[, i], digits = digits[i], format = TRUE))
  out <- rbind(printed_names, do.call(cbind, out))
  out <- cbind(c(x_name, x_levels), out)

  dimnames(out) <- list(rep("", nrow(out)), rep("", ncol(out)))
  out[grepl("^\\s*NA\\s*$", out)] <- ""
  print(noquote(out))
}


#================smrytable================#
print.smrytable <- function(obj, ...) {
  x_name <- attr(obj, "x")$name
  out <- cbind(names(obj), obj)
  out <- rbind(c(x_name, ""), out)
  dimnames(out) <- list(rep("", nrow(out)), rep("", ncol(out)))
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

