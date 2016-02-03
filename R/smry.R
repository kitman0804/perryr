#================================================================#

# This function is a generic function to extract important summary of the input 'x'

#================================================================#
# S3 class
smry <- function(x, ...) {
  UseMethod("smry", x)
}


#================================#
# default class
# - summary(x)

smry.default <- function(x, ...) {
  summary(x)
}


#================================#
# numeric class
# - mean
# - sd
# - no. of missing
# - quantiles

smry.numeric <- function(x, qt = c(0.0, 0.5, 1.0), digits = 2, x_name, ...) {
  # argument checking
  if (!is.numeric(qt)) stop("'qt' must be numeric")
  if (sum(qt < 0) > 0 | sum(qt > 1) > 0) stop("'qt' must be value(s) in [0, 1]")
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)

  # mean & s.d.
  out <- data.frame(
    variable = x_name,
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
  # quantile
  out <- data.frame(
    out,
    as.list(quantile(x, probs = qt, na.rm = TRUE))
  )
  names(out)[(ncol(out) - length(qt) + 1):ncol(out)] <- paste0("q", qt * 100)
  # no. of missing
  out$na <- sum(is.na(x))

  # output
  digits <- digits[[1]] * c(rep(1, ncol(out) - 1), 0)
  printed_names <- names(out)
  printed_names <- gsub("sd", "s.d.", printed_names)
  printed_names <- gsub("q([0-9]+)", "\\1\\% quantile", printed_names)
  printed_names <- gsub("na", "No. of NA's", printed_names)
  out <- format_df(out, printed_names = list(NULL, printed_names), digits = digits)
  return(out)
}




#================================#
# factor
# - count
# - percentage
# - valid percentage (percentage after excluding NA)

smry.factor <- function(x, digits = 2, x_name, ...) {
  x <- droplevels(x)
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)

  # frequency & percentage
  freq <- summary(x)
  out <- data.frame(
    variable = c(x_name, rep("", length(freq) - 1)),
    level = ifelse(nchar(names(freq)) <= 60, names(freq), paste0(substr(names(freq), 1, 60), " ...")),
    freq = freq
  )
  out$freq <- summary(x)
  out$percent <- with(out, freq/sum(freq) * 100)
  # valid percentage
  if ("NA's" %in% rownames(out)) {
    out$valid_percent <- with(out, c(freq[-length(freq)]/sum(freq[-length(freq)]), NA)) * 100
  } else {
    out$valid_percent <- out$percent
  }

  # output
  digits <- digits[[1]] * c(rep(0, 3), rep(1, ncol(out) - 3))
  printed_names <- names(out)
  printed_names <- gsub("freq", "n", printed_names)
  printed_names <- gsub("percent", "\\%", printed_names)
  printed_names <- gsub("_", " ", printed_names)
  out <- format_df(out, printed_names = list(NULL, printed_names), digits = digits)
  return(out)
}




#================================#
# character
# - (Same as factor)
smry.character <- function(x, x_name, ...) {
  # Covert 'x' to factor
  x <- factor(x)
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)

  # smry
  smry.factor(x, x_name = x_name, ...)
}


#================================#
# integer
# - (Same as factor)
smry.integer <- function(x, x_name, ...) {
  # Covert 'x' to factor
  x <- factor(x)
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)

  # smry
  smry.factor(x, x_name = x_name, ...)
}


#================================#
# logical
# - (Same as factor)
smry.logical <- function(x, x_name, ...) {
  # Covert 'x' to factor
  x <- factor(x)
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)

  # smry
  smry.factor(x, x_name = x_name, ...)
}


#================================#
# lm
# - coefficients
# - goodness of fit summary
smry.lm <- function(x, ...) {
  out <- list(call = x$call, coef = get_coef(x, ...), gof = get_gof(x, ...))

  # Assign class
  class(out) <- append("smry_mod", class(out))
  return(out)
}


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


#================================#
# glm
# - coefficients
# - goodness of fit summary
smry.glm <- function(x, ...) {
  out <- list(call = x$call, coef = get_coef(x, ...), gof = get_gof(x, ...))

  # Assign class
  class(out) <- append("smry_mod", class(out))
  return(out)
}


#================================#
# coxph
# - coefficients
# - goodness of fit summary
smry.coxph<- function(x, ...) {
  out <- list(call = x$call, coef = get_coef(x, ...), gof = get_gof(x, ...))

  # Assign class
  class(out) <- append("smry_mod", class(out))
  return(out)
}


#================================#
# survfit
smry.survfit <- function(x, ...) {
  out <- x

  # Assign class
  class(out) <- "smry_survfit"
  return(out)
}





#================list================#
smry.list <- function(x, ...) {
  vnames <- names(x)
  out <- lapply(vnames, function(v) {
    smry(x[[v]], ..., x_name = v)
  })
  names(out) <- vnames

  # Assign class
  class(out) <- append("smry_ls", class(out))
  return(out)
}

# printed format
print.smry_ls <- function(obj, ...) {
  x_names <- lapply(obj, function(obj_i) {
    attr(obj_i, "x")$name
  })
  len <- lapply(obj, length)
  len <- max(unlist(len))

  out <- lapply(obj, function(obj_i) {
    c(paste0(names(obj_i), " : ", obj_i), rep("", len - length(obj_i)))
  })
  out <- do.call(cbind, out)
  dimnames(out) <- list(rep("", len), x_names)
  class(out) <- "table"

  print(out)
}



#================data.frame================#
smry.data.frame <- function(x, ...) {
  vnames <- names(x)
  out <- lapply(vnames, function(v) {
    smry(x[[v]], ..., x_name = v)
  })
  names(out) <- vnames

  # Assign class
  class(out) <- append("smry_ls", class(out))
  return(out)
}
