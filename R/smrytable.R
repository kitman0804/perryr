
#================================#
smrytable <- function(x, ...) {
  require(plyr)
  UseMethod("smrytable", x)
}

#================================#
# printed format
print.smrytable <- function(obj, ...) {
  x_name <- attr(obj, "x")$name
  out <- cbind(names(obj), obj)
  out <- rbind(c(x_name, ""), out)
  dimnames(out) <- list(rep("", nrow(out)), rep("", ncol(out)))
  print(noquote(out))
}

#================================#
# numeric
smrytable.numeric <- function(x, by_vrbs, x_name, ...) {
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)

  cnames <- c("all")
  smry_x <- list(all = smry(x, x_name = x_name, ...))
  if (!missing(by_vrbs)) {
    # argument checking (x & by_vrbs)
    if (is.vector(by_vrbs) | is.factor(by_vrbs)) {
      if (length(x) != length(by_vrbs)) stop("")
    } else if (is.matrix(by_vrbs) | is.data.frame(by_vrbs)) {
      if (length(x) != nrow(by_vrbs)) stop("")
    } else {
      stop("'by_vrbs' must be a vector, matrix or data.frame")
    }
    tmpdf <- data.frame(x = x, by_vrbs)
    smry_by <- dlply(tmpdf, .variables = names(tmpdf)[-1], function(d) smry(d$x), ...)
    cnames <- c(cnames, unlist(attr(smry_by, "split_labels")))
    smry_x <- c(smry_x, smry_by)
  }
  out <- lapply(smry_x, smry_format, format = "mean (sd)")
  out[] <- lapply(1:length(cnames), function(i) {
    colnames(out[[i]])[2] <- cnames[i]
    return(out[[i]])
  })
  out <- Reduce(function(...) merge(..., by = "label", all = TRUE), out)
  out <- noquote(out)
  return(out)
}

#================================#
# factor
smrytable.factor <- function(x, by_vrbs, x_name, ...) {
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)

  cnames <- c("all")
  smry_x <- list(smry(x, x_name = x_name, ...))
  if (!missing(by_vrbs)) {
    # argument checking (x & by_vrbs)
    if (is.vector(by_vrbs) | is.factor(by_vrbs)) {
      if (length(x) != length(by_vrbs)) stop("")
    } else if (is.matrix(by_vrbs) | is.data.frame(by_vrbs)) {
      if (length(x) != nrow(by_vrbs)) stop("")
    } else {
      stop("'by_vrbs' must be a vector, matrix or data.frame")
    }
    tmpdf <- data.frame(x = x, by_vrbs)
    smry_by <- dlply(tmpdf, .variables = names(tmpdf)[-1], function(d) smry(d$x), ...)
    cnames <- c(cnames, unlist(attr(smry_by, "split_labels")))
    smry_x <- c(smry_x, smry_by)
  }
  out <- lapply(smry_x, smry_format, format = "count (percent)")
  out[] <- lapply(1:length(cnames), function(i) {
    colnames(out[[i]])[2] <- cnames[i]
    return(out[[i]])
  })
  out <- Reduce(function(...) merge(..., by = "label", all = TRUE), out)
  out <- noquote(out)
  return(out)
}

#================================#
# character
smrytable.character <- function(x, ...) {
  smrytable.factor(x, ...)
}

#================================#
# integer
smrytable.integer <- function(x, ...) {
  smrytable.factor(x, ...)
}
