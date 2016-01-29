
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
smrytable.numeric <- function(x, by_vrbs, format = "mean (sd)", x_name, na = "", ...) {
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
      stop("'by_vrbs' must be a vector, factor, matrix or data.frame")
    }
    tmpdf <- data.frame(x = x, by_vrbs)
    smry_by <- dlply(tmpdf, .variables = names(tmpdf)[-1], function(d, ...) {
      smry(d$x, x_name = x_name, ...)
    })
    cnames <- c(cnames, names(smry_by))
    smry_x <- c(smry_x, smry_by)
  }

  out <- lapply(1:length(smry_x), function(i) {
    s <- smry_x[[i]]
    tbl <- report_table(s, format, digits = attr(s, "format")$digits, header = cnames[i])
    tbl <- data.frame(variable = "mean (sd)", tbl, stringsAsFactors = FALSE)
    vrb_name <- data.frame(s$variable[[1]], rep("", ncol(tbl) - 1), stringsAsFactors = FALSE)
    names(vrb_name) <- names(tbl)
    tbl <- rbind(vrb_name, tbl)
    colnames(tbl)[1] <- "variable"
    return(tbl)
  })
  out <- Reduce(function(...) merge(..., by = "variable", all = TRUE, sort = FALSE), out)
  out[] <- lapply(out, as.character)
  out[is.na(out)] <- na
  return(out)
}


#================================#
# factor
smrytable.factor <- function(x, by_vrbs, format = "freq (valid_percent)", x_name, na = "", ...) {
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
      stop("'by_vrbs' must be a vector, factor, matrix or data.frame")
    }
    tmpdf <- data.frame(x = x, by_vrbs)
    smry_by <- dlply(tmpdf, .variables = names(tmpdf)[-1], function(d, ...) {
      smry(d$x, x_name = x_name, ...)
    })
    cnames <- c(cnames, names(smry_by))
    smry_x <- c(smry_x, smry_by)
  }

  out <- lapply(1:length(cnames), function(i) {
    s <- smry_x[[i]]
    tbl <- report_table(s, format = format, digits = attr(s, "format")$digits, header = cnames[i])
    tbl <- data.frame(variable = s$level, tbl, stringsAsFactors = FALSE)
    vrb_name <- data.frame(s$variable[[1]], rep("", ncol(tbl) - 1), stringsAsFactors = FALSE)
    names(vrb_name) <- names(tbl)
    tbl <- rbind(vrb_name, tbl)
    return(tbl)
  })
  out <- Reduce(function(...) merge(..., by = "variable", all = TRUE, sort = FALSE), out)
  out[] <- lapply(out, as.character)
  out[is.na(out)] <- na
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


#================================#
# logical
smrytable.logical <- function(x, ...) {
  smrytable.factor(x, ...)
}


#================================#
# data.frame
smrytable.data.frame <- function(x, by_vrbs, na, ...) {
  if (missing(by_vrbs)) {
    out <- lapply(1:length(x), function(i, ...) {
      smrytable(x[[i]], x_name = names(x)[i])
    })
  } else {
    if (is.character(by_vrbs)) {
      by_vrbs <- x[, by_vrbs, drop = FALSE]
      x <- x[, !(names(x) %in% names(by_vrbs))]
    }
    out <- lapply(1:length(x), function(i, ...) {
      smrytable(x[[i]], by_vrbs = by_vrbs, x_name = names(x)[i])
    })
  }
  out[] <- lapply(1:length(out), function(i) {
    rbind(out[[i]], "")
  })
  out <- do.call(rbind, out)
  out[is.na(out)] <- ifelse(missing(na), "", na)
  row.names(out) <- NULL
  return(out)
}



