smry <- function(x, ...) {
  UseMethod("smry", x)
}


#================default================#
smry.default <- function(x, ...) {
  summary(x)
}


#================numeric================#
smry.numeric <- function(x, qt=c(0, .5, 1), digits = 2, x_name, ...) {
  out <- data.frame(
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE),
    as.list(quantile(x, probs = qt, na.rm = TRUE)),
    sum(is.na(x))
  )
  names(out) <- c("mean", "sd", paste0("q", qt * 100), "NA's")
  rownames(out) <- ""

  digits <- digits[[1]] * c(rep(1, ncol(out) - 1), 0)
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)
  class(out) <- c("smry_cts", class(out))
  attr(out, "x") <- list(name = x_name, levels = NULL)
  attr(out, "format") <- list(digits = digits, printed_names = c("mean", "s.d.", paste0(qt * 100, "% quantile"), "No. of NA"))
  return(out)
}


#================factor================#
smry.factor <- function(x, digits = 2, x_name, ...) {
  x <- droplevels(x)

  out <- data.frame(count = summary(x))
  out$percent <- with(out, count/sum(count) * 100)
  if ("NA's" %in% rownames(out)) {
    out$valid_percent <- with(out, c(count[-length(count)]/sum(count[-length(count)]), NA)) * 100
  } else {
    out$valid_percent <- out$percent
  }

  digits <- digits[[1]] * c(0, 1, 1)
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)
  class(out) <- c("smry_cat", class(out))
  attr(out, "x") <- list(name = x_name, levels = rownames(out))
  attr(out, "format") <- list(digits = digits, printed_names = c("n", "%", "valid %"))
  return(out)
}


#================character================#
smry.character <- function(x, x_name, ...) {
  x <- factor(x, exclude = NULL)
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)
  smry.factor(x, x_name = x_name, ...)
}


#================integer================#
smry.integer <- function(x, x_name, ...) {
  x <- factor(x, exclude = NULL)
  x_name <- ifelse(missing(x_name), deparse(as.list(match.call())$x), x_name)
  smry.factor(x, x_name = x_name, ...)
}


#================lm================#
smry.lm <- function(x, ...) {
  out <- list(call = x$call, coef = get_coef(x, ...), gof = get_gof(x, ...))
  class(out) <- append("smry_mod", class(out))
  return(out)
}


#================glm================#
smry.glm <- function(x, ...) {
  out <- list(call = x$call, coef = get_coef(x, ...), gof = get_gof(x, ...))
  class(out) <- append("smry_mod", class(out))
  return(out)
}


#================coxph================#
smry.coxph<- function(x, ...) {
  out <- list(call = x$call, coef = get_coef(x, ...), gof = get_gof(x, ...))
  class(out) <- append("smry_mod", class(out))
  return(out)
}


#================survfit================#
smry.survfit <- function(x, ...) {

  out <- x

  class(out) <- "smry_survfit"
  return(out)
}





#================list================#
smry.list <- function(x, ...) {
  vnames <- names(x)
  out <- lapply(vnames, function(v) {
    with(x, smry(x[[v]], ..., x_name = v))
  })
  names(out) <- vnames
  class(out) <- "smry_ls"
  return(out)
}


#================data.frame================#
smry.data.frame <- function(x, ...) {
  vnames <- names(x)
  out <- lapply(vnames, function(v) {
    with(x, smry(x[, v], ..., x_name = v))
  })
  names(out) <- vnames
  class(out) <- "smry_ls"
  return(out)
}
