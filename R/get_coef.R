#================================================================#
#================Extract coefficients from a model================#
get_coef <- function(mod, ...) {
  UseMethod("get_coef", mod)
}

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

#================lm================#
get_coef.lm <- function(mod, ci_lvl = 0.95, add_ref = FALSE, trans_fun = identity, ...) {
  if (!is.function(trans_fun)) stop("'trans_fun' must be a function")
  smry_mod <- summary(mod)

  #========Estimation========#
  pval <- smry_mod$coefficients[, 4]
  sig <- 1 + (pval < 0.05) + (pval < 0.01) + (pval < 0.001)
  sig <- sapply(sig, switch, '0' = ' ', '1' = '*', '2' = '**', '3' = '***')
  ci <- confint(mod, level = ci_lvl)
  coef <- data.frame(smry_mod$coefficients[, 1:2], ci, pval, sig, stringsAsFactors = FALSE)
  names(coef) <- c("estimate", "se", "cil", "ciu", "pval", "sig")


  #========Add reference group========#
  xlevels <- mod$xlevels
  if (add_ref & length(xlevels) > 0) {
    ref <- lapply(1:length(xlevels), function(i) {paste0(names(xlevels)[i], xlevels[[i]][1], " (ref)")})
    coef_ref <- data.frame(rep(0, length(ref)), matrix(NA, nrow = length(ref), ncol = 4), rep(" ", length(ref)), stringsAsFactors = FALSE)
    rownames(coef_ref) <- ref
    colnames(coef_ref) <- colnames(coef)
    for (v in names(xlevels)) {
      loc <- min(grep(v, rownames(coef), fixed = TRUE))
      coef <- rbind(coef[1:(loc - 1), ], coef_ref, coef[loc:nrow(coef), ])
    }
  }

  #========Transformation========#
  if (!identical(trans_fun, identity)) {
    trans_coef <- trans_fun(coef[, c("estimate", "cil", "ciu")])
    colnames(trans_coef) <- paste0("trans_", names(trans_coef))
    coef <- cbind(coef[, 1:4], trans_coef, coef[, -(1:4)])
  }

  attr(coef, "ci_level") <- ci_lvl
  attr(coef, "trans_fun") <- substitute(trans_fun)
  attr(coef, "print_names") <- names(coef)
  attr(coef, "print_names") <- gsub("ci", paste0(attr(coef, "ci_level") * 100, "% ci"), attr(coef, "print_names"))
  attr(coef, "print_names") <- gsub("se", "std. error", attr(coef, "print_names"))
  attr(coef, "print_names") <- gsub("trans_(.*)", paste0(substitute(trans_fun), "(\\1)"), attr(coef, "print_names"))

  dispersion <- smry_mod[names(smry_mod) %in% c("sigma", "dispersion")]

  out <- list(coef = coef, dispersion = dispersion)
  class(out) <- append("get_coef", class(out))
  return(out)
}


#================glm================#
get_coef.glm <- function(mod, ...) {
  get_coef.lm(mod, ...)
}



#================coxph================#
get_coef.coxph <- function(mod, ci_lvl = 0.95, ...) {
  smry_mod <- summary(mod)
  ci <- confint(mod, level = ci_lvl)

  if (is.null(mod$naive.var)) {
    coef <- cbind(smry_mod$coefficients[, c(1, 3)], ci)
    pval <- smry_mod$coefficients[, 4]
  } else {
    coef <- cbind(smry_mod$coefficients[, c(1, 4)], ci)
    pval <- smry_mod$coefficients[, 6]
  }
  sig <- 1 + (pval < 0.05) + (pval < 0.01) + (pval < 0.001)
  sig <- sapply(sig, switch, '0' = ' ', '1' = '*', '2' = '**', '3' = '***')
  coef <- data.frame(coef, exp(coef[, -2]), pval, sig)
  names(coef) <- c("estimate", "se", "cil", "ciu", "hr", "hr_cil", "hr_ciu", "pval", "sig")

  out <- coef
  attr(out, "ci_level") <- ci_lvl
  attr(out, "print_names") <- gsub("ci", paste0(attr(out, "ci_level") * 100, "%ci"), names(out))
  class(out) <- append("get_coef", class(out))
  return(out)
}





