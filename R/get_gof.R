#================Extract GOF measures from a model================#
get_gof <- function(mod, ...) {
  UseMethod("get_gof", mod)
}


#========lm========#
get_gof.lm <- function(mod, ...) {
  smry_mod <- summary(mod)

  out <- list(r_sq = smry_mod$r.squared, adj_r_sq = smry_mod$adj.r.squared, num_obs = nobs(mod))
  attr(out, "print_names") <- c("R^2", "Adj. R^2", "No. Obs.")
  attr(out, "decimal") <- c(TRUE, TRUE, FALSE)
  class(out) <- append("get_gof", class(out))
  return(out)
}


#========glm========#
get_gof.glm <- function(mod, ...) {
  smry_mod <- summary(mod)
  loglik_mod <- logLik(mod)

  out <- list(aic = AIC(mod), bic = BIC(mod), loglik = loglik_mod[1], dev = deviance(mod), df = attr(loglik_mod, "df"), num_obs = nobs(mod))
  attr(out, "print_names") <- c("AIC", "BIC", "Log likelihood", "Deviance", "DF", "No. obs.")
  attr(out, "decimal") <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  class(out) <- append("get_gof", class(out))
  return(out)
}


#========coxph========#
get_gof.coxph <- function(mod, ...) {
  smry_mod <- summary(mod)
  loglik_mod <- logLik(mod)
  ph_test <- survival::cox.zph(mod)$table["GLOBAL", "p"]

  out <- list(aic = AIC(mod), loglik = loglik_mod[1], df = attr(loglik_mod, "df"), num_obs = mod$n, num_events = mod$nevent, N.A. = length(mod$na.action), ph = ph_test)
  attr(out, "print_names") <- c("AIC", "Log likelihood", "DF", "No. obs.", "No. events", "N.A.", "PH test")
  attr(out, "decimal") <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  class(out) <- append("gof_smry", class(out))
  return(out)
}



