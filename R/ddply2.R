ddply2 <- function(..., fill) {
  require(plyr)
  out <- dlply(...)
  by_vrbs <- attr(out, "split_labels")
  if (!is.data.frame(out[[1]])) out <- lapply(out, function(L) as.data.frame(matrix(L, nrow=1, dimnames=list(NULL, names(L))), stringsAsFactors=FALSE))
  out <- lapply(out, function(L) {names(L)[is.na(names(L))] <- "NA's"; L}) #Avoid NA in colnames
  if (length(out)>1) {
    out <- Reduce(rbind.fill, out)
    out <- cbind(by_vrbs, out)
  } else {
    out <- out[[1]]
  }
  if (!missing(fill)) {out[is.na(out)] <- fill}
  return(out)
}
