#ref: https://tomizonor.wordpress.com/2013/04/17/file-utf8-windows/
#ref: http://stackoverflow.com/questions/16028658/unicode-conversion-and-export-in-r

#========readUTF8========#
if (!"stringr"%in%rownames(installed.packages())) install.packages("stringr")

readUTF8 <- function(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE) {
  x <- readLines(file)
  x <- stringr::str_split(x, sep)
  header <- ifelse(header, x[[1]], NULL)
  x <- ifelse(header, x[-1], x)
  x <- Reduce(function(...) rbind(..., deparse.level = 0), x)
  x <- as.data.frame(x, stringsAsFactors = stringsAsFactors)
  if (!is.null(header)) names(x) <- header

  x[] <- lapply(x, function(col) {
    if (is.character(col)) Encoding(col) <- "UTF-8"
    return(col)
  })
  return(x)
}
