#ref: https://tomizonor.wordpress.com/2013/04/17/file-utf8-windows/
#ref: http://stackoverflow.com/questions/16028658/unicode-conversion-and-export-in-r


#========writeUTF8========#
writeUTF8 <- function(x, file, sep = "\t", bom = F) {
  con <- file(file, "wb") #Writing in binary mode

  x <- if (is.matrix(x) | is.data.frame(x)) rbind(colnames(x), x)
  apply(x, 1, function(row) {
    row <- paste0(paste0(row, collapse = sep), "\r\n")
    if (bom) writeBin(BOM, con, endian = "little")
    writeBin(charToRaw(row), con, endian = "little")
  })

  close(con) #Close connection
}


