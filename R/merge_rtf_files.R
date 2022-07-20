#' Merge RTF files into one single RTF file 
#' 
#' @param path a vector of RTF file path
#' 
#' @export
merge_rtf_files <- function(path){
  
  rtf <- lapply(path, readLines)
  
  n <- length(rtf)
  start <- c(1, rep(2, n - 1))
  end <- vapply(rtf, length, numeric(1))
  end[-n] <- end[-n] - 1
  
  for(i in 1:n){
    rtf[[i]] <- rtf[[i]][start[i]:end[i]]
	    if(i < n) rtf[[i]] <- c(rtf[[i]], "{\\pard\\fs2\\par}\\page{\\pard\\fs2\\par}")
  }
  rtf <- do.call(c, rtf)
  
  rtf
  
}
