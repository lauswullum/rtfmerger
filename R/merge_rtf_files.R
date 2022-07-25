#' Merge RTFs
#' 
#' Merges RTF files - rtf character vectors - that are read 
#' linewise using readLine.
#' 
#' @param path 
#'
#' @return Returns a merged RTF character vector
#' 
#' @export
#'
#' @examples
#' paths = c("path1/file1.rtf", "path2/file2.rtf")
#' mergedrtf = merge_rtf_files(paths)
#' writeLines(mergedrtf, "mergedfile1andfile2.rtf")
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
